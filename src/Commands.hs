{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Commands where

import Control.Applicative ((<|>), optional)
import Data.Attoparsec.ByteString.Char8 hiding ( match )
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Text.Read ( readMaybe )
import Data.Monoid (All(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.State.Strict
import Control.Monad ( forM_, filterM, forM, foldM )
import Control.Monad.IO.Class ( liftIO )
import Control.Exception ( bracket, bracketOnError, evaluate, try, SomeException, catch )
import Control.DeepSeq (force)
import Data.Char ( toUpper )
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import qualified Data.Vector.Unboxed as VU
import Data.List ( nub, sortOn, intercalate, isPrefixOf )
import Data.Ord (Down(..))
import Data.List.Split ( splitOn )
import Control.Lens (over)

import qualified Data.Text as T

import Data.SRTree
import Data.SRTree.Datasets
import Data.SRTree.Recursion
import Data.SRTree.Eval
import Data.SRTree.Internal (countNodes, convertProtectedOps)
import Data.SRTree.Print hiding ( printExpr )
import Text.ParseSR (SRAlgs(..), parseSR, Output(..), showOutput)
import System.Random

import Statistics.Distribution ( ContDistr(quantile) )
import Statistics.Distribution.FDistribution ( fDistribution )
import System.IO.Unsafe (unsafePerformIO)
import Algorithm.SRTree.Likelihoods
import Algorithm.SRTree.ConfidenceIntervals (CIType(..), PType(..), paramCI, getAllProfiles, getStatsFromModel, CI(..), BasicStats(..), getCol, approximateContour, ProfileT(..))
import Algorithm.SRTree.Compile (compileTree, EvalTree(..))
import Algorithm.SRTree.Utils (invChol, toRowMajor, fromRowMajor)

import Algorithm.EqSat
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat.Queries
import Algorithm.EqSat.DB
import Algorithm.EqSat.Simplify hiding (myCost)

import Algorithm.SRTree.ModelSelection
import Algorithm.EqSat.SearchSR hiding (fitnessFun, fitnessFunRep, fitnessMV)

import Data.Binary ( encode, decode )
import qualified Data.ByteString.Lazy as BS

import Database.SQLite3 ( Database, open, close )
import Database.PostgreSQL.LibPQ ( Connection, connectdb, finish )
import Algorithm.EqSat.Storage.SQLite ( saveGraph, loadGraphLazy, emptyPagedGraph, pushFit, refreshFitness, flushStore )
import Algorithm.EqSat.Storage.Postgres ()
import Algorithm.EqSat.Storage.Backend ( SqlBackend, queryDb, SqlValue(..), sqlToText, sqlToInt, sqlToMaybeDouble )
import Algorithm.EqSat.Storage.Import (importEqs, ImportSummary(..), recordExpressionIndex)
import Algorithm.EqSat.Storage.Stream (streamByOpCount, streamMatchNAry)
import Algorithm.EqSat.Storage.ClassStore (loadFrontierRows)
import Algorithm.EqSat.Storage.Extract (extractBestFromDB, readPage)
import Algorithm.EqSat.Storage.Types (parseTheta, serializeTheta)
import qualified Algorithm.EqSat.Storage.Query as Q

import Util
-- * Parsing

-- top 5 by fitness|mdl [less than 5 params, less than 10 nodes]
data Command  = Top Int Filter Criteria PatStr Bool
              | Distribution FilterDist (Maybe Limit) CriteriaDist Int Int
              | DistTokens Int
              | Modular Int FilterDist Criteria
              -- below these will not be a parsable command
              | Report EClassId ArgOpt Bool
              | Optimize EClassId Int ArgOpt Bool
              | Insert String ArgOpt
              | Subtrees EClassId
                | Pareto Criteria Bool
              | CountPat String
              | ExtractPat EClassId
              | Save String
              | Load String
              | Persist String String String
              | LoadDB String String String
              | DBTop String String String Int [String] Bool (Maybe String)
              | DBDist String String String Int
              | DBCount String String String
                | DBPareto String String String Bool Bool (Maybe String)
                  -- fname fitPath ds ci byFitness mData
                | PushFit String String String
                | RefreshFit String String String
                | DBEqSat String String String Int String
                | DBEqSatFrontier String String String Int String
                | DBInsert String String String String String
                | DBSetFit String String String Int Double
                | DBStream String String String Int
                | DBReport String String String String String Int Bool -- fname fitPath ds trainPath testPath eid ci
                | DBOptimize String String String String Int Bool String  -- fname fitPath ds dataPath eid ci lossName
                | DBSubtrees String String String Int             -- fname _fitPath ds eid
                | DBGetNExprs String String String Int Int        -- fname _fitPath ds n eid
                | DBGetNEclasses String String String Int Int     -- fname _fitPath ds n eid
                | DBEClassTerminals String String String Int      -- fname _fitPath ds eid
                | DBTopPattern String String String Int String Bool Bool (Maybe String)
                  -- fname fitPath ds n pattern isRoot negate ci
                | DBDistribution String String String Int   -- fname fitPath ds n (top-N bounded)
                | DBModularity String String String Int     -- fname fitPath ds n
                | DBCountPat String String String String Int -- fname fitPath ds pattern n
                | DBPatternMap String String String String Int -- fname fitPath ds pattern n
                | DBExtractPat String String String Int     -- fname fitPath ds eid
                | DBDistTokens String String String Int     -- fname fitPath ds n
                | DBProfileData String String String Int String  -- fname fitPath ds eid dataPath
               | ImportDB String String String Loss String Bool
               | Import String Loss String Bool
              | EqSatStep Int ArgOpt
              | GetNExprs Int EClassId
              | GetNEclass Int Int
              | PatternMap String (Maybe Int)
              | EClassTerminals Int

type Filter = EClass -> Bool -- pattern?
type FilterDist = Int -> Bool
data Criteria = ByFitness | ByDL deriving Eq
data CriteriaDist = ByCount | ByAvgFit
data Limit = Limit Int Bool deriving Show
data PatStr = PatStr String Bool | AntiPatStr String Bool | NoPat
type ArgOpt = (Loss, [DataSet], [DataSet])

-- top 10 with <=10|=10 size with <=4 parameters by fitness|dl matching pat
-- report id
-- optimize id
-- insert eq
-- subtrees id
-- distribution with size <=10 limited at 10 asc|dsc
--


parseCmd parser = eitherResult . (`feed` "") . parse parser . putEOL . B.strip

stripSp = many' (char ' ')

parseTop = do n <- decimal
              stripSp
              filters  <- many' parseFilter
              stripSp
              criteria <- fromMaybe ByFitness . listToMaybe <$> many' parseCriteria
              stripSp
              pats' <- many' (parsePattern <|> parseAnti)
              pats <- case pats' of
                        [] -> pure $ NoPat
                        (x:_) -> pure $ x
              stripSp
              ci <- parseWithCI
              pure $ Top n (getAll . mconcat filters) criteria pats ci

parseWithCI = (string "with" >> stripSp >> string "ci" >> pure True) <|> pure False

parseDist = do filters' <- many' parseFilterDist
               let filters = if null filters'
                               then [(\pat -> All $ pat <= 10)]
                               else filters'
               stripSp
               limit   <- listToMaybe <$> many' parseLimit
               stripSp
               by'     <- listToMaybe <$> many' parseCriteriaDL
               stripSp 
               least' <- listToMaybe <$> many' parseLeast
               stripSp 
               top'   <- listToMaybe <$> many' parseTopDist
               let by = case by' of 
                          Nothing -> ByCount
                          Just b  -> b
                   least = case least' of 
                             Nothing -> 1 
                             Just l  -> l 
                   top   = case top' of 
                             Nothing -> 1000
                             Just t  -> t
               pure $ Distribution (getAll . mconcat filters) limit by least top 

parseModular = do n <- decimal
                  stripSp
                  filters' <- many' parseFilterDist
                  let filters = if null filters'
                                  then [(\sz -> All $ sz <= 10)]
                                  else filters'
                  stripSp
                  by'     <- fromMaybe ByFitness . listToMaybe <$> many' parseCriteria
                  pure $ Modular n (getAll . mconcat filters) by'

parsePatternMap = do raw <- B.unpack <$> takeWhile1 (/= '\n')
                     let (patPart, limitPart) = splitLimitedAt raw
                     let limit = case reads (strip limitPart) of
                                   [(n, _)] -> Just (n :: Int)
                                   _        -> Nothing
                     pure $ PatternMap (strip patPart) limit
  where
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    splitLimitedAt [] = ([], "")
    splitLimitedAt s@(c:cs)
      | map toUpper (Prelude.take 10 s) == "LIMITED AT" = ([], drop 10 s)
      | otherwise = let (p, r) = splitLimitedAt cs in (c : p, r)

parseEClassTerminals = EClassTerminals <$> decimal

parseLeast = stringCI "with at least " >> decimal 
parseTopDist = stringCI "from top " >> decimal 

-- * SQLite-backed commands (srtree-db)
-- filename must not contain whitespace
parseFname = takeWhile1 (\c -> c /= ' ' && c /= '\n')

-- | Parse a possibly colon-separated "egraph_path:fit_path" pair.
-- If there is no colon, both paths are set to the same value (backward compat).
parseSplitFname :: Parser (String, String)
parseSplitFname = do
  raw <- B.unpack <$> parseFname
  case break (== ':') raw of
    (path, ':':fitPath) -> do
      pure (path, fitPath)
    _                   -> pure (raw, raw)

parsePersist = string "persist " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  pure (Persist fname fitPath ds)
parseLoadDB = string "load " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  pure (LoadDB fname fitPath ds)
parseDBTop   = string "top " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  stripSp
  ci <- parseWithCI
  mData <- if ci
           then do stripSp
                   optional (string "data" >> stripSp >> fmap B.unpack parseFname)
           else pure Nothing
  pure (DBTop fname fitPath ds n ["x"] ci mData)
parseDBDist  = string "distribution " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  pure (DBDist fname fitPath ds n)
parseDBCount = string "count " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  op <- B.unpack <$> parseFname
  pure (DBCount fname fitPath op)
parseDBPareto = string "pareto " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  ci <- parseWithCI
  mData <- if ci
           then do stripSp
                   optional (string "data" >> stripSp >> fmap B.unpack parseFname)
           else pure Nothing
  stripSp
  byFitness <- option True (do string "by dl"; pure False)
  pure (DBPareto fname fitPath ds ci byFitness mData)
parsePushFit = string "push-fit " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  pure (PushFit fname fitPath ds)
parseRefreshFit = string "refresh-fitness " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  pure (RefreshFit fname fitPath ds)
parseDBEqSat = string "eqsat " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  stripSp
  rs <- option "" (B.unpack <$> parseFname)
  pure (DBEqSat fname fitPath ds n rs)
parseDBEqSatFrontier = string "eqsat-frontier " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  stripSp
  rs <- option "" (B.unpack <$> parseFname)
  pure (DBEqSatFrontier fname fitPath ds n rs)
parseDBInsert = string "insert " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  alg <- B.unpack <$> parseFname
  stripSp
  expr <- B.unpack . B.pack <$> manyTill anyChar endOfInput
  pure (DBInsert fname fitPath ds alg expr)
parseDBSetFit = string "set-fit " >>= \_ -> do
  (fname, fitPath) <- parseSplitFname
  stripSp
  ds <- B.unpack <$> parseFname
  stripSp
  eid <- decimal
  stripSp
  fit <- double
  pure (DBSetFit fname fitPath ds eid fit)
parseCriteriaDL = (stringCI "by count" >> pure ByCount)
              <|> (stringCI "by fitness" >> pure ByAvgFit)

parseFilter = do stringCI "with"
                 stripSp
                 field <- parseSz <|> parseCost <|> parseParams
                 stripSp
                 cmp <- parseCmp
                 stripSp
                 pure (\ec -> All $ cmp (field ec))
parseFilterDist = do stringCI "with"
                     stripSp
                     stringCI "size"
                     stripSp
                     cmp <- parseCmp
                     stripSp
                     pure (\pat -> All $ cmp pat)

parseSz = stringCI "size" >> pure (_size . _info)
parseCost = stringCI "cost" >> pure (_cost . _info)
parseParams = stringCI "parameters" >> pure (mbLen . _theta . _info)
   where
      mbLen [] = 0
      mbLen ps = VU.length $ Prelude.head ps
parseCmp = do op <- parseLEQ <|> parseLT <|> parseEQ <|> parseGEQ <|> parseGT
              stripSp
              n <- decimal
              pure (`op` n)

parseLT  = string "<"  >> pure (<)
parseLEQ = string "<=" >> pure (<=)
parseEQ  = string "="  >> pure (==)
parseGEQ = string ">="  >> pure (>=)
parseGT  = string ">" >> pure (>)

parsePattern = do stringCI "matching"
                  stripSp
                  b <- option True parseRoot
                  pat <- many' anyChar
                  pure $ PatStr pat b
parseAnti = do stringCI "not matching"
               stripSp
               b <- option True parseRoot
               pat <- many' anyChar
               pure $ AntiPatStr pat b

parseLimit = do stringCI "limited at"
                stripSp
                n <- decimal
                stripSp
                ascOrdsc <- stringCI "asc" <|> stringCI "dsc"
                pure $ Limit n (ascOrdsc == "asc")
parseRoot = do stringCI "root"
               stripSp
               pure False
parseCriteria = parseByFit <|> parseByDL
parseByFit = do stringCI "by fitness"
                pure ByFitness
parseByDL  = do stringCI "by dl"
                pure ByDL

putEOL :: B.ByteString -> B.ByteString
putEOL bs | B.null bs = bs
          | B.last bs == '\n' = bs
          | otherwise         = B.snoc bs '\n'

-- * Pattern parser (previously exported by Text.ParseSR)

type ParsePat = Parser Pattern

-- | parses a string representing a pattern expression (e.g. @v0 * x0 + t0@).
parsePat :: B.ByteString -> Either String Pattern
parsePat = eitherResult . (`feed` "") . parse parsePatExpr . putEOL . B.strip

parsePatExpr :: ParsePat
parsePatExpr = parsePatternExpr (prefixOps : binOps) [] var
  where
    prefixOps = Prelude.map (uncurry prefix)
                [ ("id", id), ("abs", abs)
                  , ("sinh", sinh), ("cosh", cosh), ("tanh", tanh)
                  , ("sin", sin), ("cos", cos), ("tan", tan)
                  , ("asinh", asinh), ("acosh", acosh), ("atanh", atanh)
                  , ("asin", asin), ("acos", acos), ("atan", atan)
                  , ("sqrtabs", sqrtabs'), ("sqrt", sqrt), ("cbrt", cbrt'), ("square", (**2))
                  , ("logabs", logabs'), ("log", log), ("exp", exp), ("cube", cube'), ("recip", recip')
                  , ("Id", id), ("Abs", abs)
                  , ("Sinh", sinh), ("Cosh", cosh), ("Tanh", tanh)
                  , ("Sin", sin), ("Cos", cos), ("Tan", tan)
                  , ("ASinh", asinh), ("ACosh", acosh), ("ATanh", atanh)
                  , ("ASin", asin), ("ACos", acos), ("ATan", atan)
                  , ("SqrtAbs", sqrtabs'), ("Sqrt", sqrt), ("Cbrt", cbrt'), ("Square", (**2))
                  , ("LogAbs", logabs'), ("Log", log), ("Exp", exp), ("Recip", recip'), ("Cube", cube')
                  , ("|log|", logabs'), ("|Log|", logabs'), ("|sqrt|", sqrtabs'), ("|Sqrt|", sqrtabs')
                  , ("√", sqrt), ("|√|", sqrtabs')
                ]
    binOps = [[binary "^" (**) AssocLeft], [binary "**" (**) AssocLeft]
            , [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft]
            , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
            , [binary "|**|" powabs AssocLeft], [binary "|^|" powabs AssocLeft]
            , [binary "aq" aq AssocLeft], [binary "|/|" aq AssocLeft]
            ]
    powabs l r  = Fixed $ Bin PowerAbs l r
    aq l r      = Fixed $ Bin AQ l r
    logabs' t   = Fixed $ Uni LogAbs t
    sqrtabs' t  = Fixed $ Uni SqrtAbs t
    cbrt' t     = Fixed $ Uni Cbrt t
    cube' t     = Fixed $ Uni Cube t
    recip' t    = Fixed $ Uni Recip t

    var = do char 'x'
             ix <- decimal
             pure $ Fixed $ Var ix
          <|> do char 't'
                 ix <- decimal
                 pure $ Fixed $ Param ix
          <|> do char 'v'
                 ix <- decimal
                 pure $ VarPat (toEnum $ ix+65)
          <?> "var"

-- | Creates a parser for a binary operator
binary :: B.ByteString -> (a -> a -> a) -> Assoc -> Operator B.ByteString a
binary name fun  = Infix (do{ string (B.cons ' ' (B.snoc name ' ')) <|> string name; pure fun })

-- | Creates a parser for a unary function
prefix :: B.ByteString -> (a -> a) -> Operator B.ByteString a
prefix  name fun = Prefix (do{ string name; pure fun })

-- | Envelopes the parser in parens
parens :: Parser a -> Parser a
parens e = do{ string "("; e' <- e; string ")"; pure e' } <?> "parens"

parsePatternExpr :: [[Operator B.ByteString Pattern]] -> [ParsePat -> ParsePat] -> ParsePat -> ParsePat
parsePatternExpr table binFuns var =
    do e <- expr
       many1' space
       pure e
  where
    term  = parens expr <|> choice (Prelude.map ($ expr) binFuns) <|> coef <|> var <?> "term"
    expr  = buildExpressionParser table term
    coef  = Fixed . Const <$> signed double <?> "const"

data PrintResults = MultiExprs [(EClassId, IntMap.IntMap (Int, Int))] | SingleExpr EClassId | Counts [(Pattern, (Int, Double))] | SimpleStr String | MultiTrees [Fix SRTree] | MultiClass [[EClassId]] | NoPrint
                  -- deriving (Show)

-- running
run :: Command -> MyEGraph PrintResults

-- | Grid-scan profile for a single parameter.
-- For each grid point, fixes the parameter and re-optimizes nuisance params.
-- Returns (taus, thetas_cols, optTh) where thetas_cols is a list of columns.
-- | Helper: convert a ProfileT to CSV rows
-- _thetas is stored as rows: each row is a full theta vector at one profile point.
-- We output one CSV row per profile point p, with columns for each parameter c.
profileToCSV :: Int -> (Int, ProfileT) -> [String]
profileToCSV k (paramIdx, prof) =
  let taus' = _taus prof
      thetas' = _thetas prof
      optVal = _opt prof
      nPts = VU.length taus'
      nThetas = length thetas'
  in [ show paramIdx <> ","
       <> show (taus' VU.! p) <> ","
       <> intercalate "," [ if p < nThetas then show ((thetas' !! p) VU.! c) else show optVal | c <- [0..k-1] ]
       <> "," <> show optVal
     | p <- [0..nPts-1] ]

run (Top n filters criteria NoPat ci) = do
   let getFun = if criteria == ByFitness then getTopFitEClassThat else getTopDLEClassThat
   ids <- getFun n filters
   pure $ MultiExprs $ [(i, IntMap.empty) | i <- reverse ids]

run (Top n filters criteria withPat ci) = do
   let (pat', getFun, isParents) =
          case withPat of
            PatStr p parent     -> (p, if criteria == ByFitness then getTopFitEClassIn else getTopDLEClassIn, parent)
            AntiPatStr p parent -> (p, if criteria == ByFitness then getTopFitEClassNotIn else getTopDLEClassNotIn, parent)

   let etree = parsePat $ B.pack pat'
   case etree of
     Left _ -> pure . SimpleStr $ "no parse for " <> pat'
     Right pat -> do
        ecs' <- (Prelude.map fromLeft . Prelude.filter isLeft . Prelude.map snd) <$> match pat

        ecs  <- Prelude.mapM canonical ecs'
                          >>= removeNotTrivial (lenPat pat)
                          >>= getParents isParents filters
        let ecsSet = IntSet.fromList ecs
            -- ecsSet' = IntSet.fromList ecs'
            -- allSet = ecsSet -- <> ecsSet'
        ids  <- getFun n filters ecs -- (IntSet.toList ecsSet) -- (nub $ ecs <> ecs')
        pure . MultiExprs $ [(i, IntMap.empty) | i <- reverse (nub ids)]
        -- printSimpleMultiExprs isCLI (reverse $ nub ids)

run (Distribution pSz mLimit by least top) = do
  ee <- IntSet.toList . IntSet.fromList <$> getTopFitEClassThat top (const True) -- getAllEvaluatedEClasses
  allPats <- getAllPatternsFrom pSz Map.empty ee
  let (n, isAsc) = case mLimit of
                     Nothing -> (Map.size allPats, True)
                     Just (Limit sz asc) -> (sz, asc)
      predCount = (if isAsc then fst else negate . fst) . snd
      predAvgFit = (if isAsc then snd else negate . snd) . snd
  pure . Counts $ (Prelude.take n
                   $ case by of 
                       ByCount -> sortOn predCount
                       ByAvgFit -> sortOn predAvgFit
                   $ Map.toList
                   $ Map.filterWithKey (\k (v,_) -> v >= least && k /= VarPat 'A' && pSz (lenPat k))
                   allPats)
                       {-
  printMultiCounts isCLI (Prelude.take n
                   $ case by of 
                       ByCount -> sortOn predCount
                       ByAvgFit -> sortOn predAvgFit
                   $ Map.toList
                   $ Map.filterWithKey (\k (v,_) -> v >= least && k /= VarPat 'A' && pSz (lenPat k))
                   allPats)
                   -}

run (Modular n pSz criteria) = do
  let getFun = if criteria == ByFitness then getTopFitEClassIn else getTopDLEClassIn
  evaluated <- getAllEvaluatedEClasses
  ecm <- forM evaluated $ \ec -> do m <- mapOfNames pSz <$> extractEClassList ec
                                    pure (ec, m)
  ids'  <- reverse . nub <$> (getFun n (const True)
        $ Prelude.map fst
        $ Prelude.filter (\(ec, m) -> not $ IntMap.null m) ecm)
  ids <- mapM canonical ids'
  let myM = IntMap.fromList ecm

  pure . MultiExprs $ [(myId, myM IntMap.! myId) | myId <- ids]


run (Report eid (dist, trainData, testData) ci) = do
  eid' <- canonical eid
  pure . SingleExpr $ eid'

run (Optimize eid nIters (dist, trainDatas, testData) ci) = do
   t <- relabelParams <$> getBestExpr eid
   --(f, thetas) <- fitnessMV False 1 nIters dist (Prelude.zip trainDatas testData) t
   let dataTrainsVals = Prelude.zip trainDatas testData
   response <- forM dataTrainsVals $ \(dt, dv) -> fitnessFunRep nIters dist dt t
   let f = Prelude.minimum (Prelude.map fst response)
       thetas = Prelude.map snd response
   insertFitness eid f thetas
   let mdl_train  = Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdlMetric dist mYErr x y theta t) $ Prelude.zip thetas trainDatas
   insertDL eid mdl_train
   pure . MultiExprs $ [(eid, IntMap.empty)]
   --printSimpleMultiExprs isCLI [eid]

run (Insert expr argOpt) = do
  let etree = parseSR TIR "" False $ B.pack expr
  case etree of
    Left _     -> pure . SimpleStr $ "no parse for " <> expr
    Right tree -> do eid <- fromTree myCost tree
                     run (Optimize eid 100 argOpt False)

run (Subtrees eid') = do
   eid <- canonical eid'
   isValid <- gets ((IntMap.member eid) . _eClass)
   if isValid
     then do ids <- getAllChildBestEClassesRep eid
             pure . MultiExprs $ [(i, IntMap.empty) | i <- ids]
             --printSimpleMultiExprs isCLI ids
     else pure . SimpleStr $ "Invalid id."

run (Pareto crit ci) = do
   maxSize <- gets (fst . IntMap.findMax . _sizeFitDB . _eDB)
   ecs <- case crit of
            ByFitness -> getParetoEcsUpTo 1 maxSize
            ByDL      -> getParetoDLEcsUpTo 1 maxSize
   pure . MultiExprs $ [(i, IntMap.empty) | i <- ecs]

run (CountPat spat) = do
  let etree = parsePat $ B.pack spat
  case etree of
    Left _     -> pure . SimpleStr $ "no parse for " <> spat
    Right pat  -> do (p, cnt) <- countPattern pat
                     pure . SimpleStr $ spat <> " appears in " <> show cnt <> " equations."
                     --if isCLI
                     --   then do io $ putStrLn $ spat <> " appears in " <> show cnt <> " equations."
                     --           pure ""
                     --   else pure $ spat <> " appears in " <> show cnt <> " equations."

run (Save fname) = do
  eg <- get
  lift $ BS.writeFile fname (encode eg)
  pure NoPrint

run (Load fname) = do
  eg <- lift $ BS.readFile fname
  put (decode eg)
  pure NoPrint

-- * SQLite-backed commands

run (Persist fname fitPath ds) = do
  eg <- get
  r  <- liftIO $ withBackend fname $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         saveGraph db dsid eg
  pure . SimpleStr $ case r of
    Left err -> "persist failed: " <> err
    Right () -> "e-graph persisted to " <> fname

run (LoadDB fname fitPath ds) = do
  -- NB: keep the DB connection alive.  loadGraphLazy builds a paged
  -- e-graph whose 'EClassPageStore' references this very connection, so closing
  -- it (as 'withBackend' does) would leave the graph pointing at a dead
  -- connection and every later page read (e.g. recalculateBestAllStream ->
  -- allKeys -> allPages, or flushGraphStore) would crash with SQLITE_MISUSE.
  -- bracketOnError closes only on failure; on success the connection is owned
  -- by the e-graph (via '_classStore') and is finalised when the graph is
  -- replaced/GC'd.
  r <- liftIO $ withBackendKeepOpen fname $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         loadGraphLazy db dsid 50000 100000 100000
  case r of
    Left err -> pure . SimpleStr $ "db-load failed: " <> err
    Right eg -> do
      put eg
      -- best/cost are not persisted in the db: recompute the
      -- cost-minimal best for every e-class (the database holds
      -- arbitrary nodes otherwise, which can explode when
      -- expanded for printing/pattern matching). This streams each
      -- class through the paged store and stays memory-bounded.
      recalculateBestAllStream myCost
      flushGraphStore
      pure (SimpleStr ("e-graph loaded from " <> fname))

run (DBTop fname fitPath ds n varnames ci mData) = do

  -- Get dataset ID and top-N from fit DB
  (dsid, top) <- liftIO $ withBackend fitPath $ \fitDb -> do
         dsid <- Q.getOrCreateDataset fitDb ds
         top  <- Q.topN fitDb dsid n
         pure (dsid, top)

  -- Extract expression trees from egraph DB (needs cstore_page)
  results <- liftIO $ withBackend fname $ \egDb ->
    forM top $ \(eid, fit) -> do
      mTree <- extractBestFromDB egDb eid
      pure (eid, fit, mTree)

  -- For CI, read theta from fit DB
  thetaMap <- if ci && not (null top)
    then liftIO $ withBackend fitPath $ \fitDb -> do
      let eids = map (\(eid,_,_) -> eid) results
          inClause = "(" <> T.pack (intercalate "," (map show eids)) <> ")"
      thRows <- queryDb fitDb
        ("SELECT eid, theta FROM dataset_fit WHERE dataset_id = ? AND eid IN " <> inClause)
        [SqlInteger (fromIntegral dsid)]
      pure [ (sqlToInt e, sqlToText t) | [e, t] <- thRows ]
    else pure []

  -- Load dataset for CI computation if requested
  mDataLoaded <- case (ci, mData) of
    (True, Just dataPath) -> do
      ((xTr, yTr, _, _), (mYErr, _), _, _) <- liftIO $ loadDataset dataPath True
      pure $ Just (xTr, yTr, mYErr)
    _ -> pure Nothing

  rows <- forM results $ \(eid, fit, mTree) ->
    case mTree of
      Nothing -> pure $ show eid <> ",<extraction failed>," <> show fit
      Just tree -> do
        let t = showExpr tree
            base = intercalate "," [show eid, t, show fit]
        case (ci, mDataLoaded) of
          (True, Just (xTr, yTr, mYErr)) -> do
            let thetaText = lookup eid thetaMap
                theta = case thetaText of
                  Nothing -> VU.empty
                  Just th -> case parseTheta (T.unpack th) of
                    []    -> VU.empty
                    (v:_) -> v
                dist = Gaussian
                nSamples = VU.length yTr
                et = compileTree dist xTr yTr mYErr tree
                stats = getStatsFromModel dist mYErr xTr yTr tree theta
            profiles <- liftIO $ getAllProfiles Bates et theta (_stdErr stats) [] 0.05
            let ciVals = paramCI (Profile stats profiles) nSamples theta 0.05
                maxP = VU.length theta
                ciStr = intercalate ","
                      $ Prelude.map (\(CI _ l h) -> show l <> "," <> show h) ciVals
                      ++ Prelude.replicate (2 * (maxP - length ciVals)) ""
            pure $ base <> "," <> ciStr
          _ -> pure base
  pure . SimpleStr $ intercalate "\n" ("Id,Expression,Fitness" : rows)
run (DBDist fname fitPath ds n) = do
  r <- liftIO $ withBackend fitPath $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         Q.distributionCounts db dsid n
  pure . SimpleStr . intercalate "\n" $ ("Size,Count" : [show s <> "," <> show c | (s, c) <- r])

run (DBCount fname fitPath op) = do
  c <- liftIO $ withBackend fname $ \db -> Q.countPattern db (T.pack op)
  pure . SimpleStr $ "e-classes containing " <> op <> ": " <> show c

run (DBPareto fname fitPath ds ci byFitness mData) = do
  r <- liftIO $ withBackend fitPath $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         if byFitness
           then do pts <- Q.paretoBySize db dsid
                   pure [(eid, f, fromIntegral s) | (eid, f, s) <- pts]
           else Q.pareto db dsid

  -- Load dataset for CI computation if requested
  mDataLoaded <- case (ci, mData) of
    (True, Just dataPath) -> do
      ((xTr, yTr, _, _), (mYErr, _), _, _) <- liftIO $ loadDataset dataPath True
      pure $ Just (xTr, yTr, mYErr)
    _ -> pure Nothing

  -- Load graph for expression extraction if CI requested
  egForCI <- case (ci, mDataLoaded) of
    (True, Just _) -> do
      er <- liftIO $ withBackendKeepOpen fname $ \db -> do
             dsid <- Q.getOrCreateDataset db ds
             loadGraphLazy db dsid 50000 100000 100000
      case er of
        Left _ -> pure Nothing
        Right eg' -> do
          put eg'
          -- Same as DBTop: skip recalculateBestAllStream; pages already
          -- carry correct _cost/_best from a prior eqsat or import.
          pure $ Just eg'
    _ -> pure Nothing

  case (ci, mDataLoaded, egForCI) of
    (True, Just (xTr, yTr, mYErr), Just _) -> do
      rows <- forM r $ \(eid, f, s) -> do
                tree <- getBestExpr eid
                thetas <- getTheta eid
                let theta = if null thetas then VU.empty else head thetas
                    dist = Gaussian
                    nSamples = VU.length yTr
                    et = compileTree dist xTr yTr mYErr tree
                    stats = getStatsFromModel dist mYErr xTr yTr tree theta
                profiles <- liftIO $ getAllProfiles Bates et theta (_stdErr stats) [] 0.05
                let ciVals = paramCI (Profile stats profiles) nSamples theta 0.05
                    maxP = VU.length theta
                    ciStr = intercalate ","
                          $ Prelude.map (\(CI _ l h) -> show l <> "," <> show h) ciVals
                          ++ Prelude.replicate (2 * (maxP - length ciVals)) ""
                pure $ show eid <> "," <> show f <> "," <> show s <> "," <> ciStr
      let header = if byFitness then "Id,Fitness,Size" else "Id,Fitness,DL"
      pure . SimpleStr . intercalate "\n" $ (header : rows)
    _ -> do
      let header = if byFitness then "Id,Fitness,Size" else "Id,Fitness,DL"
      pure . SimpleStr . intercalate "\n" $ (header : [show eid <> "," <> show f <> "," <> show s | (eid, f, s) <- r])

run (PushFit fname fitPath ds) = do
  eg <- get
  liftIO $ withBackend fname $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         pushFit db dsid eg
  pure . SimpleStr $ "dataset_fit table written to " <> fname

run (RefreshFit fname fitPath ds) = do
  eg <- get
  r  <- liftIO $ withBackend fname $ \db -> do
         dsid <- Q.getOrCreateDataset db ds
         refreshFitness db dsid eg
  case r of
    Left err -> pure . SimpleStr $ "db-refresh-fitness failed: " <> err
    Right eg' -> do put eg'
                    rebuildAllRanges
                    pure (SimpleStr ("fitness refreshed from " <> fname))

-- | Run equality saturation entirely against a lazily loaded (out-of-core) graph.
-- The e-graph stays paged: only the structural indexes are resident, every
-- e-class body is streamed through the store, and class bodies never all live in
-- memory at once. The rewritten graph is written back with 'saveGraph'.
run (DBEqSat fname fitPath ds iters rs) = do
  let rules = case rs of
               "params" -> rewritesParams
               _        -> rewrites
  r <- liftIO $ withBackend fname $ \db -> do
        dsid <- Q.getOrCreateDataset db ds
        er <- loadGraphLazy db dsid 50000 100000 100000
        case er of
          Left err -> pure (Left err)
          Right eg -> do
            -- cost/best are not persisted: recompute them first so rewrites
            -- operate on valid per-class data (imported DBs carry defaults).
            let go g = execStateT (runEqSat myCost rules iters) g
            eg' <- go eg
            flushStore eg'
            saveGraph db dsid eg'
            -- a full pass re-saturated everything: clear any frontier marks
            case _classStore eg' of
              Nothing -> pure ()
              Just h  -> liftIO (cpsEndFrontier h)
            pure (Right ())
  pure . SimpleStr $ case r of
    Left err -> "db-eqsat failed: " <> err
    Right () -> "db-eqsat applied " <> show iters <> " iteration(s) of '"
                  <> (if null rs then "default" else rs) <> "' on " <> fname

-- | Re-saturate only the frontier: e-classes that have been created or merged
-- since the last re-saturation pass (tracked by the write-through into the
-- @frontier@ table). The matcher's candidate roots are restricted to the
-- frontier, so unchanged parts of the graph are not re-worked. Clears the
-- frontier afterwards. O(1) memory, paged. The pure in-memory eggp loop and a
-- full 'dbEqSat' are unaffected.
run (DBEqSatFrontier fname fitPath ds iters rs) = do
  let rules = case rs of
               "params" -> rewritesParams
               _        -> rewrites
  r <- liftIO $ withBackend fname $ \db -> do
        dsid <- Q.getOrCreateDataset db ds
        er <- loadGraphLazy db dsid 50000 100000 100000
        case er of
          Left err -> pure (Left err)
          Right eg -> case _classStore eg of
            Nothing -> pure (Left "db-eqsat-frontier requires a paged graph")
            Just h  -> do
              n0 <- length <$> loadFrontierRows db
              liftIO (cpsBeginFrontier h)
              eg' <- execStateT (runEqSat myCost rules iters) eg
              liftIO (cpsEndFrontier h)
              flushStore eg'
              saveGraph db dsid eg'
              pure (Right n0)
  pure . SimpleStr $ case r of
    Left err -> "db-eqsat-frontier failed: " <> err
    Right n0 -> "db-eqsat-frontier re-saturated " <> show n0
                  <> " frontier class(es) in " <> show iters
                  <> " iteration(s) of '"
                  <> (if null rs then "default" else rs) <> "' on " <> fname

-- | Local eggp delta: insert a single expression into the DB-backed (paged)
-- graph. Its subgraph is written through (content-addressed: existing
-- subexpressions dedup against the live tables) and every genuinely-new class is
-- marked as part of the re-saturation frontier, so a later 'dbEqSatFrontier'
-- re-saturates only what changed. The root e-class id is returned (symmetric
-- with the in-memory 'insert') so the eggp loop can track / evaluate it, and the
-- expression is recorded in @expression_index@ (item: "was this seen?").
-- O(subgraph) work, O(1) memory.
run (DBInsert fname fitPath ds alg expr) = do
  let etree = parseSR (algToAlgs alg) "" False (B.pack expr)
  r <- liftIO $ case etree of
    Left _     -> pure (Left "no parse")
    Right tree -> do
      dsid <- withBackend fitPath $ \fitDb -> Q.getOrCreateDataset fitDb ds
      withBackend fname $ \db -> do
        er <- loadGraphLazy db dsid 50000 100000 100000
        -- no e-graph stored yet: seed an empty paged graph so the first insert
        -- works (subsequent inserts load the now-existing graph).
        eg0 <- case er of
          Left _  -> emptyPagedGraph db 50000 100000 100000
          Right eg -> pure eg
        case _classStore eg0 of
          Nothing -> pure (Left "db-insert requires a paged graph")
          Just _  -> do
            (eid, eg') <- runStateT (fromTree myCost tree) eg0
            flushStore eg'
            withBackend fitPath $ \fitDb -> recordExpressionIndex fitDb dsid eid
            saveGraph db dsid eg'
            pure (Right eid)
  pure . SimpleStr $ case r of
    Left err  -> "db-insert failed: " <> err
    Right eid -> show eid

-- | Set the fitness of a single e-class in @dataset_fit@, so a newly-inserted
-- DB expression (see 'DBInsert') can be ranked by the query layer once the eggp
-- loop has evaluated it. Writes only that class's row (structure untouched).
run (DBSetFit fname fitPath ds eid fit) =
  liftIO $ withBackend fitPath $ \db -> do
    dsid <- Q.getOrCreateDataset db ds
    Q.writeDatasetFit db dsid eid (Just fit) Nothing "" 0
    pure (SimpleStr ("db-set-fit " <> show eid <> " <- " <> show fit))

run (Import fname dist varnames params) = do
  importCSV dist fname varnames params
  pure NoPrint

-- | PoC: stream the @enode@ table by operator through a SQLite cursor and
-- report the total count and the first @budget@ matched e-classes. Used to
-- measure that streaming matching is O(1) memory vs the in-RAM pattern trie.
run (DBStream fname fitPath op budget) = do
  r <- liftIO $ do
    db <- open (T.pack fname)
    n  <- streamByOpCount db (T.pack op)
    ms <- streamMatchNAry db (T.pack op) budget
    close db
    pure (n, ms)
  pure . SimpleStr $ "count=" <> show (fst r) <> " matches=" <> show (length (snd r))

-- | DB-native report: extract the best expression from the DB for a single
-- e-class, show its canonical ID, expression, and fitness. No in-memory graph.
run (DBReport fname fitPath ds trainPath testPath eid ci) = do
  -- canonical id
  canonical <- liftIO $ withBackend fname $ \db -> do
    rows <- queryDb db "SELECT canonical FROM eclass WHERE eid = ?"
                      [SqlInteger (fromIntegral eid)]
    case rows of
      [[SqlInteger c]] -> pure (fromIntegral c :: Int)
      _                -> pure eid

  -- extract expression from DB
  mTree <- liftIO $ withBackend fname $ \db -> extractBestFromDB db eid

  -- fitness + theta from the dataset's fit table
  (mFit, thetaText) <- liftIO $ withBackend fitPath $ \fitDb -> do
    dsid <- Q.getOrCreateDataset fitDb ds
    rows <- queryDb fitDb
      "SELECT fitness, dl, size, theta FROM dataset_fit WHERE dataset_id = ? AND eid = ?"
      [SqlInteger (fromIntegral dsid), SqlInteger (fromIntegral eid)]
    case rows of
      [[f, d, s, th]] -> pure (Just (sqlToMaybeDouble f, sqlToMaybeDouble d, sqlToInt s), sqlToText th)
      _                -> pure (Nothing, "")

  -- load the training dataset data (from its file path) for the metrics and CI
  mData <- liftIO $ do
    datasets <- Prelude.mapM (flip loadDataset True) (words trainPath)
    case datasets of
      (((xTr, yTr, _, _), (mYErr, _), _, _) : _) -> pure $ Just (xTr, yTr, mYErr)
      _ -> pure Nothing

  -- load the test dataset (if provided) for the Test column
  mTestData <- liftIO $ if testPath == "-" || null testPath
    then pure Nothing
    else do
      datasets <- Prelude.mapM (flip loadDataset True) (words testPath)
      case datasets of
        (((xTe, yTe, _, _), (mYErrTe, _), _, _) : _) -> pure $ Just (xTe, yTe, mYErrTe)
        _ -> pure Nothing

  let dist = Gaussian
      loss = NLL Gaussian
      theta = case parseTheta (T.unpack thetaText) of [] -> VU.empty; (v:_) -> v
      thetaStr = intercalate ", " (Prelude.map show (VU.toList theta))
      treeStr  = maybe "<extraction failed>" showExpr mTree
      numNodes = maybe 0 (countNodes . convertProtectedOps) mTree
      py       = maybe "NA" showPython mTree
      fitStr   = case mFit of
        Nothing -> "NA"
        Just (f, _, _) -> maybe "NA" show f
      metsOf mData' = case (mTree, mData') of
        (Just tr, Just (x, y, e)) ->
          let t = relabelParams tr
          in ( mseMetric x y t theta
             , r2Metric x y t theta
             , nllMetric loss e x y t theta
             , mdlMetric loss e x y theta t )
        _ -> (0, 0, 0, 0)
      (mseTr, r2Tr, nllTr, mdlTr) = metsOf mData
      (mseTe, r2Te, nllTe, mdlTe) = metsOf mTestData
      fmt d = if testPath == "-" || null testPath then "" else show d
      mainRows =
        "Info,Training,Test\n"
        <> "Id," <> show canonical <> ",\n"
        <> "Expr," <> treeStr <> ",\n"
        <> "Numpy,\"" <> py <> "\",\n"
        <> "Nodes," <> show numNodes <> ",\n"
        <> "params," <> thetaStr <> ",\n"
        <> "Fitness," <> fitStr <> ",\n"
        <> "MSE," <> show mseTr <> "," <> fmt mseTe <> "\n"
        <> "R^2," <> show r2Tr <> "," <> fmt r2Te <> "\n"
        <> "nll," <> show nllTr <> "," <> fmt nllTe <> "\n"
        <> "DL," <> show mdlTr <> "," <> fmt mdlTe <> "\n"

  -- CI (profile likelihood) rows appended when requested
  ciStr <- case (ci, mTree, mData) of
    (True, Just tr, Just (xTr, yTr, mYErr)) -> do
      let t  = relabelParams tr
          et = compileTree dist xTr yTr mYErr t
          stats = getStatsFromModel dist mYErr xTr yTr t theta
      profiles <- liftIO $ getAllProfiles Bates et theta (_stdErr stats) [] 0.05
      let ciVals = paramCI (Profile stats profiles) (VU.length yTr) theta 0.05
          rows = [ "ci_param_lower_" <> show i <> "," <> show l <> ","
                 | (i, CI _ l _) <- zip [0..] ciVals ]
              <> [ "ci_param_upper_" <> show i <> "," <> show u <> ","
                 | (i, CI _ _ u) <- zip [0..] ciVals ]
      pure (if null rows then "" else "\n" <> intercalate "\n" rows)
    _ -> pure ""

  pure . SimpleStr $ mainRows <> ciStr

-- | DB-native profile data: compute profile likelihood for each parameter
-- and return the raw spline data (taus, thetas) plus pairwise contours.
-- Output format: two CSV sections separated by a blank line.
-- Section 1 (profile data): param,tau,theta_0,...,theta_k,opt
-- Section 2 (contour data): i,j,theta_i,theta_j
run (DBProfileData fname fitPath ds eid dataPath) = do
  -- Extract expression from DB
  mTree <- liftIO $ withBackend fname $ \db -> extractBestFromDB db eid

  case mTree of
    Nothing -> pure . SimpleStr $ "extraction failed for e-class " <> show eid
    Just tree -> do
      -- Get theta from fit DB
      thetaText <- liftIO $ withBackend fitPath $ \fitDb -> do
        dsid <- Q.getOrCreateDataset fitDb ds
        rows <- queryDb fitDb
          "SELECT theta FROM dataset_fit WHERE dataset_id = ? AND eid = ?"
          [SqlInteger (fromIntegral dsid), SqlInteger (fromIntegral eid)]
        case rows of
          [[th]] -> pure (sqlToText th)
          _      -> pure ""

      let theta = case parseTheta (T.unpack thetaText) of
            []    -> VU.empty
            (v:_) -> v

      if VU.length theta < 2
        then pure . SimpleStr $ "theta has fewer than 2 parameters, cannot profile"
        else do
          -- Load dataset
          ((xTr, yTr, _, _), (mYErr, _), _, _) <- liftIO $ loadDataset dataPath True
          let dist = Gaussian
              nSamples = VU.length yTr
              kTree = countParamsUniq tree
              nNoiseParams = 1  -- Gaussian adds sigma parameter
              k = min (VU.length theta) (kTree + nNoiseParams)
              thetaProf = VU.take k theta
              et = compileTree dist xTr yTr mYErr tree

          -- Find correct MLE via random restarts (stored theta may be wrong)
          let nRestarts = 10 :: Int
              optimizer = ctOptimizer et
          mOptTheta <- liftIO $ Control.Exception.try $ do
            restarts <- mapM (\_ -> do
              theta0 <- VU.replicateM k (pure (fromRational (toRational (unsafePerformIO (randomRIO (-5.0, 5.0) :: IO Double)))))
              let thetaOpt = optimizer theta0
              pure (ctNLL et thetaOpt, thetaOpt)
              ) [1..nRestarts]
            let storedOpt = optimizer thetaProf
                storedNll = ctNLL et storedOpt
                (bestNll, bestTheta) = minimum restarts
            pure $ if storedNll < bestNll then storedOpt else bestTheta

          case mOptTheta of
            Left (ex :: SomeException) -> pure . SimpleStr $ "MLE optimization failed: " ++ show ex
            Right mleTheta -> do
              -- Get standard errors from Hessian at MLE
              let stdErrsMle = _stdErr $ getStatsFromModel dist mYErr xTr yTr tree mleTheta

              -- Run Bates profiling with correct MLE
              mBatesProfiles <- liftIO $ Control.Exception.try $
                getAllProfiles Bates et mleTheta stdErrsMle [] 0.05

              case mBatesProfiles of
                Left (ex :: SomeException) -> pure . SimpleStr $ "Bates profiling failed: " ++ show ex
                Right batesProfiles -> do
                  let -- Take only tree-parameter profiles
                      batesTreeProfiles = Prelude.take kTree batesProfiles
                      nTreeProfs = length batesTreeProfiles

                      -- Section 1: profile data from Bates walk
                      profileLines = concatMap (profileToCSV nTreeProfs) (zip [0..] batesTreeProfiles)
                      -- Section 2: contour from Bates profiles (proper splines)
                      contourLines = if nTreeProfs >= 2
                        then ["i,j,theta_i,theta_j"]
                             ++ [ show i <> "," <> show j <> "," <> show ti <> "," <> show tj
                                | (i,j) <- [(i,j) | i <- [0..nTreeProfs-1], j <- [i+1..nTreeProfs-1]]
                                , (ti, tj) <- approximateContour kTree nSamples batesTreeProfiles i j 0.05
                                ]
                        else []

                      result = unlines $
                        ["param,tau," <> intercalate "," [ "theta_" <> show c | c <- [0..nTreeProfs-1] ] <> ",opt"]
                        ++ profileLines
                        ++ [""]
                        ++ contourLines

                  mResult <- liftIO $ Control.Exception.try (evaluate (force result) :: IO String)
                  case mResult of
                    Left (ex :: SomeException) -> pure . SimpleStr $ "profiling failed: " ++ show ex
                    Right r -> pure . SimpleStr $ r

-- | DB-native optimize: extract the best expression, re-fit with NLopt,
-- write fitness back to dataset_fit. No in-memory graph needed for extraction;
-- fitting uses the dataset loaded inside this command.
run (DBOptimize fname fitPath ds dataPath eid ci lossName) = do
  let loss = fromMaybe (NLL Gaussian) (readLoss lossName)
  mTree <- liftIO $ withBackend fname $ \db -> extractBestFromDB db eid
  case mTree of
    Nothing -> pure $ SimpleStr "e-class not found or extraction failed"
    Just tree -> do
      -- Load the dataset data from its FILE PATH (dataPath) for fitting; the
      -- dataset identity (ds) is its name, used for the dataset_fit lookup.
      dataTrainsWP' <- liftIO $ Prelude.mapM (flip loadDataset True) (words dataPath)
      let dataTrains = Prelude.map (\((a, b, _, _), (c, _), v, _) -> ((a,b,c), v)) dataTrainsWP'
          trainDatas = Prelude.map fst dataTrains
          t = relabelParams tree
      -- Fit with NLopt using the monadic fitnessFunRep
      let dataTrainsVals = Prelude.zip trainDatas (Prelude.map snd dataTrains)
      response <- forM dataTrainsVals $ \(dt, dv) -> fitnessFunRep 100 loss dt t
      let fitness = Prelude.minimum (Prelude.map fst response)
          thetas = Prelude.map snd response
          thetaText = T.pack (serializeTheta thetas)
      -- Write to dataset_fit
      liftIO $ withBackend fitPath $ \fitDb -> do
        dsid <- Q.getOrCreateDataset fitDb ds
        Q.writeDatasetFit fitDb dsid eid (Just fitness) Nothing thetaText 0
      pure $ SimpleStr ("optimized e-class " <> show eid <> ": fitness=" <> show fitness)

-- | DB-native subtrees: walk the best expression tree from DB pages and
-- collect all reachable e-class IDs. No in-memory graph needed.
run (DBSubtrees fname fitPath ds eid) = do
  eids <- liftIO $ withBackend fname $ \db -> expandTreeFromDB db eid
  pure . SimpleStr $ intercalate "," (map show eids)

-- | DB-native getNExprs: extract up to N expression variants from a single
-- e-class by reading its page and iterating over e-nodes.
run (DBGetNExprs fname fitPath ds n eid) = do
  exprs <- liftIO $ withBackend fname $ \db -> do
    mPage <- readPage db eid
    case mPage of
      Nothing -> pure []
      Just page -> do
        let ec = decode page :: EClass
            nodes = Prelude.take n $ Set.toList (_eNodes ec)
        forM nodes $ \en -> extractTreeFromNode db IntSet.empty 0 en
  let rows = [showExpr t | Just t <- exprs]
  pure . SimpleStr $ intercalate "\n" ("Expression" : rows)

-- | DB-native getNEclasses: like getNExprs but returns e-class ID sets.
run (DBGetNEclasses fname fitPath ds n eid) = do
  eclassSets <- liftIO $ withBackend fname $ \db -> do
    mPage <- readPage db eid
    case mPage of
      Nothing -> pure []
      Just page -> do
        let ec = decode page :: EClass
            nodes = Prelude.take n $ Set.toList (_eNodes ec)
        forM nodes $ \en -> do
          let ids = collectEClassIds en
          pure ids
  let rows = [intercalate "," (map show ids) | ids <- eclassSets]
  pure . SimpleStr $ intercalate "\n" ("EClassIds" : rows)

-- | DB-native eclass-terminals: walk all e-nodes in a class and collect
-- unique terminals (Var, Param, Const) with cycle detection.
run (DBEClassTerminals fname fitPath ds eid) = do
  terminals <- liftIO $ withBackend fname $ \db -> do
    mPage <- readPage db eid
    case mPage of
      Nothing -> pure []
      Just page -> do
        let ec = decode page :: EClass
            nodes = Set.toList (_eNodes ec)
        collectAllTerminals db eid nodes
  let rows = map (\(typ, name) -> typ <> "," <> name) terminals
  pure . SimpleStr $ intercalate "\n" ("Type,Name" : rows)

-- | DB-native top with pattern matching: memory-bounded approach.
-- 1. Query top M candidates by fitness from dataset_fit (M >> N)
-- 2. For each candidate, load its best-expression tree and check pattern match
-- 3. Return the first N that match, with wildcard bindings
-- Memory is O(M + depth) — no full match result set in memory.
run (DBTopPattern fname fitPath ds n patStr isRoot negate ci) = do
  -- 1. Parse pattern
  let etree = parsePat (B.pack patStr)
  case etree of
    Left _ -> pure . SimpleStr $ "no parse for " <> patStr
    Right pat -> do
      let wildcards = collectWildcards pat
      -- 2. Get top M candidates by fitness (M = 10x requested N, bounded)
      let multiplier = 10
          m = n * multiplier
      candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
        dsid <- Q.getOrCreateDataset fitDb ds
        Q.topN fitDb dsid m
      -- 3. Check each candidate: extract its best tree and match against pattern
      matches <- liftIO $ withBackend fname $ \egDb ->
        fmap catMaybes $ forM candidates $ \(eid, fit) -> do
          mTree <- extractBestFromDB egDb eid
          case mTree of
            Nothing -> pure Nothing
            Just tree -> do
              let treePat = cata (\t -> Fixed t) tree
                  matched = if isRoot
                    then patternMatches pat treePat
                    else patternMatchesAny pat treePat
                  keep = if negate then not matched else matched
              if keep
                then case matchTree pat tree of
                       Just bindings -> pure $ Just (eid, fit, bindings)
                       Nothing       -> pure $ Just (eid, fit, Map.empty)
                else pure Nothing
      -- 4. Extract expressions for the final results
      rows <- liftIO $ withBackend fname $ \egDb ->
        forM (Prelude.take n matches) $ \(eid, fit, bindings) -> do
          mTree <- extractBestFromDB egDb eid
          pure (eid, fit, mTree, bindings)
      -- 5. For CI, read theta from fit DB and load dataset
      (thetaMap, mDataLoaded) <- case ci of
        Nothing -> pure ([], Nothing)
        Just dataPath -> do
          tm <- liftIO $ withBackend fitPath $ \fitDb -> do
            dsid <- Q.getOrCreateDataset fitDb ds
            let eids = map (\(eid,_,_,_) -> eid) rows
                inClause = "(" <> T.pack (intercalate "," (map show eids)) <> ")"
            thRows <- queryDb fitDb
              ("SELECT eid, theta FROM dataset_fit WHERE dataset_id = ? AND eid IN " <> inClause)
              [SqlInteger (fromIntegral dsid)]
            pure [ (sqlToInt e, sqlToText t) | [e, t] <- thRows ]
          md <- liftIO $ do
            ((xTr, yTr, _, _), (mYErr, _), _, _) <- loadDataset dataPath True
            pure $ Just (xTr, yTr, mYErr)
          pure (tm, md)
      -- 6. Format output
      let header = "Id,Expression,Fitness" <> concat ["," <> vname | (_, vname) <- wildcards]
      body <- forM rows $ \(eid, fit, mTree, bindings) ->
        case mTree of
          Nothing -> pure $ show eid <> ",<extraction failed>," <> show fit
                             <> concat (Prelude.replicate (length wildcards) ",?")
          Just tree -> do
            let t = showExpr tree
                wcCols = concatMap (\(c, vname) ->
                           case Map.lookup c bindings of
                             Just subtree -> "," <> showExpr subtree
                             Nothing      -> ",?")
                         wildcards
                base = intercalate "," [show eid, t, show fit] <> wcCols
            case (ci, mDataLoaded) of
              (Just _, Just (xTr, yTr, mYErr)) -> do
                let thetaText = lookup eid thetaMap
                    theta = case thetaText of
                      Nothing -> VU.empty
                      Just th -> case parseTheta (T.unpack th) of
                        []    -> VU.empty
                        (v:_) -> v
                    dist = Gaussian
                    nSamples = VU.length yTr
                    et = compileTree dist xTr yTr mYErr tree
                    stats = getStatsFromModel dist mYErr xTr yTr tree theta
                profiles <- liftIO $ getAllProfiles Bates et theta (_stdErr stats) [] 0.05
                let ciVals = paramCI (Profile stats profiles) nSamples theta 0.05
                    maxP = VU.length theta
                    ciStr = intercalate ","
                          $ Prelude.map (\(CI _ l h) -> show l <> "," <> show h) ciVals
                          ++ Prelude.replicate (2 * (maxP - length ciVals)) ""
                pure $ base <> "," <> ciStr
              _ -> pure base
      pure . SimpleStr $ intercalate "\n" (header : body)

-- | Out-of-core seed import: stream every expression from the CSV file
-- directly into the database (structural, content-addressed) instead of first
-- building the e-graph in RAM. The produced DB is identical to 'persist' of
-- the corresponding in-memory seed and can be saturated with 'dbEqSat'.
run (ImportDB fname eqs ds dist varnames params) = do
  let alg = getFormat eqs
      toT  [eq, t, f] = (eq, Prelude.map Prelude.read $ Prelude.filter (not.null) $ splitOn ";" t, fromMaybe (-1.0/0.0) $ readMaybe f)
      toT xss = error $ show xss
      parseOne (eq, ps, f) = case parseSR alg (B.pack varnames) False (B.pack eq) of
        Left _ -> Nothing
        Right tree' -> do
          let (tree, pvs) = if params then floatConstsToParam tree' else (tree', [])
              theta       = if params then if dist==MSE then pvs <> ps else pvs else ps
          Just (relabelP0 tree, [VU.fromList theta], Just f)
  content <- liftIO $ Prelude.map (toT . splitOn ",") . lines <$> readFile eqs
  r <- liftIO $ withBackend fname $ \db -> importEqs db (Just ds) (catMaybes (Prelude.map parseOne content))
  pure . SimpleStr $ case r of
    Left err -> "db-import failed: " <> err
    Right s  -> "imported " <> show (isExpressions s) <> " expressions (" <> show (isClasses s) <> " e-classes) into " <> fname
  where
    relabelP0 = cata alg
      where
        alg (Uni f t) = Fix (Uni f t)
        alg (Bin op l r) = Fix (Bin op l r)
        alg (Param ix) = Fix (Param 0)
        alg x = Fix x

run (DistTokens n) = do
  ee <- if n > 0
           then IntSet.toList . IntSet.fromList <$> getTopFitEClassThat n (const True)
           else getAllEvaluatedEClasses
  allPats <- getAllTokensFrom Map.empty ee
  pure . Counts $ (Map.toList allPats)

run (ExtractPat eid) = do
  pats <- getAllPatterns (<= 10) eid
  pure . Counts $ Prelude.map (\(p, c) -> (p, (c, 0))) $ Map.toList pats

run (PatternMap patStr mLimit) = do
  let etree = parsePat $ B.pack patStr
  case etree of
    Left _ -> pure . SimpleStr $ "no parse for " <> patStr
    Right pat -> do
      let wildcards = collectWildcards pat
          wcEnum = Map.fromList [(fromEnum c, vname) | (c, vname) <- wildcards]
      results <- match pat
      rows <- forM results $ \(_subst, root) ->
        case root of
          Left eid -> do
            eid' <- canonical eid
            best <- relabelParams <$> getBestExpr eid'
            let rootExpr = showExpr best
            wcCols <- forM wildcards $ \(c, vname) -> do
              let varKey = Right (fromEnum c)
              case Map.lookup varKey _subst of
                Just (SVOne (Left weid)) -> do
                  weid' <- canonical weid
                  wbest <- relabelParams <$> getBestExpr weid'
                  pure (vname, showExpr wbest, show weid')
                _ -> pure (vname, "?", "?")
            pure (rootExpr, wcCols)
          _ -> pure ("?", [])
      let limited = maybe id Prelude.take mLimit rows
          header = "Match,Expression" <> concat ["," <> vname <> "," <> vname <> "_eid" | (_, vname) <- wildcards]
          body = [ intercalate "," (show idx : expr : concatMap (\(_, e, eid') -> [e, eid']) wc)
                 | (idx, (expr, wc)) <- zip [0 :: Int ..] limited]
      pure . SimpleStr $ intercalate "\n" (header : body)

run (EClassTerminals eid) = do
  ec <- getEClass eid
  let nodes = Set.toList (_eNodes ec)
  terms <- evalStateT (concat <$> mapM collectFromNode nodes) Set.empty
  let header = "Type,Name"
      rows = [ intercalate "," [kind, name] | (kind, name) <- sortOn snd terms ]
  pure . SimpleStr $ if null rows then header else intercalate "\n" (header : rows)
  where
    collectFromNode :: ENode -> StateT (Set.HashSet Int) MyEGraph [(String, String)]
    collectFromNode (EVar ix)   = pure [("Var", 'x' : show ix)]
    collectFromNode (EParam ix) = pure [("Param", 't' : show ix)]
    collectFromNode (EConst x)  = pure [("Const", show x)]
    collectFromNode en = do
      let children = eChildren en
      concat <$> mapM collectFromEc children

    collectFromEc :: EClassId -> StateT (Set.HashSet Int) MyEGraph [(String, String)]
    collectFromEc ecId = do
      ecId' <- lift $ canonical ecId
      visited <- get
      if ecId' `Set.member` visited
        then pure []
        else do
          modify' (Set.insert ecId')
          ec <- lift $ getEClass ecId'
          let nodes = Set.toList (_eNodes ec)
          concat <$> mapM collectFromNode nodes

--run (EqSatStep n dataInfo) = do (forM rewrites $ \r -> runEqSat myCost [r] n) >> refitChanged dataInfo
--                                pure NoPrint
run (EqSatStep n dataInfo) = do createDB 
                                forM_ [1..n] $ \_ -> (do runEqSat myCost rewrites 1
                                                         createDB
                                                         -- durable commit point: write back any
                                                         -- dirty e-class pages (no-op unless paged)
                                                         flushGraphStore)
                                refitChanged dataInfo
                                pure NoPrint

run (GetNExprs n eid) = do ts <- getNExpressionsFrom n eid
                           pure $ MultiTrees ts

run (GetNEclass n eid) = do
  ids <- getNEclassFrom n eid
  pure $ MultiClass ids

-- dataInfo = (dist, trainDatas, testData)
--  runEqSat myCost rewrites 1

-- * DB-native bounded-N commands (top-N with hard cap of 10000)

-- | DB-native distribution: pattern enumeration over top-N expressions.
run (DBDistribution fname fitPath ds n) = do
  let n' = clampN n
  candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
    dsid <- Q.getOrCreateDataset fitDb ds
    Q.topN fitDb dsid n'
  results <- liftIO $ withBackend fname $ \egDb ->
    fmap (Map.unionsWith addTuple) $ forM candidates $ \(eid, fit) -> do
      mTree <- extractBestFromDB egDb eid
      case mTree of
        Nothing -> pure Map.empty
        Just tree -> pure $ Map.map (, fit) (getAllPatternsOnTree tree)
  let averaged = Map.map (\(v1, v2) -> (v1, v2 / fromIntegral v1)) results
      sorted = sortOn (Down . snd . snd) (Map.toList averaged)
      header = "Pattern,Count,AvgFitness"
      body = [ show p <> "," <> show cnt <> "," <> show avgFit
             | (p, (cnt, avgFit)) <- sorted ]
  pure . SimpleStr $ intercalate "\n" (header : body)

-- | DB-native modularity: find reusable sub-components in top-N expressions.
run (DBModularity fname fitPath ds n) = do
  let n' = clampN n
  candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
    dsid <- Q.getOrCreateDataset fitDb ds
    Q.topN fitDb dsid n'
  freqMap <- liftIO $ withBackend fname $ \egDb ->
    foldM (\acc (eid, _) -> do
      eids <- expandTreeFromDB egDb eid
      pure $ foldl' (\m e -> Map.insertWith (+) e (1 :: Int) m) acc eids
    ) Map.empty candidates
  let reusable = Map.filter (> 1) freqMap
      sorted = sortOn (Down . snd) (Map.toList reusable)
      header = "SubEClassId,RefCount"
      body = [ show eid <> "," <> show cnt | (eid, cnt) <- sorted ]
  pure . SimpleStr $ intercalate "\n" (header : body)

-- | DB-native count-pattern: count structural pattern matches in top-N expressions.
run (DBCountPat fname fitPath ds patStr n) = do
  let n' = clampN n
  case parsePat (B.pack patStr) of
    Left _ -> pure . SimpleStr $ "no parse for " <> patStr
    Right pat -> do
      candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
        dsid <- Q.getOrCreateDataset fitDb ds
        Q.topN fitDb dsid n'
      count <- liftIO $ withBackend fname $ \egDb ->
        foldM (\acc (eid, _) -> do
          mTree <- extractBestFromDB egDb eid
          case mTree of
            Nothing -> pure acc
            Just tree -> do
              let treePat = cata (\t -> Fixed t) tree
                  matched = patternMatches pat treePat || patternMatchesAny pat treePat
              pure (if matched then acc + 1 else acc)
        ) 0 candidates
      pure . SimpleStr $ "pattern '" <> patStr <> "' matched " <> show count <> " of " <> show n' <> " expressions"

-- | DB-native pattern-map: show wildcard bindings for pattern matches in top-N.
run (DBPatternMap fname fitPath ds patStr n) = do
  let n' = clampN n
  case parsePat (B.pack patStr) of
    Left _ -> pure . SimpleStr $ "no parse for " <> patStr
    Right pat -> do
      let wildcards = collectWildcards pat
      candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
        dsid <- Q.getOrCreateDataset fitDb ds
        Q.topN fitDb dsid n'
      matches <- liftIO $ withBackend fname $ \egDb ->
        fmap catMaybes $ forM candidates $ \(eid, fit) -> do
          mTree <- extractBestFromDB egDb eid
          case mTree of
            Nothing -> pure Nothing
            Just tree -> do
              let treePat = cata (\t -> Fixed t) tree
                  matched = patternMatches pat treePat || patternMatchesAny pat treePat
              if matched
                then case matchTree pat tree of
                       Just bindings -> pure $ Just (eid, fit, tree, bindings)
                       Nothing       -> pure Nothing
                else pure Nothing
      let header = "Id,Expression,Fitness" <> concat ["," <> vname | (_, vname) <- wildcards]
          body = [ intercalate "," [show eid, showExpr tree, show fit]
                    <> concatMap (\(c, vname) ->
                         case Map.lookup c bindings of
                           Just subtree -> "," <> showExpr subtree
                           Nothing      -> ",?")
                   wildcards
                 | (eid, fit, tree, bindings) <- matches ]
      pure . SimpleStr $ intercalate "\n" (header : body)

-- | DB-native extract-pattern: enumerate patterns in a single expression.
run (DBExtractPat fname fitPath ds eid) = do
  mTree <- liftIO $ withBackend fname $ \db -> extractBestFromDB db eid
  case mTree of
    Nothing -> pure . SimpleStr $ "e-class " <> show eid <> " not found or extraction failed"
    Just tree -> do
      let pats = getAllPatternsOnTree tree
          sorted = sortOn (Down . snd) (Map.toList pats)
          header = "Pattern,Count"
          body = [ show p <> "," <> show c | (p, c) <- sorted ]
      pure . SimpleStr $ intercalate "\n" (header : body)

-- | DB-native distributionOfTokens: count token frequencies in top-N expressions.
run (DBDistTokens fname fitPath ds n) = do
  let n' = clampN n
  candidates <- liftIO $ withBackend fitPath $ \fitDb -> do
    dsid <- Q.getOrCreateDataset fitDb ds
    Q.topN fitDb dsid n'
  results <- liftIO $ withBackend fname $ \egDb ->
    fmap (Map.unionsWith addTuple) $ forM candidates $ \(eid, fit) -> do
      mTree <- extractBestFromDB egDb eid
      case mTree of
        Nothing -> pure Map.empty
        Just tree -> pure $ Map.map (, fit) (getAllTokensOnTree tree)
  let averaged = Map.map (\(v1, v2) -> (v1, v2 / fromIntegral v1)) results
      sorted = sortOn (Down . snd . snd) (Map.toList averaged)
      header = "Token,Count,AvgFitness"
      body = [ show t <> "," <> show cnt <> "," <> show avgFit
             | (t, (cnt, avgFit)) <- sorted ]
  pure . SimpleStr $ intercalate "\n" (header : body)

-- | Helper: add tuples element-wise
addTuple :: (Int, Double) -> (Int, Double) -> (Int, Double)
addTuple (a, b) (c, d) = (a + c, b + d)

-- | Max N cap for bounded DB-native commands
maxBoundedN :: Int
maxBoundedN = 10000

clampN :: Int -> Int
clampN n = Prelude.min n maxBoundedN

-- | Pure pattern enumeration on a Fix SRTree (no IO, no state).
-- Returns Map Pattern Int counting occurrences of each structural sub-pattern.
getAllPatternsOnTree :: Fix SRTree -> Map.Map Pattern Int
getAllPatternsOnTree tree = go Map.empty tree
  where
    go acc (Fix (Var ix))     = Map.insertWith (+) (Fixed (Var ix)) 1
                                $ Map.insertWith (+) (VarPat 'A') 1 acc
    go acc (Fix (Param ix))   = Map.insertWith (+) (Fixed (Param ix)) 1
                                $ Map.insertWith (+) (VarPat 'A') 1 acc
    go acc (Fix (Const x))    = Map.insertWith (+) (Fixed (Const x)) 1
                                $ Map.insertWith (+) (VarPat 'A') 1 acc
    go acc (Fix (Uni f t))    = let pats = go Map.empty t
                                    acc' = Map.insertWith (+) (VarPat 'A') 1 acc
                                in Map.unionWith (+) acc'
                                   (Map.mapKeys (\t' -> Fixed (Uni f t')) pats)
    go acc (Fix (Bin op l r)) = let patsL = go Map.empty l
                                    patsR = go Map.empty r
                                    acc'  = Map.insertWith (+) (VarPat 'A') 1 acc
                                in Map.unionWith (+) acc'
                                   (Map.fromList [(relabelVarPat (Fixed (Bin op l' r')), min vl vr)
                                                 | (l', vl) <- Map.toList patsL
                                                 , (r', vr) <- Map.toList patsR])
    go acc (Fix (Y _))        = Map.insertWith (+) (VarPat 'A') 1 acc

-- | Pure token frequency counting on a Fix SRTree.
-- Counts operator shapes (e.g., "how many Add, how many Exp").
getAllTokensOnTree :: Fix SRTree -> Map.Map Pattern Int
getAllTokensOnTree tree = go Map.empty tree
  where
    go acc (Fix (Var ix))     = Map.insertWith (+) (Fixed (Var ix)) 1 acc
    go acc (Fix (Param ix))   = Map.insertWith (+) (Fixed (Param ix)) 1 acc
    go acc (Fix (Const x))    = Map.insertWith (+) (Fixed (Const x)) 1 acc
    go acc (Fix (Uni f t))    = Map.insertWith (+) (Fixed (Uni f (VarPat 'A'))) 1
                                $ go acc t
    go acc (Fix (Bin op l r)) = Map.insertWith (+) (Fixed (Bin op (VarPat 'A') (VarPat 'B'))) 1
                                $ go (go acc l) r
    go acc (Fix (Y _))        = acc

-- * DB-native helper functions (for commands that don't load in-memory EGraph)

-- | Collect all eclass IDs reachable from a root by walking @_best@ pointers
-- through DB pages. O(depth) memory, no in-memory EGraph needed.
expandTreeFromDB :: SqlBackend db => db -> EClassId -> IO [EClassId]
expandTreeFromDB db root = IntSet.toList <$> go IntSet.empty 0 root
  where
    go seen _ eid | IntSet.member eid seen = pure seen
    go seen n _ | n >= 200 = pure seen
    go seen n eid = do
      mPage <- readPage db eid
      case mPage of
        Nothing -> pure (IntSet.insert eid seen)
        Just page -> do
          let ec = decode page :: EClass
              seen' = IntSet.insert eid seen
              best = _best (_info ec)
          goNode seen' (n+1) best

    goNode seen _ (EVar _)   = pure seen
    goNode seen _ (EParam _) = pure seen
    goNode seen _ (EConst _) = pure seen
    goNode seen n (EUni _ t) = go seen n t
    goNode seen n (EBin _ l r) = do
      s <- go seen n l
      go s n r
    goNode seen n (ENAry _ m) =
      foldM (\s (cid, _) -> go s n cid) seen (IntMap.toAscList m)

-- Helper: extract a tree from a single ENode (for DBGetNExprs)
extractTreeFromNode :: SqlBackend db => db -> IntSet.IntSet -> Int -> ENode -> IO (Maybe (Fix SRTree))
extractTreeFromNode _ _ _ (EVar ix)   = pure (Just (Fix (Var ix)))
extractTreeFromNode _ _ _ (EParam ix) = pure (Just (Fix (Param ix)))
extractTreeFromNode _ _ _ (EConst x)  = pure (Just (Fix (Const x)))
extractTreeFromNode db seen n (EUni f t) = do
  mt <- extractTreeFromPage db seen (n+1) t
  pure $ Fix . Uni f <$> mt
extractTreeFromNode db seen n (EBin op l r) = do
  ml <- extractTreeFromPage db seen (n+1) l
  case ml of
    Nothing -> pure Nothing
    Just l' -> do
      mr <- extractTreeFromPage db seen (n+1) r
      pure $ Fix . Bin op l' <$> mr
extractTreeFromNode db seen n (ENAry op m) = do
  let children = IntMap.toAscList m
  mts <- expandNaryNodes db seen (n+1) children
  pure $ naryTreeOp op <$> mts

extractTreeFromPage :: SqlBackend db => db -> IntSet.IntSet -> Int -> EClassId -> IO (Maybe (Fix SRTree))
extractTreeFromPage _ _ n _ | n >= 200 = pure Nothing
extractTreeFromPage db seen n eid
  | IntSet.member eid seen = pure Nothing
  | otherwise = do
      mPage <- readPage db eid
      case mPage of
        Nothing -> pure Nothing
        Just page -> do
          let ec = decode page :: EClass
              nodes = Set.toList (_eNodes ec)
          case nodes of
            [] -> pure Nothing
            (en : _) -> extractTreeFromNode db (IntSet.insert eid seen) n en

expandNaryNodes :: SqlBackend db => db -> IntSet.IntSet -> Int -> [(EClassId, Int)] -> IO (Maybe [Fix SRTree])
expandNaryNodes _ _ _ [] = pure (Just [])
expandNaryNodes db seen n ((cid, cnt) : rest) = do
  mc <- extractTreeFromPage db seen n cid
  case mc of
    Nothing -> pure Nothing
    Just c -> do
      mrest <- expandNaryNodes db seen (n+1) rest
      case mrest of
        Nothing -> pure Nothing
        Just rs -> pure (Just (replicate (min cnt (200 - n)) c ++ rs))

naryTreeOp :: NOp -> [Fix SRTree] -> Fix SRTree
naryTreeOp _ [] = Fix (Var 0)
naryTreeOp op ts = foldr1 (\a b -> Fix (Bin (toOp op) a b)) ts

-- Helper: collect e-class IDs from an ENode (for DBGetNEclasses)
collectEClassIds :: ENode -> [EClassId]
collectEClassIds (EVar _)   = []
collectEClassIds (EParam _) = []
collectEClassIds (EConst _) = []
collectEClassIds (EUni _ t) = [t]
collectEClassIds (EBin _ l r) = [l, r]
collectEClassIds (ENAry _ m) = IntMap.keys m

-- | Collect unique terminals from all e-nodes in a class, walking children
-- through DB pages with cycle detection.
collectAllTerminals :: SqlBackend db => db -> EClassId -> [ENode] -> IO [(String, String)]
collectAllTerminals db root nodes = do
  let initTerms = concatMap nodeTerminals nodes
  goTerms IntSet.empty 0 (concatMap nodeChildIds nodes) (nub initTerms)
  where
    nodeTerminals (EVar ix)   = [("Var", "x" <> show ix)]
    nodeTerminals (EParam ix) = [("Param", "t" <> show ix)]
    nodeTerminals (EConst x)  = [("Const", show x)]
    nodeTerminals _           = []

    nodeChildIds (EUni _ t)   = [t]
    nodeChildIds (EBin _ l r) = [l, r]
    nodeChildIds (ENAry _ m)  = IntMap.keys m
    nodeChildIds _            = []

    goTerms _ _ [] terms = pure terms
    goTerms seen n (eid':rest) terms
      | n >= 200 = pure terms
      | IntSet.member eid' seen = goTerms seen n rest terms
      | otherwise = do
          mPage <- readPage db eid'
          case mPage of
            Nothing -> goTerms (IntSet.insert eid' seen) (n+1) rest terms
            Just page -> do
              let ec = decode page :: EClass
                  nodes' = Set.toList (_eNodes ec)
                  terms' = terms ++ concatMap nodeTerminals nodes'
                  children = concatMap nodeChildIds nodes'
                  seen' = IntSet.insert eid' seen
              goTerms seen' (n+1) (rest ++ children) (nub terms')

-- * Pattern matching helpers for DBTopPattern

-- | Does the pattern match the expression tree at the root?
patternMatches :: Pattern -> Pattern -> Bool
patternMatches (Fixed (Var ix)) (Fixed (Var jx))       = ix == jx
patternMatches (Fixed (Param ix)) (Fixed (Param jx))   = ix == jx
patternMatches (Fixed (Const x)) (Fixed (Const y))     = x == y
patternMatches (Fixed (Uni _ pp)) (Fixed (Uni _ tp))   = patternMatches pp tp
patternMatches (Fixed (Bin _ pl pr)) (Fixed (Bin _ tl tr)) =
  patternMatches pl tl && patternMatches pr tr
patternMatches (VarPat _) _                             = True
patternMatches (NAry _ _) (Fixed (Bin _ _ _))          = True
patternMatches (NAry _ _) (Fixed (Uni _ _))            = True
patternMatches _ _                                      = False

-- | Does the pattern match at ANY position (root or sub-expression) in the tree?
patternMatchesAny :: Pattern -> Pattern -> Bool
patternMatchesAny pat tree =
  patternMatches pat tree || any (patternMatchesAny pat) (patChildrenOf tree)

-- | Direct children of a Pattern node (for tree walking).
patChildrenOf :: Pattern -> [Pattern]
patChildrenOf (Fixed (Uni _ t))   = [t]
patChildrenOf (Fixed (Bin _ l r)) = [l, r]
patChildrenOf _                   = []

-- * Pure tree matcher: match a Pattern against a concrete Fix SRTree,
-- returning wildcard bindings. Unlike the e-graph matcher, this works
-- on a single extracted tree without loading the graph.

-- | Match a Pattern against a concrete tree, returning wildcard bindings.
-- Repeated wildcards (e.g. v0 + v0) require both occurrences to bind
-- the same subtree.
matchTree :: Pattern -> Fix SRTree -> Maybe (Map.Map Char (Fix SRTree))
matchTree pat tree = go pat tree Map.empty
  where
    go (VarPat c) t bindings = Just (Map.insertWith checkKey c t bindings)
    go Hole _ bindings = Just bindings
    go (Fixed t) (Fix t') bindings = goChildren t t' bindings
    go (NAry op ncs) (Fix (Bin bop l r)) bindings
      | naryOpMatches op bop = goNary ncs [l, r] bindings
    go _ _ _ = Nothing

    goChildren (Uni f p) (Uni f' t) bindings
      | f == f' = go p t bindings
    goChildren (Bin op pl pr) (Bin op' tl tr) bindings
      | op == op' = do
          lb <- go pl tl bindings
          go pr tr lb
    goChildren (Param ix) (Param ix') bindings
      | ix == ix' = Just bindings
    goChildren (Var ix) (Var ix') bindings
      | ix == ix' = Just bindings
    goChildren (Const x) (Const x') bindings
      | x == x' = Just bindings
    goChildren _ _ _ = Nothing

    -- Match n-ary pattern children against tree children (binary case)
    goNary [] _ bindings = Just bindings
    goNary _ [] bindings = Just bindings
    goNary (Ch p : ps) (t : ts) bindings = do
      lb <- go p t bindings
      goNary ps ts lb
    goNary (Rest c : _) ts bindings =
      -- Rest captures all remaining children as a single compound tree
      Just (Map.insertWith checkKey c (rebuildBin ts) bindings)
    goNary (MapP _ _ : ps) ts bindings = goNary ps ts bindings  -- skip MapP (rewrite targets)

    -- Rebuild a list of trees into a binary tree (for Rest variable binding)
    rebuildBin [t] = t
    rebuildBin (t : ts) = Fix (Bin Add t (rebuildBin ts))
    rebuildBin [] = Fix (Const 0)

    naryOpMatches EAdd Add = True
    naryOpMatches EMul Mul = True
    naryOpMatches _ _ = False

    -- Check that repeated wildcards bind structurally equal subtrees
    checkKey :: Fix SRTree -> Fix SRTree -> Fix SRTree
    checkKey new old
      | showExpr new == showExpr old = old
      | otherwise = old  -- keep first binding (conservative on mismatch)

-- * auxiliary functions
-- | Write back any pending dirty e-class pages (durable commit point).
flushGraphStore :: MyEGraph ()
flushGraphStore = do
  eg <- get
  liftIO (flushStore eg)

withBackend :: String -> (forall b. SqlBackend b => b -> IO a) -> IO a
withBackend spec k
  | "postgresql://" `isPrefixOf` spec || "postgres://" `isPrefixOf` spec =
      bracket (connectdb (B.pack spec)) finish (\c -> k c)
  | otherwise =
      bracket (open (T.pack spec)) close (\d -> k d)

-- | Like 'withBackend' but the connection is kept open on success: it is owned by
-- the value returned from @k@ (e.g. a paged e-graph whose page store references
-- it) and is finalised when that value is dropped. Only on exception is the
-- connection closed here.
withBackendKeepOpen :: String -> (forall b. SqlBackend b => b -> IO a) -> IO a
withBackendKeepOpen spec k
  | "postgresql://" `isPrefixOf` spec || "postgres://" `isPrefixOf` spec =
      bracketOnError (connectdb (B.pack spec)) finish (\c -> k c)
  | otherwise =
      bracketOnError (open (T.pack spec)) close (\d -> k d)

-- | Open two backend connections (egraph and fit) and run two actions, one on
-- each connection.  When both paths are identical the second connection is
-- still opened (the caller may use different subsets of each connection).
withBackendSplit :: String -> String
                 -> (forall b. SqlBackend b => b -> IO a)
                 -> (forall b. SqlBackend b => b -> IO a)
                 -> IO (a, a)
withBackendSplit egraphSpec fitSpec kEgraph kFit =
  withBackend egraphSpec $ \dbEgraph ->
    withBackend fitSpec $ \dbFit ->
      (,) <$> kEgraph dbEgraph <*> kFit dbFit

importCSV :: Loss -> String -> String -> Bool -> MyEGraph ()
importCSV dist fname hdr convertParam = cleanDB >> parseEqs >> createDB >> rebuildAllRanges
  where
    alg = getFormat fname

    toTuple :: [String] -> (String, [Double], Double)
    toTuple [eq, t, f] = (eq, Prelude.map Prelude.read $ Prelude.filter (not.null) $ splitOn ";" t, fromMaybe (-1.0/0.0) $ readMaybe f)
    toTuple xss = error $ show xss

    relabelP0 = cata alg
      where
        alg (Uni f t) = Fix (Uni f t)
        alg (Bin op l r) = Fix (Bin op l r)
        alg (Param ix) = Fix (Param 0)
        alg x = Fix x

    parseEqs :: MyEGraph ()
    parseEqs = do content <- Prelude.map (toTuple . splitOn ",") . lines <$> (liftIO $ readFile fname)
                  forM_ content $ \(eq, params, f) -> do
                    case parseSR alg (B.pack hdr) False (B.pack eq) of
                         Left _ -> pure ()
                         Right tree' -> do
                           let (tree, ps) = if convertParam then floatConstsToParam tree' else (tree', theta)
                               theta      = if convertParam then if dist==MSE then ps <> params else ps else params
                           eid <- fromTree myCost (relabelP0 tree) >>= canonical
                           -- TODO: how to import MvSR?
                           insertFitness eid f $ [VU.fromList theta]
                           runEqSat myCost rewritesParams 1
                           cleanDB


parseCSV :: Loss -> String -> String -> Bool -> IO EGraph
parseCSV dist fname hdr convertParam = do g <- (execStateT parseEqs emptyGraph) -- `evalStateT` (mkStdGen 0)
                                          pure g
  where
    alg = getFormat fname

    toTuple :: [String] -> (String, [Double], Double)
    toTuple [eq, t, f] = (eq, Prelude.map Prelude.read $ Prelude.filter (not.null) $ splitOn ";" t, fromMaybe (-1.0/0.0) $ readMaybe f)
    toTuple xss = error $ show xss

    parseEqs :: MyEGraph ()
    parseEqs = do content <- Prelude.map (toTuple . splitOn ",") . lines <$> (liftIO $ readFile fname)
                  forM_ content $ \(eq, params, f) -> do
                    case parseSR alg (B.pack hdr) False (B.pack eq) of
                         Left _ -> pure ()
                         Right tree' -> do
                           let (tree, ps) = if convertParam then floatConstsToParam tree' else (tree', theta)
                               theta      = if convertParam then if dist==MSE then ps <> params else ps else params
                           eid <- fromTree myCost tree >>= canonical
                           -- TODO: how to import MvSR?
                           insertFitness eid f $ [VU.fromList theta]
                           runEqSat myCost rewritesParams 1
                           cleanDB
getFormat :: String -> SRAlgs
getFormat = Prelude.read . Prelude.map toUpper . Prelude.last . splitOn "."

-- | Resolve an equation-format/algorithm name (e.g. \"TIR\", \"hl\", \"operon\")
-- to the 'SRAlgs' parser to use, defaulting to TIR on an unknown name.
algToAlgs :: String -> SRAlgs
algToAlgs = fromMaybe TIR . readMaybe . Prelude.map toUpper





convert :: String -> Output -> String -> IO ()
convert fname out hdr = do
  let alg = getFormat fname
  content <- Prelude.map (toTuple . splitOn ",") . lines <$> readFile fname
  forM_ content $ \(eq, params, f) -> do
    case parseSR alg (B.pack hdr) False (B.pack eq) of
          Left _ -> pure ()
          Right tree -> do
            putStr (showOutput out tree)
            putChar ','
            putStr params
            putChar ','
            putStrLn f
  where
    toTuple :: [String] -> (String, String, String)
    toTuple [eq, t, f] = (eq, t, f)
    toTuple xss = error $ show xss

getParents False _ ecs = pure ecs
getParents True  p ecs = IntSet.toList <$> getParentsOf p (IntSet.fromList ecs) 300000 ecs

isBest (e', en') = do e <- canonical e'
                      best <- gets (_best . _info . (IntMap.! e) . _eClass) >>= canonize
                      en <- canonize en'
                      pure (en == best)

getParentsOf :: (EClass -> Bool) -> IntSet.IntSet -> Int -> [EClassId] -> MyEGraph IntSet.IntSet
getParentsOf p visited n queue | IntSet.size visited >= n || null queue = pure visited
getParentsOf p visited n queue =
   do parents'     <- IntSet.unions <$> Prelude.mapM (\e -> canonical e >>= canonizeParents) queue

      grandParents <- getParentsOf p ((visited <> parents')) n (IntSet.toList parents')
      pure (visited <> grandParents)
   where
      filterUneval uneval = IntSet.filter (`IntSet.notMember` uneval)
      isNew ec (e, en) = ec `Prelude.elem` (eChildren en) && (e `IntSet.notMember` visited)
      canonizeParents ec = do ecl <- gets ((IntMap.! ec) . _eClass)
                              let parents' = Set.toList . Set.filter (isNew ec) $ _parents ecl
                              parents <- Prelude.map fst <$> filterM isBest parents'
                              pure (IntSet.fromList parents)

isLeft (Left _)   = True
isLeft _          = False
fromLeft (Left x) = x
fromLeft _        = undefined

collectWildcards :: Pattern -> [(Char, String)]
collectWildcards = Map.toAscList . Map.fromList . go
  where
    go (VarPat c) = [(c, 'v' : show (fromEnum c - 65))]
    go (Fixed (Uni _ t)) = go t
    go (Fixed (Bin _ l r)) = go l ++ go r
    go (Fixed _) = []
    go Hole = []
    go (NAry _ ncs) = concatMap goNChild ncs
    goNChild (Ch p) = go p
    goNChild (Rest c) = [(c, 'v' : show (fromEnum c - 65))]
    goNChild (MapP p _) = go p

getAllTokensFrom :: Map.Map Pattern (Int, Double) -> [EClassId] -> MyEGraph (Map.Map Pattern (Int, Double))
getAllTokensFrom counts [] = pure $ Map.map (\(v1, v2) -> (v1, v2/fromIntegral v1)) counts
getAllTokensFrom counts (x:xs) = do fit' <- getFitness x
                                    case fit' of
                                      Nothing -> getAllTokensFrom counts xs
                                      Just fit -> do tokens <- Map.map (,fit) <$> getAllTokens x
                                                     getAllTokensFrom (Map.unionWith addTuple tokens counts) xs

getAllPatternsFrom :: (Int -> Bool) -> Map.Map Pattern (Int, Double) -> [EClassId] -> MyEGraph (Map.Map Pattern (Int, Double))
getAllPatternsFrom pSz counts []     = pure $ Map.map (\(v1, v2) -> (v1, v2/fromIntegral v1)) counts
getAllPatternsFrom pSz counts (x:xs) = do fit' <- getFitness x 
                                          case fit' of 
                                            Nothing -> getAllPatternsFrom pSz counts xs
                                            Just fit -> do
                                                         pats <- Map.map (,fit) <$> getAllPatterns pSz x
                                                         getAllPatternsFrom pSz (Map.unionWith addTuple pats counts) xs

relabelVarPat :: Pattern -> Pattern
relabelVarPat t = alg t `evalState` 65
   where
      alg :: Pattern -> State Int Pattern
      alg (VarPat _) = do ix <- Control.Monad.State.Strict.get; Control.Monad.State.Strict.modify (+1); pure (VarPat $ toEnum ix)
      alg (Fixed (Uni f t')) = do t <- alg t'; pure $ Fixed (Uni f t)
      alg (Fixed (Bin op l' r')) = do l <- alg l'; r <- alg r'; pure $ Fixed (Bin op l r)
      alg pt                   = pure pt

lenPat :: Pattern -> Int
lenPat (Fixed (Uni _ t)) = 1 + lenPat t
lenPat (Fixed (Bin _ l r)) = 1 + lenPat l + lenPat r
lenPat _ = 1

countPattern pat = do
  ecs' <- (Prelude.map fromLeft . Prelude.filter isLeft . Prelude.map snd) <$> match pat
  ecs <- Prelude.mapM canonical ecs'
                    >>= getEvaluated
  pure (pat, IntSet.size ecs)

getEvaluated ecs = getParentsOf (const True) (IntSet.fromList ecs) 500000 ecs

getAllPatterns :: ClassStore m => (Int -> Bool) -> EClassId -> EGraphST m (Map.Map Pattern Int)
getAllPatterns pSz eid = do
   eid' <- canonical eid
   best <- gets (_best . _info . (IntMap.! eid') . _eClass)
   case best of
      EVar ix     -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Var ix), 1)]
      EParam ix   -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Param ix), 1)]
      EConst x    -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Const x), 1)]
      EUni f t    -> do pats <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz t
                        pure $ Map.insertWith (+) (VarPat 'A') 1 
                             $ Map.mapKeysWith (+) (\t' -> Fixed (Uni f t')) pats
      EBin op l r | l==r -> do pats <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz l
                               pure $ Map.insertWith (+) (VarPat 'A') 1 $ Map.mapKeysWith (+) (\t' -> Fixed (Bin op t' t')) pats
                  | otherwise -> do patsL <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz l
                                    patsR <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz r
                                    pure $ Map.fromList $ (VarPat 'A', 1) : [(relabelVarPat $ Fixed (Bin op l' r'), min vl vr) | (l', vl) <- Map.toList patsL, (r', vr) <- Map.toList patsR]
      ENAry op xs -> do pats <- Prelude.mapM (\c -> filterPat pSz <$> getAllPatterns pSz c) (expandedList xs)
                        pure $ Map.insertWith (+) (VarPat 'A') 1 $ combineAll pats
                        where
                          filterPat pSz' = Map.filterWithKey (\k _ -> (pSz' . lenPat) k)
                          combineAll []     = Map.empty
                          combineAll [p]    = p
                          combineAll (p:ps) = filterPat pSz $ combineBin p (combineAll ps)
                          combineBin pL pR  = Map.fromList
                            [(relabelVarPat $ Fixed (Bin (toOp op) l' r'), min vl vr)
                            | (l', vl) <- Map.toList pL, (r', vr) <- Map.toList pR]

getAllTokens :: ClassStore m => EClassId -> EGraphST m (Map.Map Pattern Int)
getAllTokens eid = do
  eid' <- canonical eid
  best <- gets (_best . _info . (IntMap.! eid') . _eClass)
  case best of
    EVar ix -> pure $ Map.singleton (Fixed (Var ix)) 1
    EParam ix -> pure $ Map.singleton (Fixed (Param ix)) 1
    EConst x -> pure $ Map.singleton (Fixed (Const x)) 1
    EUni f t -> do pats <- getAllTokens t
                   pure $ Map.insertWith (+) (Fixed (Uni f (VarPat 'A'))) 1 pats
    EBin op l r -> do patsL <- getAllTokens l
                      patsR <- getAllTokens r
                      pure $ Map.insertWith (+) (Fixed (Bin op (VarPat 'A') (VarPat 'B'))) 1
                           $ Map.unionWith (+) patsL patsR
    ENAry op xs -> do pats <- Prelude.mapM getAllTokens (expandedList xs)
                      pure $ Map.insertWith (+) (Fixed (Bin (toOp op) (VarPat 'A') (VarPat 'B'))) 1
                           $ Map.unionsWith (+) pats

isNotTrivial :: ClassStore m => Int -> EClassId -> EGraphST m Bool
isNotTrivial n ec = do
  c <- gets (_consts . _info . (IntMap.! ec) . _eClass)
  m <- gets (_size . _info . (IntMap.! ec) . _eClass)
  pure (c == NotConst && m >= n)
removeNotTrivial :: ClassStore m => Int -> [EClassId] -> EGraphST m [EClassId]
removeNotTrivial n [] = pure []
removeNotTrivial n (ec:ecs) = do
  b <- isNotTrivial n ec
  ecs' <- removeNotTrivial n ecs
  pure $ if b then (ec:ecs') else ecs'

refitChanged (dist, trainDatas, testData) = do
  ids <- gets (_refits . _eDB) >>= Prelude.mapM canonical . IntSet.toList >>= pure . nub
  modify' $ over (eDB . refits) (const IntSet.empty)
  forM_ ids $ \ec -> do t <- relabelParams <$> getBestExpr ec
                        let dataTrainsVals = Prelude.zip trainDatas testData
                        response <- forM dataTrainsVals $ \(dt, dv) -> fitnessFunRep 100 dist dt t
                        let f = Prelude.minimum (Prelude.map fst response)
                            thetas = Prelude.map snd response
                        insertFitness ec f thetas
                        let mdl_train  = Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdlMetric dist mYErr x y theta t) $ Prelude.zip thetas trainDatas
                        insertDL ec mdl_train


mapOfNames :: (Int -> Bool) -> IntMap.IntMap (Int, Int, Int) -> IntMap.IntMap (Int, Int)
mapOfNames maxSz m' =
  let m = IntMap.toList $ IntMap.filter (\(cnt,ps,sz) -> cnt > 1 && maxSz sz) m'
  in IntMap.fromList $ Prelude.zipWith (\(k, (a,b,c)) ix -> (k, (b,ix))) m [0..]

extractEClassList :: ClassStore m => EClassId -> EGraphST m (IntMap.IntMap (Int, Int, Int))
extractEClassList ec' = do
  ec   <- canonical ec'
  best <- gets (_best . _info . (IntMap.! ec) . _eClass) >>= canonize
  mec_b <- gets ((HM.!? best) . _eNodeToEClass)
  case mec_b of
    Nothing   -> pure IntMap.empty
    Just ec_b' -> do ec_b <- canonical ec_b'
                     case best of
                        EUni _ t   -> do m <- extractEClassList t
                                         sm <- createSingle ec_b t t m True
                                         pure $ IntMap.unionWith merge sm m
                        EBin _ l r -> do m1 <- extractEClassList l
                                         m2 <- extractEClassList r
                                         let m = IntMap.unionWith merge m1 m2
                                         sm <- createSingle ec_b l r m False
                                         pure $ IntMap.unionWith merge sm m
                        EParam _   -> pure (IntMap.singleton ec_b (1, 1, 1))
                        ENAry op xs -> do
                          let chs = expandedList xs
                          ms <- Prelude.mapM extractEClassList chs
                          let m = IntMap.unionsWith merge ms
                              (bTot, szTot) = foldr (\c (bs, ss) -> let (_, b, sz) = m IntMap.! c in (bs + b, ss + sz)) (0, 0) chs
                          pure $ IntMap.insertWith merge ec_b (1, bTot, szTot + 1) m
                        _         -> pure (IntMap.singleton ec_b (1, 0, 1))
  where
    merge (count1, ps1, sz1) (count2, ps2, sz2) = (count1+count2, ps1, sz1)
    createSingle ec_b l' r' m uni = do
      l <- canonical l'
      r <- canonical r'
      let (_, b1, sz1) = m IntMap.! l
          (_, b2, sz2) = m IntMap.! r
      pure $ if uni
                then IntMap.singleton ec_b (1, b1, sz1+1)
                else IntMap.singleton ec_b (1, b1 + b2, sz1+sz2+1)
