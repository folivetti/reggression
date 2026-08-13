{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module Commands where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 hiding ( match )
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Text.Read ( readMaybe )
import Data.Monoid (All(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.State.Strict
import Control.Monad ( forM_, filterM, forM )
import Control.Monad.IO.Class ( liftIO )
import Control.Exception ( bracket )
import Data.Char ( toUpper )
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import qualified Data.Vector.Unboxed as VU
import Data.List ( nub, sortOn, intercalate )
import Data.List.Split ( splitOn )
import Control.Lens (over)

import qualified Data.Text as T

import Data.SRTree
import Data.SRTree.Datasets
import Data.SRTree.Recursion
import Data.SRTree.Eval
import Data.SRTree.Print hiding ( printExpr )
import Text.ParseSR (SRAlgs(..), parseSR, Output(..), showOutput)
import System.Random

import Algorithm.SRTree.Likelihoods

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
import Algorithm.EqSat.Storage.SQLite ( saveGraph, loadGraph )
import qualified Algorithm.EqSat.Storage.Query as Q

import Util
import Debug.Trace 
-- * Parsing

-- top 5 by fitness|mdl [less than 5 params, less than 10 nodes]
data Command  = Top Int Filter Criteria PatStr
              | Distribution FilterDist (Maybe Limit) CriteriaDist Int Int
              | DistTokens Int
              | Modular Int FilterDist Criteria
              -- below these will not be a parsable command
              | Report EClassId ArgOpt
              | Optimize EClassId Int ArgOpt
              | Insert String ArgOpt
              | Subtrees EClassId
              | Pareto Criteria
              | CountPat String
              | ExtractPat EClassId
              | Save String
              | Load String
              | Persist String
              | LoadDB String
              | DBTop String Int
              | DBDist String Int
              | DBCount String String
              | DBPareto String
              | Import String Loss String Bool
              | EqSatStep Int ArgOpt
              | GetNExprs Int EClassId
              | Clean Int
              | GetEClassIds Int
              | GetNEclass Int Int

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
              pure $ Top n (getAll . mconcat filters) criteria pats

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

parseLeast = stringCI "with at least " >> decimal 
parseTopDist = stringCI "from top " >> decimal 

-- * SQLite-backed commands (srtree-db)
-- filename must not contain whitespace
parseFname = takeWhile1 (\c -> c /= ' ' && c /= '\n')

parsePersist = string "persist " *> (B.unpack <$> parseFname)
parseLoadDB  = string "db-load " *> (B.unpack <$> parseFname)
parseDBTop   = string "db-top " >>= \_ -> do
  fname <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  pure (DBTop fname n)
parseDBDist  = string "db-distribution " >>= \_ -> do
  fname <- B.unpack <$> parseFname
  stripSp
  n <- decimal
  pure (DBDist fname n)
parseDBCount = string "db-count " >>= \_ -> do
  fname <- B.unpack <$> parseFname
  stripSp
  op <- B.unpack <$> parseFname
  pure (DBCount fname op)
parseDBPareto = string "db-pareto " *> (DBPareto <$> (B.unpack <$> parseFname)) 
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
putEOL bs | B.last bs == '\n' = bs
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
run (Top n filters criteria NoPat) = do
   let getFun = if criteria == ByFitness then getTopFitEClassThat else getTopDLEClassThat
   ids <- getFun n filters
   pure $ MultiExprs $ [(i, IntMap.empty) | i <- reverse ids]
   -- printSimpleMultiExprs (reverse ids)

run (Top n filters criteria withPat) = do
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


run (Report eid (dist, trainData, testData)) = do eid' <- canonical eid
                                                  pure . SingleExpr $ eid' -- printExpr isCLI trainData testData dist eid

run (Optimize eid nIters (dist, trainDatas, testData)) = do -- dist trainData testData
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
                     run (Optimize eid 100 argOpt)

run (Subtrees eid') = do
   eid <- canonical eid'
   isValid <- gets ((IntMap.member eid) . _eClass)
   if isValid
     then do ids <- getAllChildBestEClassesRep eid
             pure . MultiExprs $ [(i, IntMap.empty) | i <- ids]
             --printSimpleMultiExprs isCLI ids
     else pure . SimpleStr $ "Invalid id."

run (Pareto crit) = do
   maxSize <- gets (fst . IntMap.findMax . _sizeFitDB . _eDB)
   ecs <- case crit of
            ByFitness -> getParetoEcsUpTo 1 maxSize
            ByDL      -> getParetoDLEcsUpTo 1 maxSize
   pure . MultiExprs $ [(i, IntMap.empty) | i <- ecs]
   -- printSimpleMultiExprs isCLI ecs

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

run (Persist fname) = do
  eg <- get
  r  <- liftIO $ withSQLite fname $ \db -> saveGraph db eg
  pure . SimpleStr $ case r of
    Left err -> "persist failed: " <> err
    Right () -> "e-graph persisted to " <> fname

run (LoadDB fname) = do
  r <- liftIO $ withSQLite fname loadGraph
  case r of
    Left err -> pure . SimpleStr $ "db-load failed: " <> err
    Right eg -> do put eg
                   -- best/cost are not persisted in the db: recompute the
                   -- cost-minimal best for every e-class (the database holds
                   -- arbitrary nodes otherwise, which can explode when
                   -- expanded for printing/pattern matching)
                   recalculateBestAll myCost
                   pure (SimpleStr ("e-graph loaded from " <> fname))

run (DBTop fname n) = do
  r <- liftIO $ withSQLite fname $ \db -> Q.topN db n
  pure . MultiExprs $ [(eid, IntMap.empty) | (eid, _) <- r]

run (DBDist fname n) = do
  r <- liftIO $ withSQLite fname $ \db -> Q.distributionCounts db n
  pure . SimpleStr . intercalate "\n" $ ("Size,Count" : [show s <> "," <> show c | (s, c) <- r])

run (DBCount fname op) = do
  c <- liftIO $ withSQLite fname $ \db -> Q.countPattern db (T.pack op)
  pure . SimpleStr $ "e-classes containing " <> op <> ": " <> show c

run (DBPareto fname) = do
  r <- liftIO $ withSQLite fname $ \db -> Q.paretoBySize db
  pure . SimpleStr . intercalate "\n" $ ("Id,Fitness,Size" : [show eid <> "," <> show f <> "," <> show s | (eid, f, s) <- r])

run (Import fname dist varnames params) = do
  importCSV dist fname varnames params
  pure NoPrint

run (DistTokens n) = do
  ee <- if n > 0
           then IntSet.toList . IntSet.fromList <$> getTopFitEClassThat n (const True)
           else getAllEvaluatedEClasses
  allPats <- getAllTokensFrom Map.empty ee
  pure . Counts $ (Map.toList allPats)

run (ExtractPat eid) = do
  pats <- getAllPatterns (<= 10) eid
  pure . Counts $ Prelude.map (\(p, c) -> (p, (c, 0))) $ Map.toList pats

--run (EqSatStep n dataInfo) = do (forM rewrites $ \r -> runEqSat myCost [r] n) >> refitChanged dataInfo
--                                pure NoPrint
run (EqSatStep n dataInfo) = do createDB 
                                forM_ [1..n] $ \_ -> (do runEqSat myCost rewrites 1
                                                         createDB)
                                refitChanged dataInfo
                                pure NoPrint

run (GetNExprs n eid) = do ts <- getNExpressionsFrom n eid
                           pure $ MultiTrees ts

run (GetNEclass n eid) = do ids <- getNEclassFrom n eid
                            pure $ MultiClass ids -- $ [(i, IntMap.empty) | i <- ids]
-- dataInfo = (dist, trainDatas, testData)
--  runEqSat myCost rewrites 1

-- * auxiliary functions
withSQLite :: FilePath -> (Database -> IO a) -> IO a
withSQLite fname k = bracket (open (T.pack fname)) close k

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

addTuple (a, b) (c, d) = (a+c, b+d)

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

getAllPatterns :: Monad m => (Int -> Bool) -> EClassId -> EGraphST m (Map.Map Pattern Int)
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

getAllTokens :: Monad m => EClassId -> EGraphST m (Map.Map Pattern Int)
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

isNotTrivial :: Monad m => Int -> EClassId -> EGraphST m Bool
isNotTrivial n ec = do
  c <- gets (_consts . _info . (IntMap.! ec) . _eClass)
  m <- gets (_size . _info . (IntMap.! ec) . _eClass)
  pure (c == NotConst && m >= n)
removeNotTrivial :: Monad m => Int -> [EClassId] -> EGraphST m [EClassId]
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

extractEClassList :: Monad m => EClassId -> EGraphST m (IntMap.IntMap (Int, Int, Int))
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
