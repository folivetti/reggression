{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module Commands where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 hiding ( match )
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Text.Read ( readMaybe )
import Data.Monoid (All(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.State.Strict
import Control.Monad ( forM_, filterM, forM )
import Data.Char ( toUpper )
import qualified Data.Map as Map
import qualified Data.HashSet as Set
import qualified Data.Massiv.Array as MA
import Data.List ( nub, sortOn )
import Data.List.Split ( splitOn )
import Control.Lens (over)

import Data.SRTree
import Data.SRTree.Datasets
import Data.SRTree.Recursion
import Data.SRTree.Eval
import Data.SRTree.Print hiding ( printExpr )
import Text.ParseSR (SRAlgs(..), parseSR, parsePat, Output(..), showOutput)
import System.Random

import Algorithm.SRTree.Likelihoods
import Algorithm.SRTree.Opt

import Algorithm.EqSat
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat.Queries
import Algorithm.EqSat.DB
import Algorithm.EqSat.Simplify

import Algorithm.SRTree.ModelSelection
import Algorithm.EqSat.SearchSR hiding (fitnessFun, fitnessFunRep, fitnessMV)

import Data.Binary ( encode, decode )
import qualified Data.ByteString.Lazy as BS

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
              | Import String Distribution String Bool
              | EqSatStep ArgOpt

type Filter = EClass -> Bool -- pattern?
type FilterDist = Int -> Bool
data Criteria = ByFitness | ByDL deriving Eq
data CriteriaDist = ByCount | ByAvgFit
data Limit = Limit Int Bool deriving Show
data PatStr = PatStr String Bool | AntiPatStr String Bool | NoPat
type ArgOpt = (Distribution, [DataSet], [DataSet])

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
      mbLen ps = MA.unSz $ MA.size $ Prelude.head ps
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

data PrintResults = MultiExprs [EClassId] | SingleExpr EClassId | Counts [(Pattern, (Int, Double))] | SimpleStr String | NoPrint
                  deriving (Show)

-- running
run :: Command -> MyEGraph PrintResults
run (Top n filters criteria NoPat) = do
   let getFun = if criteria == ByFitness then getTopFitEClassThat else getTopDLEClassThat
   ids <- getFun n filters
   pure $ MultiExprs $ reverse ids
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
        pure . MultiExprs $ reverse (nub ids)
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
  ecm <- forM evaluated $ \ec -> do m <- extractEClassList ec
                                    hasMinSz <- isSz m
                                    pure (ec, hasMinSz)
  ids  <- getFun n (const True) $ Prelude.map fst $ Prelude.filter snd ecm
  pure . MultiExprs $ reverse (nub ids)
  where
    isSz m  = do let ecs = Prelude.map fst . IntMap.toList $ IntMap.filter (>1) m
                 szs <- forM ecs getSize
                 pure $ any pSz szs
    extractEClassList :: Monad m => EClassId -> EGraphST m (IntMap.IntMap Int)
    extractEClassList ec' = do
      ec   <- canonical ec'
      best <- gets (_best . _info . (IntMap.! ec) . _eClass)
      mec_b <- fmap (`IntMap.singleton` 1) <$>  gets ((Map.!? best) . _eNodeToEClass)
      case mec_b of
        Nothing   -> pure IntMap.empty
        Just ec_b -> case best of
                      Uni _ t   -> do m <- extractEClassList t
                                      pure $ IntMap.unionWith (+) ec_b m
                      Bin _ l r -> do m1 <- extractEClassList l
                                      m2 <- extractEClassList r
                                      pure $ IntMap.unionsWith (+) [ec_b, m1, m2]
                      _         -> pure ec_b

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
   let mdl_train  = Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdl dist mYErr x y theta t) $ Prelude.zip thetas trainDatas
   insertDL eid mdl_train
   pure . MultiExprs $ [eid]
   --printSimpleMultiExprs isCLI [eid]

run (Insert expr argOpt) = do
  let etree = parseSR TIR "" False $ B.pack expr
  case etree of
    Left _     -> pure . SimpleStr $ "no parse for " <> expr
    Right tree -> do eid <- fromTree myCost tree
                     run (Optimize eid 100 argOpt)

run (Subtrees eid) = do
   isValid <- gets ((IntMap.member eid) . _eClass)
   if isValid
     then do ids <- getAllChildBestEClasses eid
             pure . MultiExprs $ ids
             --printSimpleMultiExprs isCLI ids
     else pure . SimpleStr $ "Invalid id."

run (Pareto crit) = do
   maxSize <- gets (fst . IntMap.findMax . _sizeFitDB . _eDB)
   ecs <- case crit of
            ByFitness -> getParetoEcsUpTo 1 maxSize
            ByDL      -> getParetoDLEcsUpTo 1 maxSize
   pure . MultiExprs $ ecs
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
  pats <- getAllPatterns (const True) eid
  pure . Counts $ Prelude.map (\(p, c) -> (p, (c, 0))) $ Map.toList pats

run (EqSatStep dataInfo) = do forM rewrites $ \r -> runEqSat myCost [r] 5 >> refitChanged dataInfo
                              pure NoPrint
-- dataInfo = (dist, trainDatas, testData)
--  runEqSat myCost rewrites 1

-- * auxiliary functions
importCSV :: Distribution -> String -> String -> Bool -> MyEGraph ()
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
                           insertFitness eid f $ [MA.fromList MA.Seq theta]
                           runEqSat myCost rewritesParams 1
                           cleanDB


parseCSV :: Distribution -> String -> String -> Bool -> IO EGraph
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
                           insertFitness eid f $ [MA.fromList MA.Seq theta]
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
      isNew ec (e, en) = ec `Prelude.elem` (childrenOf en) && (e `IntSet.notMember` visited)
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
      Var ix     -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Var ix), 1)]
      Param ix   -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Param ix), 1)]
      Const x    -> pure $ Map.fromList [(VarPat 'A', 1), (Fixed (Const x), 1)]
      Uni f t    -> do pats <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz t
                       pure $ Map.insertWith (+) (VarPat 'A') 1 
                            $ Map.mapKeysWith (+) (\t' -> Fixed (Uni f t')) pats
      Bin op l r | l==r -> do pats <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz l
                              pure $ Map.insertWith (+) (VarPat 'A') 1 $ Map.mapKeysWith (+) (\t' -> Fixed (Bin op t' t')) pats
                  | otherwise -> do patsL <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz l
                                    patsR <- Map.filterWithKey (\k _ -> (pSz . lenPat) k) <$> getAllPatterns pSz r
                                    pure $ Map.fromList $ (VarPat 'A', 1) : [(relabelVarPat $ Fixed (Bin op l' r'), min vl vr) | (l', vl) <- Map.toList patsL, (r', vr) <- Map.toList patsR]

getAllTokens :: Monad m => EClassId -> EGraphST m (Map.Map Pattern Int)
getAllTokens eid = do
  eid' <- canonical eid
  best <- gets (_best . _info . (IntMap.! eid') . _eClass)
  case best of
    Var ix -> pure $ Map.singleton (Fixed (Var ix)) 1
    Param ix -> pure $ Map.singleton (Fixed (Param ix)) 1
    Const x -> pure $ Map.singleton (Fixed (Const x)) 1
    Uni f t -> do pats <- getAllTokens t
                  pure $ Map.insertWith (+) (Fixed (Uni f (VarPat 'A'))) 1 pats
    Bin op l r -> do patsL <- getAllTokens l
                     patsR <- getAllTokens r
                     pure $ Map.insertWith (+) (Fixed (Bin op (VarPat 'A') (VarPat 'B'))) 1
                          $ Map.unionWith (+) patsL patsR

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
  ids <- gets (_refits . _eDB) >>= Prelude.mapM canonical . Set.toList >>= pure . nub
  modify' $ over (eDB . refits) (const Set.empty)
  forM_ ids $ \ec -> do t <- relabelParams <$> getBestExpr ec
                        let dataTrainsVals = Prelude.zip trainDatas testData
                        response <- forM dataTrainsVals $ \(dt, dv) -> fitnessFunRep 100 dist dt t
                        let f = Prelude.minimum (Prelude.map fst response)
                            thetas = Prelude.map snd response
                        insertFitness ec f thetas
                        let mdl_train  = Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdl dist mYErr x y theta t) $ Prelude.zip thetas trainDatas
                        insertDL ec mdl_train
