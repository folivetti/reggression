{-# LANGUAGE  BlockArguments #-}
{-# LANGUAGE  TupleSections #-}

module Util where

import Control.Lens ( over )

import qualified Data.Map.Strict as Map
import Data.Massiv.Array as MA hiding (forM_, forM)
import Data.SRTree
import Data.SRTree.Eval
import Algorithm.SRTree.Opt
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat ( recalculateBest )
import qualified Data.Sequence as FingerTree

import Algorithm.SRTree.NonlinearOpt
import System.Random
import Data.SRTree.Random hiding (randomVec,randomRange)
import Algorithm.SRTree.Likelihoods
import Algorithm.SRTree.ModelSelection
import Data.SRTree.Print
--import Algorithm.SRTree.ModelSelection
--import Algorithm.SRTree.Opt
import qualified Data.IntMap.Strict as IM
import Control.Monad.State.Strict
import Control.Monad ( when, replicateM, forM, forM_ )
import Data.Maybe ( fromJust )
import Data.List ( maximumBy, intercalate )
import Data.Function ( on )
import List.Shuffle ( shuffle )
import Data.List.Split ( splitOn )
import Data.Char ( toLower )
import qualified Data.IntSet as IntSet
import Data.SRTree.Datasets
import Algorithm.EqSat.Queries
import Algorithm.EqSat.DB
import Data.List (nub)
import Data.SRTree.Recursion (Fix (..), cata)

import Algorithm.EqSat.SearchSR hiding (fitnessFun, fitnessFunRep, io)
import System.Console.Repline hiding (Repl)
import Text.Printf
import Text.Layout.Table hiding (top)
import Text.Layout.Table.Cell.Formatted
import Text.Layout.Table.Cell
import System.Console.ANSI.Codes

data Info = Info {_training :: DataSet, _test :: DataSet, _dist :: Distribution}
type MyEGraph = StateT EGraph IO
--type Repl = HaskelineT (StateT EGraph (StateT StdGen IO))
type Repl = HaskelineT (StateT EGraph IO)

io = lift 

maxVar = cata alg
  where
    alg (Var ix) = ix
    alg (Param _) = 0
    alg (Const _) = 0
    alg (Bin _ l r) = max l r
    alg (Uni _ t) = t

fitnessFun :: Int -> Distribution -> DataSet -> Fix SRTree -> PVector -> (Double, PVector)
fitnessFun nIter distribution (x, y, mYErr) _tree thetaOrig =
  if n <= nVars || isNaN tr
    then (-(1/0), thetaOrig) -- infinity
    else (tr, theta)
  where
    (Sz2 _ n)     = MA.size x
    nVars         = maxVar _tree
    tree          = relabelParams _tree
    nParams       = countParams tree + if distribution == ROXY then 3 else if distribution == Gaussian then 1 else 0
    (theta, _, _) = minimizeNLL' VAR1 distribution mYErr nIter x y tree thetaOrig
    evalF a b c   = negate $ nll distribution c a b tree $ if nParams == 0 then thetaOrig else theta
    tr            = evalF x y mYErr



{-# INLINE fitnessFun #-}

randomRange :: (Ord val, Random val) => (val, val) -> IO val
randomRange rng = (randomRIO rng)
{-# INLINE randomRange #-}

randomVec :: Int -> IO PVector
randomVec n = MA.fromList compMode <$> replicateM n (randomRange (-1, 1))

fitnessFunRep :: Int -> Distribution -> DataSet -> Fix SRTree -> MyEGraph (Double, PVector)
fitnessFunRep nIter distribution dataTrain _tree = do
    let tree = relabelParams _tree
        nParams = countParams tree + if distribution == ROXY then 3 else if distribution == Gaussian then 1 else 0

    thetaOrigs <- lift (randomVec nParams)
    --lift $ print thetaOrigs
    pure (fitnessFun nIter distribution dataTrain tree thetaOrigs)
{-# INLINE fitnessFunRep #-}

mvFun fun thetas datasets = Prelude.map (\(theta, (x,y,e)) -> fun x y e theta)
                          $ Prelude.zip thetas datasets

bold s = formatted (setSGRCode [SetConsoleIntensity BoldIntensity]) (plain s) (setSGRCode [Reset])

printExpr :: [DataSet] -> [DataSet] -> Distribution -> EClassId -> MyEGraph String
printExpr dataTrain dataTest distribution ec = do
        thetas <- getTheta ec

        bestExpr <- getBestExpr ec
        let --(x, y, mYErr) = dataTrain
            --(x_te, y_te, mYErr_te) = dataTest
            best'       = relabelParams bestExpr
            showFun     = show

            mseMV x y e theta = showFun $ mse x y best' theta
            r2MV  x y e theta = showFun $ r2 x y best' theta
            nllMV x y e theta = showFun $ nll distribution e x y best' theta
            mdlMV x y e theta = showFun $ mdl distribution e x y theta best'

            -- expr        = paramsToConst (MA.toList theta) best'
            mse_trains  = intercalate "; " $ mvFun mseMV thetas dataTrain
            mse_tes     = intercalate "; " $ mvFun mseMV thetas dataTest
            r2_trains   = intercalate "; " $ mvFun r2MV thetas dataTrain
            r2_tes      = intercalate "; " $ mvFun r2MV thetas dataTest
            nll_trains  = intercalate "; " $ mvFun nllMV thetas dataTrain
            nll_tes     = intercalate "; " $ mvFun nllMV thetas dataTest
            mdl_trains  = intercalate "; " $ mvFun mdlMV thetas dataTrain
            mdl_tes     = intercalate "; " $ mvFun mdlMV thetas dataTest
            thetaStr    = intercalate "; " $ Prelude.map (intercalate ", " . Prelude.map show . MA.toList) thetas
        insertDL ec $ Prelude.maximum $ Prelude.map (\(theta, (x, y, mYerr)) -> mdl distribution mYerr x y theta best') $ Prelude.zip thetas dataTrain

        pure $ "Info,Training,Test\n"
               <> "Expr," <> showExpr best' <> ",\n"
               <> "Numpy,\"" <> showPython best' <> "\",\n"
               <> "Nodes," <> show (countNodes $ convertProtectedOps best') <> ",\n"
               <> "params," <>  thetaStr <> ",\n"
               <> intercalate "," ["MSE", mse_trains, mse_tes] <> "\n"
               <> intercalate "," ["R^2", r2_trains, r2_tes] <> "\n"
               <> intercalate "," ["nll", nll_trains, nll_tes] <> "\n"
               <> intercalate "," ["DL",  mdl_trains, mdl_tes] <> "\n"

printExprCLI dataTrain dataTest distribution ec = do
        thetas <- getTheta ec

        bestExpr <- getBestExpr ec
        let --(x, y, mYErr) = dataTrain
            --(x_te, y_te, mYErr_te) = dataTest
            best'       = relabelParams bestExpr
            showFun     = printf "%.4e"

            mseMV x y e theta = showFun $ mse x y best' theta
            r2MV  x y e theta = showFun $ r2 x y best' theta
            nllMV x y e theta = showFun $ nll distribution e x y best' theta
            mdlMV x y e theta = showFun $ mdl distribution e x y theta best'

            -- expr        = paramsToConst (MA.toList theta) best'
            mse_trains  = intercalate "; " $ mvFun mseMV thetas dataTrain
            mse_tes     = intercalate "; " $ mvFun mseMV thetas dataTest
            r2_trains   = intercalate "; " $ mvFun r2MV thetas dataTrain
            r2_tes      = intercalate "; " $ mvFun r2MV thetas dataTest
            nll_trains  = intercalate "; " $ mvFun nllMV thetas dataTrain
            nll_tes     = intercalate "; " $ mvFun nllMV thetas dataTest
            mdl_trains  = intercalate "; " $ mvFun mdlMV thetas dataTrain
            mdl_tes     = intercalate "; " $ mvFun mdlMV thetas dataTest
            thetaStr    = intercalate "; " $ Prelude.map (intercalate ", " . Prelude.map show . MA.toList) thetas
        insertDL ec $ Prelude.maximum $ Prelude.map (\(theta, (x, y, mYerr)) -> mdl distribution mYerr x y theta best') $ Prelude.zip thetas dataTrain


        io . putStr $ "Evaluation metrics for expression (" <> (show ec) <> "): "
        io . putStr $ setSGRCode [SetConsoleIntensity BoldIntensity]
        io . putStrLn $ showExpr best'
        io . putStr $ setSGRCode [Reset]
        io . putStrLn $ "# of nodes\t" <> show (countNodes $ convertProtectedOps best')
        io . putStrLn $ "params:\t[" <> thetaStr <> "]"

        let rows = [ rowG ["MSE", mse_trains, mse_tes]
                   , rowG ["R^2", r2_trains, r2_tes]
                   , rowG ["nll", nll_trains, nll_tes]
                   , rowG ["DL",  mdl_trains, mdl_tes]
                   ]
            columnsReport = [def, numCol, numCol]
            headerReport = titlesH $ Prelude.map bold ["Metric", "Training", "Test"]
        io . putStrLn $ tableString (columnHeaderTableS columnsReport unicodeS headerReport rows)

printsimpleExpr eid = do
   let showFun = show
   t   <- relabelParams <$> getBestExpr eid
   fit <- getFitness eid
   sz  <- getSize eid
   p   <- getTheta eid
   dl  <- getDL eid
   let fit' = case fit of
                Nothing -> "NA"
                Just f  -> showFun f
       p' = case p of
              [] -> "NA"
              pss -> intercalate "|" $ Prelude.map (\ps -> "[" <> intercalate ", " (Prelude.map show (MA.toList ps)) <> "]") pss
       dl' = case dl of
              Nothing -> "NA"
              Just d  -> showFun d
   pure $ intercalate "," [show eid, showExpr t, "\"" <> showPython t <> "\"", fit', "\"" <> p' <> "\"", show sz, dl']

printsimpleExprCLI eid = do
   let showFun = printf "%.4e"
   t   <- relabelParams <$> getBestExpr eid
   fit <- getFitness eid
   sz  <- getSize eid
   p   <- getTheta eid
   dl  <- getDL eid
   let fit' = case fit of
                Nothing -> "NA"
                Just f  -> showFun f
       p' = case p of
              [] -> "NA"
              pss -> intercalate "|" $ Prelude.map (\ps -> "[" <> intercalate ", " (Prelude.map show (MA.toList ps)) <> "]") pss
       dl' = case dl of
              Nothing -> "NA"
              Just d  -> showFun d
   pure $ colsAllG center [[show eid], justifyText 50 $ showExpr t, [fit'], justifyText 50 p', [show sz], [dl']]

printCounts (pat, (cnt, avgfit)) = do
  let spat = showPat pat
  pure $ intercalate "," [spat, show cnt, show avgfit]
  where
    showPat (Fixed (Var ix)) = 'x' : show ix
    showPat (Fixed (Param ix)) = 't' : show ix
    showPat (Fixed (Const x))  = show x
    showPat (Fixed (Bin op l r)) = concat ["(", showPat l, " ", showOp op, " ", showPat r, ")"]
    showPat (Fixed (Uni f t)) = concat [show f, "(", showPat t, ")"]
    showPat (VarPat ix) = 'v' : show (fromEnum ix-65)

printCountsCLI (pat, (cnt, avgfit)) = do
  let spat = showPat pat
  pure $ colsAllG center [justifyText 50 spat, [show cnt], [printf "%.4e" avgfit]]
  where
    showPat (Fixed (Var ix)) = 'x' : show ix
    showPat (Fixed (Param ix)) = 't' : show ix
    showPat (Fixed (Const x))  = show x
    showPat (Fixed (Bin op l r)) = concat ["(", showPat l, " ", showOp op, " ", showPat r, ")"]
    showPat (Fixed (Uni f t)) = concat [show f, "(", showPat t, ")"]
    showPat (VarPat ix) = 'v' : show (fromEnum ix-65)

printSimpleMultiExprs eids =
  do rows <- forM (nub eids) printsimpleExpr
     pure . intercalate "\n" $ (headerSimple:rows)

printSimpleMultiExprsCLI eids =
  do rows <- forM (nub eids) printsimpleExprCLI
     io.putStrLn $ tableString (columnHeaderTableS columns unicodeS headerSimpleCLI rows)

printMultiCounts cnts =
  do rows <- forM cnts printCounts
     pure . intercalate "\n" $ (headerCount:rows)

printMultiCountsCLI cnts =
  do rows <- forM cnts printCountsCLI
     io.putStrLn $ tableString (columnHeaderTableS [fixedLeftCol 50, numCol, numCol] unicodeS headerCountCLI rows)


headerSimple = intercalate "," ["Id", "Expression", "Numpy", "Fitness", "Parameters", "Size", "DL"]
headerCount = intercalate "," ["Pattern", "Count", "AvgFit"]

headerSimpleCLI :: HeaderSpec LineStyle (Formatted String)
headerSimpleCLI = titlesH $ Prelude.map (bold) ["Id", "Expression", "Fitness", "Parameters", "Size", "DL"]
columns = [numCol, fixedLeftCol 50, numCol, fixedLeftCol 50, numCol, numCol]
headerCountCLI :: HeaderSpec LineStyle (Formatted String)
headerCountCLI = titlesH $ Prelude.map bold ["Pattern", "Count", "Avg. Fitness"]


fillDL dist datasets = do
  ecs <- getAllEvaluatedEClasses
  let (x', _, _) = head datasets
      (Sz2 _ n)     = MA.size x'
  forM_ ecs $ \ec -> do
    thetas <- getTheta ec
    bestExpr <- relabelParams <$> getBestExpr ec
    let nVars = maxVar bestExpr
    if MA.size (head thetas) /= countParams bestExpr || n <= nVars
       then (lift . putStrLn) $ "Wrong number of parameters in " <> showExpr bestExpr <> ": " <> show (head thetas) <> "   " <> show ec
       else do let mdl_trains = Prelude.map (\(theta, (x, y, mYerr)) -> mdl dist mYerr x y theta bestExpr) $ Prelude.zip thetas datasets
               insertDL ec $ Prelude.maximum mdl_trains

fillFit dist trainDatas = do
  ecs <- getAllEvaluatedEClasses
  cleanAllDBs
  let (x', _, _) = head trainDatas
      (Sz2 _ n)     = MA.size x'
  forM_ ecs $ \ec -> do
    unsetFitness ec
    t <- relabelParams <$> getBestExpr ec
    let nVars = maxVar t
    response <- forM trainDatas $ \dt -> if n <= nVars then pure (-1.0/0.0, MA.fromList MA.Seq []) else fitnessFunRep 50 dist dt t
    let f      = Prelude.minimum (Prelude.map fst response)
        thetas = Prelude.map snd response
    insertFitness ec f thetas
    let mdl_train  = if isInfinite f then (1.0/0.0) else Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdl dist mYErr x y theta t) $ Prelude.zip thetas trainDatas
    insertDL ec mdl_train


cleanAllDBs = do
  modify' $ over (eDB . fitRangeDB) (const FingerTree.Empty)
          . over (eDB . sizeFitDB) (const IM.empty)
          . over (eDB . dlRangeDB) (const FingerTree.Empty)
          . over (eDB . sizeDLDB) (const IM.empty)

unsetFitness :: Monad m => EClassId -> EGraphST m ()
unsetFitness eId = do
  --eId <- canonical eId'
  ec <- gets ((IM.! eId) . _eClass)
  let newInfo = (_info ec){_fitness = Nothing}
      newEc   = ec{_info = newInfo}
  modify' $ over eClass (IM.insert eId newEc)
