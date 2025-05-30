{-# LANGUAGE  BlockArguments #-}
{-# LANGUAGE  TupleSections #-}

module Util where

import qualified Data.Map.Strict as Map
import Data.Massiv.Array as MA hiding (forM_, forM)
import Data.SRTree
import Data.SRTree.Eval
import Algorithm.SRTree.Opt
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat ( recalculateBest )

import Algorithm.SRTree.NonlinearOpt
import System.Random
import Random
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

type RndEGraph = StateT EGraph IO
type DataSet = (SRMatrix, PVector, Maybe PVector)
data Info = Info {_training :: DataSet, _test :: DataSet, _dist :: Distribution}

csvHeader :: String
csvHeader = "id,Expression,theta,size,MSE_train,MSE_val,MSE_test,R2_train,R2_val,R2_test,nll_train,nll_val,nll_test,mdl_train,mdl_val,mdl_test"

--io :: IO a -> Repl a
io :: IO a -> RndEGraph a
io = lift
{-# INLINE io #-}
--egraph :: RndEGraph a -> Repl a
--egraph = lift

myCost :: SRTree Int -> Int
myCost (Var _)     = 1
myCost (Const _)   = 1
myCost (Param _)   = 1
myCost (Bin _ l r) = 2 + l + r
myCost (Uni _ t)   = 3 + t

fitnessFun :: Int -> Distribution -> DataSet -> Fix SRTree -> PVector -> (Double, PVector)
fitnessFun nIter distribution (x, y, mYErr) _tree thetaOrig =
  if isNaN tr
    then (-(1/0), theta) -- infinity
    else (tr, theta)
  where
    tree          = relabelParams _tree
    nParams       = countParams tree + if distribution == ROXY then 3 else if distribution == Gaussian then 1 else 0
    (theta, _, _) = minimizeNLL' VAR1 distribution mYErr nIter x y tree thetaOrig
    evalF a b c   = negate $ nll distribution c a b tree $ if nParams == 0 then thetaOrig else theta
    tr            = evalF x y mYErr

{-# INLINE fitnessFun #-}

fitnessFunRep :: Int -> Distribution -> DataSet -> Fix SRTree -> RndEGraph (Double, PVector)
fitnessFunRep nIter distribution dataTrain _tree = do
    let tree = relabelParams _tree
        nParams = countParams tree + if distribution == ROXY then 3 else if distribution == Gaussian then 1 else 0

    thetaOrigs <- lift (randomVec nParams)
    --lift $ print thetaOrigs
    pure (fitnessFun nIter distribution dataTrain tree thetaOrigs)
{-# INLINE fitnessFunRep #-}

fitnessMV :: Int -> Distribution -> [DataSet] -> Fix SRTree -> RndEGraph (Double, [PVector])
fitnessMV nIter distribution dataTrainsVals _tree = do
  response <- forM dataTrainsVals $ \dt -> fitnessFunRep nIter distribution dt _tree
  pure (minimum (Prelude.map fst response), Prelude.map snd response)


-- helper query functions
-- TODO: move to egraph lib
getFitness :: EClassId -> RndEGraph (Maybe Double)
getFitness c = gets (_fitness . _info . (IM.! c) . _eClass)
{-# INLINE getFitness #-}
getTheta :: EClassId -> RndEGraph ([PVector])
getTheta c = gets (_theta . _info . (IM.! c) . _eClass)
{-# INLINE getTheta #-}
getSize :: EClassId -> RndEGraph Int
getSize c = gets (_size . _info . (IM.! c) . _eClass)
{-# INLINE getSize #-}
isSizeOf :: (Int -> Bool) -> EClass -> Bool
isSizeOf p = p . _size . _info
{-# INLINE isSizeOf #-}
getDL :: EClassId -> RndEGraph (Maybe Double)
getDL c = gets (_dl . _info . (IM.! c) . _eClass)
{-# INLINE getDL #-}

getBestFitness :: RndEGraph (Maybe Double)
getBestFitness = do
    bec <- (gets (snd . getGreatest . _fitRangeDB . _eDB) >>= canonical)
    gets (_fitness . _info . (IM.! bec) . _eClass)

getTrain :: ((a, b1, c1, d1), (c2, b2), c3, d2) -> ((a, b1, c2), c3)
getTrain ((a, b, _, _), (c, _), varnames, _) = ((a,b,c), varnames)

getX :: DataSet -> SRMatrix
getX (a, _, _) = a

getTarget :: DataSet -> PVector
getTarget (_, b, _) = b

getError :: DataSet -> Maybe PVector
getError (_, _, c) = c

loadTrainingOnly fname b = getTrain <$> loadDataset fname b

mvFun fun thetas datasets = Prelude.map (\(theta, (x,y,e)) -> fun x y e theta)
                          $ Prelude.zip thetas datasets

printExpr :: [DataSet] -> [DataSet] -> Distribution -> EClassId -> RndEGraph String
printExpr dataTrain dataTest distribution ec = do
        thetas <- getTheta ec

        bestExpr <- getBestExpr ec
        let --(x, y, mYErr) = dataTrain
            --(x_te, y_te, mYErr_te) = dataTest
            best'       = relabelParams bestExpr

            mseMV x y e theta = show $ mse x y best' theta
            r2MV  x y e theta = show $ r2 x y best' theta
            nllMV x y e theta = show $ nll distribution e x y best' theta
            mdlMV x y e theta = show $ mdl distribution e x y theta best'

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
               <> "Nodes," <> show (countNodes $ convertProtectedOps best') <> ",\n"
               <> "params," <>  thetaStr <> ",\n"
               <> intercalate "," ["MSE", mse_trains, mse_tes] <> "\n"
               <> intercalate "," ["R^2", r2_trains, r2_tes] <> "\n"
               <> intercalate "," ["nll", nll_trains, nll_tes] <> "\n"
               <> intercalate "," ["DL",  mdl_trains, mdl_tes] <> "\n"

printsimpleExpr eid = do t   <- relabelParams <$> getBestExpr eid
                         fit <- getFitness eid
                         sz  <- getSize eid
                         p   <- getTheta eid
                         dl  <- getDL eid
                         let fit' = case fit of
                                      Nothing -> "NA"
                                      Just f  -> show f
                             p' = case p of
                                    [] -> "NA"
                                    pss -> intercalate "|" $ Prelude.map (\ps -> "[" <> intercalate ", " (Prelude.map show (MA.toList ps)) <> "]") pss
                             dl' = case dl of
                                    Nothing -> "NA"
                                    Just d  -> show d

                         pure $ intercalate "," [show eid, showExpr t, fit', "\"" <> p' <> "\"", show sz, dl']

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

printSimpleMultiExprs eids = do rows <- forM (nub eids) printsimpleExpr
                                pure . intercalate "\n" $ (headerSimple:rows)

printMultiCounts cnts = do rows <- forM cnts printCounts
                           pure . intercalate "\n" $ (headerCount:rows)

headerSimple = intercalate "," ["Id", "Expression", "Fitness", "Parameters", "Size", "DL"]
headerCount = intercalate "," ["Pattern", "Count", "AvgFit"]

-- RndEGraph utils
-- fitFun fitnessFunRep rep iter distribution x y mYErr x_val y_val mYErr_val
insertExpr :: Fix SRTree -> (Fix SRTree -> RndEGraph (Double, [PVector])) -> RndEGraph EClassId
insertExpr t fitFun = do
    ecId <- fromTree myCost t >>= canonical
    (f, p) <- fitFun t
    insertFitness ecId f p
    pure ecId
  where powabs l r  = Fix (Bin PowerAbs l r)

updateIfNothing fitFun ec = do
      mf <- getFitness ec
      case mf of
        Nothing -> do
          t <- getBestExpr ec
          (f, p) <- fitFun t
          insertFitness ec f p
          pure True
        Just _ -> pure False

-- getParetoEcsUpTo b n maxSize = concat <$> forM [1..maxSize] (\i -> getTopEClassWithSize b i 1)

getParetoEcsUpTo b maxSize = go 1 (if b then -(1.0/0.0) else (1.0/0.0))
    where
    go :: Int -> Double -> RndEGraph [Int]
    go n f
        | n > maxSize = pure []
        | otherwise   = do
            ecList <- getTopEClassWithSize b n 1
            if not (null ecList)
                then do let ec = head ecList
                        ec' <- canonical ec
                        f' <- fromJust <$> (if b then getFitness ec' else getDL ec')
                        if (if b then f' > f else f' < f) && (not . isNaN) f' && (not . isInfinite) f'
                          then do ts <- go (n+1) f'
                                  --io $ print (f', f)
                                  pure (ec':ts)
                          else go (n+1) f
                else go (n+1) f

getBestExprWithSize n =
        do ec <- getTopFitEClassWithSize n 1 >>= traverse canonical
           if (not (null ec))
            then do
              bestFit <- getFitness $ head ec
              bestP   <- gets (_theta . _info . (IM.! (head ec)) . _eClass)
              (:[]) . (,bestP) . (,bestFit) . (,ec) <$> getBestExpr (head ec)
            else pure []

printBest :: (Int -> EClassId -> RndEGraph ()) -> RndEGraph ()
printBest printExprFun = do
      bec <- gets (snd . getGreatest . _fitRangeDB . _eDB) >>= canonical
      printExprFun 0 bec

paretoFront :: Int -> (Int -> EClassId -> RndEGraph ()) -> RndEGraph ()
paretoFront maxSize printExprFun = go 1 0 (-(1.0/0.0))
    where
    go :: Int -> Int -> Double -> RndEGraph ()
    go n ix f
        | n > maxSize = pure ()
        | otherwise   = do
            ecList <- getBestExprWithSize n
            if not (null ecList)
                then do let (((_, ec), mf), _) = head ecList
                            improved = fromJust mf > f
                        ec' <- traverse canonical ec
                        when improved $ printExprFun ix (head ec')
                        go (n+1) (ix + if improved then 1 else 0) (max f (fromJust mf))
                else go (n+1) ix f

evaluateUnevaluated fitFun = do
          ec <- gets (IntSet.toList . _unevaluated . _eDB)
          forM_ ec $ \c -> do
              t <- getBestExpr c
              (f, p) <- fitFun t
              insertFitness c f p

fillDL dist datasets = do
  ecs <- getAllEvaluatedEClasses
  forM_ ecs $ \ec -> do
    thetas <- getTheta ec
    bestExpr <- relabelParams <$> getBestExpr ec
    if MA.size (head thetas) /= countParams bestExpr
       then (lift . putStrLn) $ "Wrong number of parameters in " <> showExpr bestExpr <> ": " <> show (head thetas) <> "   " <> show ec
       else do let mdl_trains = Prelude.map (\(theta, (x, y, mYerr)) -> mdl dist mYerr x y theta bestExpr) $ Prelude.zip thetas datasets
               insertDL ec $ Prelude.maximum mdl_trains
