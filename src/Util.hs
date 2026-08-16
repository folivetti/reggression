{-# LANGUAGE  BlockArguments #-}
{-# LANGUAGE  TupleSections #-}

module Util where

import Control.Lens ( over )

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.SRTree
import Data.SRTree.Eval
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat ( recalculateBest )
import qualified Data.Set as Set

import Algorithm.SRTree.NonlinearOpt
import Algorithm.SRTree.AD ( ADBackEnd (..) )
import Numeric.Optimization.NLOPT ( LocalAlgorithm ( VAR1 ) )
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

data Info = Info {_training :: DataSet, _test :: DataSet, _dist :: Loss}
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

fitnessFun :: Int -> Loss -> DataSet -> Fix SRTree -> Target -> (Double, Target)
fitnessFun nIter loss (x, y, mYErr) _tree thetaOrig =
  if n <= nVars || isNaN tr
    then (-(1/0), thetaOrig) -- infinity
    else (tr, theta)
  where
    n         = length x
    nVars     = maxVar _tree
    tree      = relabelParams _tree
    nParams   = countParams tree + if loss == NLL ROXY then 3 else if loss == NLL Gaussian then 1 else 0
    (theta, _, _) = minimizeNLL' VAR1 MultiThread loss mYErr nIter x y tree thetaOrig
    evalF a b c   = negate $ compileLoss a (buildLoss loss (fromIntegral (VU.length b)) tree) b c $ if nParams == 0 then thetaOrig else theta
    tr            = evalF x y mYErr



{-# INLINE fitnessFun #-}

randomRange :: (Ord val, Random val) => (val, val) -> IO val
randomRange rng = (randomRIO rng)
{-# INLINE randomRange #-}

randomVec :: Int -> IO Target
randomVec n = VU.replicateM n (randomRange (-1, 1))

fitnessFunRep :: Int -> Loss -> DataSet -> Fix SRTree -> MyEGraph (Double, Target)
fitnessFunRep nIter loss dataTrain _tree = do
    let tree = relabelParams _tree
        nParams = countParams tree + if loss == NLL ROXY then 3 else if loss == NLL Gaussian then 1 else 0

    thetaOrigs <- lift (randomVec nParams)
    --lift $ print thetaOrigs
    pure (fitnessFun nIter loss dataTrain tree thetaOrigs)
{-# INLINE fitnessFunRep #-}

-- | `createDB` creates a database of patterns from the e-nodes of the e-graph
createDB :: ClassStore m => EGraphST m DB
createDB = do modify' $ over (eDB . patDB) (const Map.empty)
              ecls <- gets (HM.toList . _eNodeToEClass)
              mapM_ (uncurry addToDB) ecls
              gets (_patDB . _eDB)
{-# INLINE createDB #-}

-- | `createDBBest` creates a database of patterns from the best e-node of
-- every e-class
createDBBest :: ClassStore m => EGraphST m DB
createDBBest = do modify' $ over (eDB . patDB) (const Map.empty)
                  ecls <- allClasses
                  mapM_ (\ec -> addToDB (_best (_info ec)) (_eClassId ec)) ecls
                  gets (_patDB . _eDB)
{-# INLINE createDBBest #-}

-- | mean squared error of a fitted model
mseMetric :: Columns -> Target -> Fix SRTree -> Target -> Double
mseMetric xss ys tree theta =
  compileLoss xss (buildLoss MSE (fromIntegral n) tree) ys Nothing theta
  where n = VU.length ys
{-# INLINE mseMetric #-}

-- | coefficient of determination of a fitted model
r2Metric :: Columns -> Target -> Fix SRTree -> Target -> Double
r2Metric xss ys tree theta = 1 - sse / sseTot
  where
    m      = VU.length ys
    sse    = fromIntegral m * mseMetric xss ys tree theta
    ym     = VU.sum ys / fromIntegral m
    sseTot = VU.sum (VU.map (\yi -> (yi - ym) ^ (2 :: Int)) ys)
{-# INLINE r2Metric #-}

-- | negative log-likelihood (or raw loss for non-NLL 'Loss') of a fitted model
nllMetric :: Loss -> Maybe Target -> Columns -> Target -> Fix SRTree -> Target -> Double
nllMetric loss mYerr xss ys tree theta =
  compileLoss xss (buildLoss loss (fromIntegral n) tree) ys mYerr theta
  where n = VU.length ys
{-# INLINE nllMetric #-}

-- | MDL criterion of a fitted model
mdlMetric :: Loss -> Maybe Target -> Columns -> Target -> Target -> Fix SRTree -> Double
mdlMetric loss mYerr xss ys theta tree =
  nllMetric loss mYerr xss ys tree theta + logFunctional tree + logParameters fisher theta
  where
    dist   = case loss of NLL d -> d; _ -> LeastSquares
    fisher = fisherNLL dist mYerr xss ys tree theta
{-# INLINE mdlMetric #-}

mvFun fun thetas datasets = Prelude.map (\(theta, (x,y,e)) -> fun x y e theta)
                          $ Prelude.zip thetas datasets

bold s = formatted (setSGRCode [SetConsoleIntensity BoldIntensity]) (plain s) (setSGRCode [Reset])

printExpr :: [String] -> [DataSet] -> [DataSet] -> Loss -> EClassId -> MyEGraph String
printExpr varnames dataTrain dataTest loss ec = do
        cec <- canonical ec
        thetas <- getTheta ec

        bestExpr <- getBestExpr ec
        let --(x, y, mYErr) = dataTrain
            --(x_te, y_te, mYErr_te) = dataTest
            best'       = relabelParams bestExpr
            showFun     = show

            mseMV x y e theta = showFun $ mseMetric x y best' theta
            r2MV  x y e theta = showFun $ r2Metric x y best' theta
            nllMV x y e theta = showFun $ nllMetric loss e x y best' theta
            mdlMV x y e theta = showFun $ mdlMetric loss e x y theta best'

            mse_trains  = intercalate "; " $ mvFun mseMV thetas dataTrain
            mse_tes     = intercalate "; " $ mvFun mseMV thetas dataTest
            r2_trains   = intercalate "; " $ mvFun r2MV thetas dataTrain
            r2_tes      = intercalate "; " $ mvFun r2MV thetas dataTest
            nll_trains  = intercalate "; " $ mvFun nllMV thetas dataTrain
            nll_tes     = intercalate "; " $ mvFun nllMV thetas dataTest
            mdl_trains  = intercalate "; " $ mvFun mdlMV thetas dataTrain
            mdl_tes     = intercalate "; " $ mvFun mdlMV thetas dataTest
            thetaStr    = intercalate "; " $ Prelude.map (intercalate ", " . Prelude.map show . VU.toList) thetas
            showExprFun = if null varnames then showExpr else showExprWithVars varnames
        insertDL ec $ Prelude.maximum $ Prelude.map (\(theta, (x, y, mYerr)) -> mdlMetric loss mYerr x y theta best') $ Prelude.zip thetas dataTrain

        pure $ "Info,Training,Test\n"
               <> "Id," <> show cec <> ",\n"
               <> "Expr," <> showExprFun best' <> ",\n"
               <> "Numpy,\"" <> showPython best' <> "\",\n"
               <> "Nodes," <> show (countNodes $ convertProtectedOps best') <> ",\n"
               <> "params," <>  thetaStr <> ",\n"
               <> intercalate "," ["MSE", mse_trains, mse_tes] <> "\n"
               <> intercalate "," ["R^2", r2_trains, r2_tes] <> "\n"
               <> intercalate "," ["nll", nll_trains, nll_tes] <> "\n"
               <> intercalate "," ["DL",  mdl_trains, mdl_tes] <> "\n"

printExprCLI dataTrain dataTest loss ec = do
        thetas <- getTheta ec

        bestExpr <- getBestExpr ec
        let --(x, y, mYErr) = dataTrain
            --(x_te, y_te, mYErr_te) = dataTest
            best'       = relabelParams bestExpr
            showFun     = printf "%.4e"

            mseMV x y e theta = showFun $ mseMetric x y best' theta
            r2MV  x y e theta = showFun $ r2Metric x y best' theta
            nllMV x y e theta = showFun $ nllMetric loss e x y best' theta
            mdlMV x y e theta = showFun $ mdlMetric loss e x y theta best'

            mse_trains  = intercalate "; " $ mvFun mseMV thetas dataTrain
            mse_tes     = intercalate "; " $ mvFun mseMV thetas dataTest
            r2_trains   = intercalate "; " $ mvFun r2MV thetas dataTrain
            r2_tes      = intercalate "; " $ mvFun r2MV thetas dataTest
            nll_trains  = intercalate "; " $ mvFun nllMV thetas dataTrain
            nll_tes     = intercalate "; " $ mvFun nllMV thetas dataTest
            mdl_trains  = intercalate "; " $ mvFun mdlMV thetas dataTrain
            mdl_tes     = intercalate "; " $ mvFun mdlMV thetas dataTest
            thetaStr    = intercalate "; " $ Prelude.map (intercalate ", " . Prelude.map show . VU.toList) thetas
        insertDL ec $ Prelude.maximum $ Prelude.map (\(theta, (x, y, mYerr)) -> mdlMetric loss mYerr x y theta best') $ Prelude.zip thetas dataTrain


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

printsimpleExpr varnames eid m = do
   let showFun = show
   t   <- relabelParams <$> getBestExpr eid
   mt   <- showModular varnames m eid True
   mts  <- showModules varnames m True
   fit <- getFitness eid
   sz  <- getSize eid
   p   <- getTheta eid
   dl  <- getDL eid
   let eq = "\\begin{align}" <> intercalate " \\\\ " (mt:mts) <> "\\end{align}"
       fit' = case fit of
                Nothing -> "NA"
                Just f  -> showFun f
       p' = case p of
              [] -> "NA"
              pss -> intercalate "|" $ Prelude.map (\ps -> "[" <> intercalate ", " (Prelude.map show (VU.toList ps)) <> "]") pss
       dl' = case dl of
              Nothing -> "NA"
              Just d  -> showFun d
   pure $ intercalate "," [show eid, showExpr t, "\"" <> showPython t <> "\"", "\"" <> eq <> "\"", fit', "\"" <> p' <> "\"", show sz, dl']

printsimpleExprCLI eid m = do
   let showFun = printf "%.4e"
   t   <- relabelParams <$> getBestExpr eid
   mt   <- showModular [] m eid True
   mts  <- showModules [] m True
   fit <- getFitness eid
   sz  <- getSize eid
   p   <- getTheta eid
   dl  <- getDL eid
   let eq = intercalate "\n" (mt:mts)
       fit' = case fit of
                Nothing -> "NA"
                Just f  -> showFun f
       p' = case p of
              [] -> "NA"
              pss -> intercalate "|" $ Prelude.map (\ps -> "[" <> intercalate ", " (Prelude.map show (VU.toList ps)) <> "]") pss
       dl' = case dl of
              Nothing -> "NA"
              Just d  -> showFun d
   pure $ colsAllG center [[show eid], justifyText 50 $ showExpr t, [fit'], justifyText 50 eq, justifyText 50 p', [show sz], [dl']]

printsimpleTree varnames t' = do
  let t = relabelParams t'
  ltx <- showLatexTree varnames t
  let eq = "\\begin{align}" <> ltx <> "\\end{align}"
  pure $ intercalate "," [showExpr t, "\"" <> showPython t <> "\"", "\"" <> eq <> "\""]

printsimpleTreeCLI t' = do
  let t = relabelParams t'
  pure $ colsAllG center [[show 1], justifyText 50 $ showExpr t]

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
    showPat (NAry op ncs) = "(" <> intercalate (" " <> showOp (toOp op) <> " ") (Prelude.map showNChild ncs) <> ")"
    showPat Hole          = "_"
    showNChild (Ch p)     = showPat p
    showNChild (Rest c)   = 'v' : show (fromEnum c-65)
    showNChild (MapP p _) = showPat p

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
    showPat (NAry op ncs) = "(" <> intercalate (" " <> showOp (toOp op) <> " ") (Prelude.map showNChild ncs) <> ")"
    showPat Hole          = "_"
    showNChild (Ch p)     = showPat p
    showNChild (Rest c)   = 'v' : show (fromEnum c-65)
    showNChild (MapP p _) = showPat p

--printSimpleMultiExprs varnames eids =
--  do rows <- forM (nub eids) (uncurry (printsimpleExpr varnames))
--     pure . intercalate "\n" $ (headerSimple:rows)
printSimpleMultiExprs varnames eids =
  do rows <- forM eids (uncurry (printsimpleExpr varnames))
     pure . intercalate "\n" $ (headerSimple:rows)

printSimpleMultiExprsCLI eids =
  do rows <- forM (nub eids) (uncurry printsimpleExprCLI)
     io.putStrLn $ tableString (columnHeaderTableS columns unicodeS headerSimpleCLI rows)

printSimpleMultiTrees varnames ts =
  do rows <- forM ts ((printsimpleTree varnames))
     pure $ intercalate "\n" $ (headerTrees:rows)

printSimpleMultiTreesCLI ts =
  do rows <- forM ts (printsimpleTreeCLI)
     io.putStrLn $ tableString (columnHeaderTableS columnsT unicodeS headerTreesCLI rows)

printMultiCounts cnts =
  do rows <- forM cnts printCounts
     pure . intercalate "\n" $ (headerCount:rows)

printMultiCountsCLI cnts =
  do rows <- forM cnts printCountsCLI
     io.putStrLn $ tableString (columnHeaderTableS [fixedLeftCol 50, numCol, numCol] unicodeS headerCountCLI rows)

printEClasses eids =
  do let rows = Prelude.map show eids -- forM eids Prelude.show
     io $ print eids
     pure . intercalate "\n" $ ("e-classes" : rows)

headerSimple = intercalate "," ["Id", "Expression", "Numpy", "Latex", "Fitness", "Parameters", "Size", "DL"]
headerCount = intercalate "," ["Pattern", "Count", "AvgFit"]
headerTrees = intercalate "," ["Expression", "Numpy", "Latex"]

headerSimpleCLI :: HeaderSpec LineStyle (Formatted String)
headerSimpleCLI = titlesH $ Prelude.map (bold) ["Id", "Expression", "Fitness", "Module", "Parameters", "Size", "DL"]
columns = [numCol, fixedLeftCol 50, numCol, fixedLeftCol 50, fixedLeftCol 50, numCol, numCol]
headerCountCLI :: HeaderSpec LineStyle (Formatted String)
headerCountCLI = titlesH $ Prelude.map bold ["Pattern", "Count", "Avg. Fitness"]

columnsT = [numCol, fixedLeftCol 50]
headerTreesCLI = titlesH $ Prelude.map bold ["Id", "Expression"]

showModules :: ClassStore m => [String] -> IM.IntMap (Int, Int) -> Bool -> EGraphST m [String]
showModules varnames m latex = forM (IM.toList m) showSingleModule
  where
    showSingleModule (eid, (0, ix)) = do s <- showModular varnames (IM.delete eid m) eid latex
                                         pure $ "z_{" <> show ix <> "} = " <> s
    showSingleModule (eid, (_, ix)) = do s <- showModular varnames (IM.delete eid m) eid latex
                                         pure $ "f_{" <> show ix <> "}(" <> (if latex then "\\theta" else "θ") <> ") = " <> s


showModular :: ClassStore m => [String] -> IM.IntMap (Int, Int) -> EClassId -> Bool -> EGraphST m String
showModular varnames mNames eid' latex = fst <$> go eid' 0
  where
    goList [] ix = pure ([], ix)
    goList (c:cs) ix = do (s, ix') <- go c ix
                          (ss, ix'') <- goList cs ix'
                          pure (s:ss, ix'')
    go id' thetaIx = do
      eid <- canonical id'
      let mResult = mNames IM.!? eid
      case mResult of
        Nothing       -> showNormal eid
        Just (ps, ix) -> if ps  == 0
                           then pure $ (if latex then "z_{" <> show ix <> "}" else ('z':show ix)
                                       , thetaIx)
                           else if ps == 1
                                  then pure $ (if latex
                                                then "f_{" <> show ix <> "}(" <> "\\theta_{" <> show thetaIx <> "})"
                                                else "f" <> show ix <> "(θ" <> show thetaIx <> ")"
                                              , thetaIx + 1)
                                  else pure $ (if latex
                                                then "f_{" <> show ix <> "}(" <> "\\theta_{" <> show thetaIx <> "\\ldots" <> show (thetaIx + ps - 1) <> "}" <> ")"
                                                else "f" <> show ix <> "(" <> "θ(" <> show thetaIx <> ".." <> show (thetaIx + ps - 1) <> ")"
                                            , thetaIx + ps)
        where
          showLower x = Prelude.map toLower $ show x
          latexify cs = go cs 0
            where
              go []       n  = Prelude.replicate n '}'
              go ('_':cs) n  = '_' : '{' : go cs (n+1)
              go (c:cs)   n  = c : go cs n

          showVar ix = if latex
                         then if null varnames then ("x_{" <> show ix <> "}") else latexify (varnames !! ix)
                         else if null varnames then ("x" <> show ix) else varnames !! ix
          showParam ix = if latex
                            then "\\theta_{" <> show ix <> "}"
                            else ('θ':show ix)
          showFun g t =
            case g of
              Id -> t
              Abs -> if latex
                        then "\\left|" <> t <> "\\right|"
                        else "|" <> t <> "|"
              Sqrt -> if latex
                         then "\\sqrt{" <> t <> "}"
                         else "sqrt(" <> t <> ")"
              SqrtAbs -> if latex
                            then "\\sqrt{\\left|" <> t <> "\\right|}"
                            else "sqrtabs(" <> t <> ")"
              Cbrt    -> t <> if latex then "^{\\frac{1}{3}}" else "^(1/3)"
              Square  -> t <> "^2"
              Cube    -> t <> "^3"
              LogAbs  -> if latex
                            then "\\log{(\\left|" <> t <> "\\right|)}"
                            else "log(" <> t <> ")"
              Exp     -> if latex
                            then "e^{" <> t <> "}"
                            else "exp(" <> t <> ")"
              Recip   -> if latex
                            then "\\frac{1}{" <> t <> "}"
                            else "(1/" <> t <> ")"
              _       -> if latex
                            then "\\operatorname{" <> showLower g <> "}(" <> t <> ")"
                            else showLower g <> "(" <> t <> ")"
          showOp op l r =
            case op of
              Add       -> if latex
                              then "\\left(" <> l <> " + " <> r <> "\\right)"
                              else "(" <> l <> ") + (" <> r <> ")"
              Sub       -> if latex
                              then "\\left(" <> l <> " - " <> r <> "\\right)"
                              else "(" <> l <> ") - (" <> r <> ")"
              Mul       -> if latex
                              then "\\left(" <> l <> " \\cdot " <> r <> "\\right)"
                              else "(" <> l <> ") * (" <> r <> ")"
              Div       -> if latex then "\\frac{" <> l <> "}{" <> r <> "}" else "(" <> l <> ")/(" <> r <> ")"
              Power     -> if latex then "{" <> l <> "^{" <> r <> "}}" else "(" <> l <> ")^(" <> r <> ")"
              PowerAbs  -> if latex then "{\\left|" <> l <> "\\right|^{" <> r <> "}}" else "(|" <> l <> "|)^(" <> r <> ")"
              AQ        -> if latex then "\\frac{" <> l <> "}{\\sqrt{1+" <> r <> "^2}}" else "(" <> l <> "/sqrt(1 + " <> r <> "^2))"
          showNormal ec' =
            do ec <- canonical ec'
               best <- gets (_best . _info . (IM.! ec) . _eClass) >>= canonize
               case best of
                  EVar   ix -> pure (showVar ix, thetaIx)
                  EParam ix -> pure (showParam thetaIx, thetaIx + 1)
                  EConst  x -> pure (show x, thetaIx)
                  EUni g  t -> do (t', thetaIx') <- go t thetaIx
                                  pure (showFun g t', thetaIx')
                  EBin op l r -> do (l', thetaIx') <- go l thetaIx
                                    (r', thetaIx'') <- go r thetaIx'
                                    pure (showOp op l' r', thetaIx'')
                  ENAry op xs -> do (ss, thetaIx') <- goList (expandedList xs) thetaIx
                                    pure (if null ss then "" else foldl1 (showOp (toOp op)) ss, thetaIx')

showLatexTree :: ClassStore m => [String] -> Fix SRTree -> EGraphST m String
showLatexTree varnames = showNormal
  where
    showLower x = Prelude.map toLower $ show x
    latexify cs = go cs 0
      where
        go []       n  = Prelude.replicate n '}'
        go ('_':cs) n  = '_' : '{' : go cs (n+1)
        go (c:cs)   n  = c : go cs n

    showVar ix = if null varnames then ("x_{" <> show ix <> "}") else latexify (varnames !! ix)

    showParam ix = "\\theta_{" <> show ix <> "}"

    showFun g t =
      case g of
        Id -> t
        Abs -> "\\left|" <> t <> "\\right|"

        Sqrt -> "\\sqrt{" <> t <> "}"

        SqrtAbs -> "\\sqrt{\\left|" <> t <> "\\right|}"

        Cbrt    -> t <> "^{\\frac{1}{3}}"
        Square  -> t <> "^2"
        Cube    -> t <> "^3"
        LogAbs  -> "\\log{(\\left|" <> t <> "\\right|)}"

        Exp     -> "e^{" <> t <> "}"

        Recip   -> "\\frac{1}{" <> t <> "}"

        _       -> "\\operatorname{" <> showLower g <> "}(" <> t <> ")"

    showOp op l r =
      case op of
        Add       -> "\\left(" <> l <> " + " <> r <> "\\right)"

        Sub       -> "\\left(" <> l <> " - " <> r <> "\\right)"

        Mul       -> "\\left(" <> l <> " \\cdot " <> r <> "\\right)"

        Div       -> "\\frac{" <> l <> "}{" <> r <> "}"
        Power     -> "{" <> l <> "^{" <> r <> "}}"
        PowerAbs  -> "{\\left|" <> l <> "\\right|^{" <> r <> "}}"
        AQ        -> "\\frac{" <> l <> "}{\\sqrt{1+" <> r <> "^2}}"
    showNormal tr =
      do case tr of
            Fix (Var ix) -> pure (showVar ix)
            Fix (Param ix) -> pure (showParam ix)
            Fix (Const  x) -> pure (show x)
            Fix (Uni g  t) -> do t' <- showNormal t
                                 pure (showFun g t')
            Fix (Bin op l r) -> do l' <- showNormal l
                                   r' <- showNormal r
                                   pure (showOp op l' r')

fillDL loss datasets = do
  ecs <- getAllEvaluatedEClasses
  let (x', _, _) = head datasets
      n          = length x'
  forM_ ecs $ \ec -> do
    thetas <- getTheta ec
    bestExpr <- relabelParams <$> getBestExpr ec
    let nVars = maxVar bestExpr
    if VU.length (head thetas) /= countParams bestExpr || n <= nVars
       then (lift . putStrLn) $ "Wrong number of parameters in " <> showExpr bestExpr <> ": " <> show (head thetas) <> "   " <> show ec
       else do let mdl_trains = Prelude.map (\(theta, (x, y, mYerr)) -> mdlMetric loss mYerr x y theta bestExpr) $ Prelude.zip thetas datasets
               insertDL ec $ Prelude.maximum mdl_trains

fillFit loss trainDatas = do
  ecs <- getAllEvaluatedEClasses
  cleanAllDBs
  let (x', _, _) = head trainDatas
      n          = length x'
  forM_ ecs $ \ec -> do
    unsetFitness ec
    t <- relabelParams <$> getBestExpr ec
    let nVars = maxVar t
    response <- forM trainDatas $ \dt -> if n <= nVars then pure (-1.0/0.0, VU.empty) else fitnessFunRep 50 loss dt t
    let f      = Prelude.minimum (Prelude.map fst response)
        thetas = Prelude.map snd response
    insertFitness ec f thetas
    let mdl_train  = if isInfinite f then (1.0/0.0) else Prelude.maximum $ Prelude.map (\(theta, (x, y, mYErr)) -> mdlMetric loss mYErr x y theta t) $ Prelude.zip thetas trainDatas
    insertDL ec mdl_train


cleanAllDBs = do
  modify' $ over (eDB . fitRangeDB) (const Set.empty)
          . over (eDB . sizeFitDB) (const IM.empty)
          . over (eDB . dlRangeDB) (const Set.empty)
          . over (eDB . sizeDLDB) (const IM.empty)

unsetFitness :: ClassStore m => EClassId -> EGraphST m ()
unsetFitness eId = do
  --eId <- canonical eId'
  ec <- gets ((IM.! eId) . _eClass)
  let newInfo = (_info ec){_fitness = Nothing}
      newEc   = ec{_info = newInfo}
  modify' $ over eClass (IM.insert eId newEc)
