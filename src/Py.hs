{-# LANGUAGE  BlockArguments #-}
{-# LANGUAGE  TupleSections #-}
{-# LANGUAGE  MultiWayIf #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}

module Py (reggression) where

import Algorithm.SRTree.Likelihoods
import Control.Monad.State.Strict

import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat.Queries
import Algorithm.EqSat.DB
import Algorithm.EqSat.Simplify

import qualified Data.IntMap as IM
import Data.Maybe (fromJust, isNothing, isJust)
import Data.SRTree
import Data.SRTree.Recursion
import Data.SRTree.Datasets
import Data.SRTree.Eval
import Data.SRTree.Random (randomTree)
import Data.SRTree.Print hiding ( printExpr )
import Options.Applicative as Opt hiding (Const, columns)
import System.Random
import qualified Data.HashSet as Set
import Data.List ( sort, sortOn )
import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.IntMap.Strict as IntMap
import Data.Char ( toLower, toUpper )
import Debug.Trace
import Algorithm.EqSat (runEqSat)

import Util
import Commands
import Data.List ( isPrefixOf, intercalate, nub )
import Text.Read hiding (get)
import Control.Monad ( forM, when, forM_ )
import Data.Binary ( encode, decode )
import qualified Data.ByteString.Lazy as BS
import Data.Maybe ( fromMaybe )
import Text.ParseSR (SRAlgs(..), parseSR, Output(..), showOutput)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IntSet
import qualified Data.Set as SSet
import System.IO (withFile, IOMode(ReadMode)) 

import Algorithm.EqSat.SearchSR hiding (io, myCost)
import Text.Read (readMaybe)

data Args = Args
  { _dataset       :: String,
    _testData      :: String,
    _loss          :: Loss,
    _dumpTo        :: String,
    _loadFrom      :: String,
    _parseCSV      :: String,
    _parseParams   :: Bool,
    _calcDL        :: Bool,
    _calcFit       :: Bool
  }
  deriving (Show)

egraph :: IO a -> MyEGraph a
egraph = Control.Monad.State.Strict.lift

printFun :: [String] -> [DataSet] -> [DataSet] -> Loss -> PrintResults -> MyEGraph String
printFun varnames _          _         _    (MultiExprs eids) = printSimpleMultiExprs varnames eids
printFun varnames datatrains datatests loss (SingleExpr eid)  = printExpr varnames datatrains datatests loss eid
printFun varnames _          _         _    (Counts pats)     = printMultiCounts pats
printFun varnames _          _         _    (SimpleStr str)   = pure str
printFun varnames _          _         _    NoPrint           = pure ""
printFun varnames _          _         _    (MultiTrees ts)   = printSimpleMultiTrees varnames ts
printFun varnames _          _         _    (MultiClass eids) = printEClasses eids
runIfRight varnames cmd = case cmd of
                            Left err -> pure $ "wrong command format."
                            Right c  -> run c >>= printFun varnames [] [] (NLL Gaussian)

--topCmd :: [String] -> Repl ()
topCmd varnames []    = helpCmd ["top"]
topCmd varnames args  = do
  let cmd = parseCmd parseTop (B.pack $ unwords args)
  runIfRight varnames cmd

--distCmd :: [String] -> Repl ()
distCmd varnames []   = helpCmd ["distribution"]
distCmd varnames args = do
  let cmd = parseCmd parseDist (B.pack $ unwords args)
  runIfRight varnames cmd

modCmd varnames []   = helpCmd ["modularity"]
modCmd varnames args = do
  let cmd = parseCmd parseModular (B.pack $ unwords args)
  runIfRight varnames cmd

--reportCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
reportCmd varnames _ _ _ [] = helpCmd ["report"]
reportCmd varnames dist trainData testData args =
  case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> run (Report n (dist, trainData, testData)) >>= printFun varnames trainData testData dist

--optimizeCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
optimizeCmd varnames _ _ _ [] = helpCmd ["optimize"]
optimizeCmd varnames dist trainData testData args =
  case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> do let nIters = if length args > 1 then fromMaybe 100 (readMaybe @Int (args !! 1)) else 100
                  run (Optimize n nIters (dist, trainData, trainData)) >>= printFun varnames trainData testData dist

eqSatCmd varnames _ _ _ [] = helpCmd ["eqsat"]
eqSatCmd varnames dist trainData testData (arg:_) = case readMaybe @Int arg of
                        Nothing -> pure "The argument must be an integer."
                        Just n  -> run (EqSatStep n (dist, trainData, trainData)) >>= printFun varnames trainData testData dist

getNExprsCmd varnames (arg1:arg2:_) = case  ((,) <$> readMaybe @Int arg1 <*> readMaybe @Int arg2) of
                                  Nothing -> pure $ "Both arguments should be an integer."
                                  Just (n,eid) -> run (GetNExprs n eid) >>= printFun varnames [] [] (NLL Gaussian)
getNExprsCmd varnames _ = helpCmd ["getNExprs"]

getNEclassesCmd varnames (arg1:arg2:_) = case  ((,) <$> readMaybe @Int arg1 <*> readMaybe @Int arg2) of
                                          Nothing -> pure $ "Both arguments should be an integer."
                                          Just (n,eid) -> run (GetNEclass n eid) >>= printFun varnames [] [] (NLL Gaussian)
getNEclassesCmd varnames _ = helpCmd ["getNEclasses"]

--subtreesCmd :: [String] -> Repl ()
subtreesCmd varnames [] = helpCmd ["subtrees"]
subtreesCmd varnames (arg:_) = case readMaybe @Int arg of
                        Nothing -> pure "The argument must be an integer."
                        Just n  -> (run (Subtrees n) >>= printFun varnames [] [] (NLL Gaussian))

--insertCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
insertCmd varnames dist trainData testData [] = helpCmd ["insert"]
insertCmd varnames dist trainData testData args = do
  let etree = parseSR TIR "" False $ B.pack (unwords args)
  case etree of
    Left _     -> pure $ "no parse for " <> unwords args
    Right tree -> do ec <- fromTree myCost tree
                     (run (Optimize ec 100 (dist, trainData, trainData)) >>= printFun varnames trainData testData dist)

--paretoCmd :: [String] -> Repl ()
paretoCmd varnames []   = run (Pareto ByFitness) >>= printFun varnames [] [] (NLL Gaussian)
paretoCmd varnames args = case (Prelude.map toLower $ unwords args) of
                    "by fitness" -> (run (Pareto ByFitness ) >>= printFun varnames [] [] (NLL Gaussian))
                    "by dl"      -> (run (Pareto ByDL) >>= printFun varnames [] [] (NLL Gaussian))
                    _            -> helpCmd ["pareto"]

--countPatCmd :: [String] -> Repl ()
countPatCmd varnames []   = helpCmd ["count-pattern"]
countPatCmd varnames args = run (CountPat (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)

--saveCmd :: [String] -> Repl ()
saveCmd varnames [] = helpCmd ["save"]
saveCmd varnames args = run (Save (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)

--loadCmd :: [String] -> Repl ()
loadCmd varnames [] = helpCmd ["load"]
loadCmd varnames args = run (Load (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)

--importCmd :: Distribution -> String -> [String] -> Repl ()
importCmd varnames dist varnames' (fname:params:_) = run (Import fname dist varnames' (Prelude.read params)) >>= printFun varnames [] [] dist
importCmd varnames dist varnames' _   = helpCmd ["import"]

distTokensCmd varnames [] = helpCmd ["distribution-tokens"]
distTokensCmd varnames (arg:_) = case readMaybe arg of
                          Just n -> run (DistTokens n) >>= printFun varnames [] [] (NLL Gaussian)
                          Nothing -> helpCmd ["distribution-tokens"]

extractPatCmd varnames args = case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> run (ExtractPat n) >>= printFun varnames [] [] (NLL Gaussian)

persistCmd varnames [] = helpCmd ["persist"]
persistCmd varnames args = run (Persist (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)

dbLoadCmd varnames [] = helpCmd ["db-load"]
dbLoadCmd varnames args = run (LoadDB (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)

dbTopCmd varnames (fname:n:_) = case readMaybe @Int n of
                                    Nothing -> pure "The n must be an integer."
                                    Just k  -> run (DBTop fname k) >>= printFun varnames [] [] (NLL Gaussian)
dbTopCmd varnames _ = helpCmd ["db-top"]

dbDistCmd varnames (fname:n:_) = case readMaybe @Int n of
                                    Nothing -> pure "The n must be an integer."
                                    Just k  -> run (DBDist fname k) >>= printFun varnames [] [] (NLL Gaussian)
dbDistCmd varnames _ = helpCmd ["db-distribution"]

dbCountCmd varnames (fname:op:_) = run (DBCount fname op) >>= printFun varnames [] [] (NLL Gaussian)
dbCountCmd varnames _ = helpCmd ["db-count"]

dbParetoCmd varnames (fname:_) = run (DBPareto fname) >>= printFun varnames [] [] (NLL Gaussian)
dbParetoCmd varnames _ = helpCmd ["db-pareto"]

commands = ["help", "top", "report", "optimize", "eqsat", "getNExprs", "subtrees", "insert", "count-pattern", "distribution", "modularity", "pareto", "save", "load", "import", "extract-pattern", "distribution-tokens", "getNEclasses", "persist", "db-load", "db-top", "db-distribution", "db-count", "db-pareto"]

topHlp = "top N [FILTER...] [CRITERIA] [[not] matching [root] PATTERN] \n \
         \ \n \
         \ FILTER: with [size|cost|parameters] [<|<=|=|>|>=] N \n \
         \ CRITERIA: [by fitness | by dl]  \n \
         \ \n \
         \ where \"dl\" is the description length, \"cost\" is the default cost function \n \
         \ and \"parameters\" refer to the number of parameters. The cost function  \n \
         \ assigns a cost of 1 to terminals, 2 to binary operators and 3 to \n \
         \ nonlinear functions. \n \
         \ \n \
         \ Example: \n \
         \ \n \
         \ top 10 with size <= 10 with parameters > 2 by fitness matching v0 * x0 + t0 \n \
         \ \n \
         \ This will return the 10 best expressions by fitness with size less than \n \
         \ or equal to 10 and more than 2 parameters containing any sub-expression  \n \
         \ in the format f(x) * x0 + t0. \n \
         \ To create a pattern for matching you can use x0 .. xn to represent a variable \n \
         \  t0 .. tn to represent a numerical parameter, and v0 .. vn to represent wildcards. \n \
         \ Notice that v0 * x0 + v0 will pattern expressions such as (sin(t0) + x0) * x0 + (sin(t0) + x0) \n \
         \ but not (sin(t0) + x0) * x0 + t0, since both occurrences of v0 will match the same expression. \n \
         \ (see `help count-pattern` for more details) \
         \ The keyword \"root\" will matches only expressions starting with this pattern."

distHlp = "distribution [FILTER] [LIMIT] \n\n \
          \ FILTER: with size [<|<=|=|>|>=] N \n \
          \ LIMIT: limited at N [asc|dsc] \n\n \
          \ Shows the distribution of all the patterns in the set of evaluated expressions.\n \
          \ The list can be filtered by the size of the pattern and limited by the top most frequent (dsc) \n \
          \ or least frequent (asc) patterns. \n\n \
          \ See `help count-pattern` for details on the syntax of pattern."

modHlp = "modularity n [FILTER] [CRITERIA] \n\n \
          \ FILTER: with size [<|<=|=|>|>=] N \n \
          \ CRITERIA: [by fitness | by dl] \n\n \
          \ Shows the top-N equations by the criteria presenting modularity \n \
          \ (repeated pattern). The filter limits the size of the repeated pattern."

countHlp = "count-pattern PAT \n\n \
           \ Count the number of occurrence of the pattern PAT in the e-graph. \n\n \
           \ A pattern follows the same syntax of an expression: \n\n\
           \ EXPR := FUN(EXPR) | EXPR OP EXPR | TERM \n\
           \ FUN := abs | sin | cos | tan | sinh | cosh | tanh | asin | acos | atan | asinh | acosh | atanh | sqrt | sqrtabs | cbrt | square | log | logabs | exp | recip | cube \n\
           \ OP := + | - | * | / | aq | ^ | |^| \n\
           \ TERM := xN | tN | vN \n\n\
           \ where: \n \
           \ - aq is the analytical quotient (x aq y = x / sqrt(1 + y^2)) \n \
           \ - x |^| y = abs(x) ^ y \n \
           \ - xN is the N-th input variable \n \
           \ - tN is the N-th numerical parameter \n \
           \ - vN is the N-th pattern variable (see below) \n\n \
           \ The pattern variable works as a wildcard matching any expression. \n \
           \ If we use the same pattern variable multiple times in the expression, \n \
           \ the pattern must be the same in every occurrence. \n\n \
           \ Examples: \n\n \
           \ v0 + x0 will match anything added to x0\n \
           \ v0 + v1 * x0 will match anything added to any expression multiplied by x0. \
           \ For example: t0 ^ 2 + exp(t1 + x1) * x0. \n \
           \ v0 + v0 * x0 will match any expression added with this same expression multiplied by x0. \
           \ For example: t0 ^ 2 + (t0 ^ 2) * x0."

hlpMap = Map.fromList $ Prelude.zip commands
                            [ "help <cmd>: shows a brief explanation for the command."
                            , topHlp
                            , "report N: displays a detailed report for the expression with id N."
                            , "optimize N: (re)optimize expression with id N."
                            , "eqsat: run a single step of equality saturation and refit any expression with changed number of parameters."
                            , "getNExprs N id: get N equivalent expressions rooted at id."
                            , "subtrees N: shows the subtrees for the tree rotted with id N."
                            , "insert EXPR: inserts a new expression EXPR and evaluates."
                            , countHlp
                            , distHlp
                            , modHlp
                            , "pareto [by fitness| by dl]: shows the pareto front where the first objective is the criteria (default: fitness) and the second objective is model size."
                            , "save FILE: save current e-graph to a file named FILE."
                            , "load FILE: load current e-graph from a file named FILE."
                            , "displays the disbution of tokens"
                            , "extract the patterns from a single expression"
                            , "extract list of e-classes for N equivalnt expressions"
                            , "persist FILE: save the current e-graph to the SQLite database FILE."
                            , "db-load FILE: load an e-graph previously persisted in the SQLite database FILE."
                            , "db-top FILE N: top-N e-classes by fitness queried from the SQLite database FILE."
                            , "db-distribution FILE N: number of evaluated e-classes per model size (size <= N) from the SQLite database FILE."
                            , "db-count FILE OP: number of e-classes containing an e-node with operator OP (e.g. EAdd, EMul, LogAbs) from the SQLite database FILE."
                            , "db-pareto FILE: Pareto front over (fitness, dl) from the SQLite database FILE."
                            ]

-- Evaluation
--cmd :: Map String ([String] -> Repl ()) -> String -> Repl ()
cmd cmdMap input = do let (cmd':args) = words input
                      case cmdMap Map.!? cmd' of
                        Nothing -> pure $ "Command not found!!!"
                        Just f  -> f args

helpCmd xs = pure $ hlpMap Map.! (head xs)

reggression myCmd dataset testData loss' loadFrom dumpTo parseCSV' parseParams calcDL calcFit varnames = do
  let loss        = fromJust $ readLoss loss'
      args = Args dataset testData loss dumpTo loadFrom parseCSV' parseParams calcDL calcFit

  g <- getStdGen
  let datasets = words (_dataset args)
  dataTrainsWP' <- Prelude.mapM (flip loadDataset True) datasets
  let dataTrainsWP = Prelude.map (\((a, b, _, _), (c, _), v, _) -> ((a,b,c), v)) dataTrainsWP'

  let dataTrains = Prelude.map fst dataTrainsWP
      varnames'  = snd . head $ dataTrainsWP

  dataTests  <- if null (_testData args)
                  then pure dataTrains
                  else (Prelude.mapM (flip loadTrainingOnly True) $ words (_testData args))
  eg <- if (not.null) (_loadFrom args)
           then withFile (_loadFrom args) ReadMode  \h -> do
                        bs <- BS.hGetContents h
                        BS.length bs `seq` pure (decode bs)
           else if (not. null) (_parseCSV args)
                 then parseCSV (_loss args) (_parseCSV args) varnames' (_parseParams args)
                 else pure emptyGraph
  let loss = _loss args
      funs = [ helpCmd
             , topCmd varnames
             , reportCmd varnames loss dataTrains dataTests
             , optimizeCmd varnames loss dataTrains dataTests
             , eqSatCmd varnames loss dataTrains dataTests
             , getNExprsCmd varnames
             , subtreesCmd varnames
             , insertCmd varnames loss dataTrains dataTests
             , countPatCmd varnames
             , distCmd varnames
             , modCmd varnames
             , paretoCmd varnames
             , saveCmd varnames
             , loadCmd varnames
             , importCmd varnames loss varnames'
             , extractPatCmd varnames
             , distTokensCmd varnames
             , getNEclassesCmd varnames
             , persistCmd varnames
             , dbLoadCmd varnames
             , dbTopCmd varnames
             , dbDistCmd varnames
             , dbCountCmd varnames
             , dbParetoCmd varnames
             ]
      cmdMap = Map.fromList $ Prelude.zip commands funs

      repl = cmd cmdMap myCmd
      crRun :: MyEGraph String
      crRun = do createDBBest
                 if _calcFit args
                    then fillFit loss dataTrains
                    else if _calcDL args
                           then fillDL loss dataTrains
                           else pure ()
                 rebuildAllRanges
                 output <- repl
                 when ((not.null) (_dumpTo args)) $ do eg <- get
                                                       io $ BS.writeFile (_dumpTo args) (encode eg)
                 pure output
  evalStateT crRun eg

