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
import Algorithm.SRTree.Opt
import Control.Monad.State.Strict

import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build
import Algorithm.EqSat.Info
import Algorithm.EqSat.Queries
import Algorithm.EqSat.DB
import Algorithm.EqSat.Simplify

import qualified Data.IntMap as IM
import Data.Massiv.Array as MA hiding (forM_, forM, Continue, convert)
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
import Text.ParseSR (SRAlgs(..), parseSR, parsePat, Output(..), showOutput)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IntSet
import qualified Data.Set as SSet
import System.IO (withFile, IOMode(ReadMode)) 

import Algorithm.EqSat.SearchSR hiding (io)
import Text.Read (readMaybe)

data Args = Args
  { _dataset       :: String,
    _testData      :: String,
    _loss          :: Distribution,
    _dumpTo        :: String,
    _loadFrom      :: String,
    _parseCSV      :: String,
    _parseParams   :: Bool,
    _calcDL        :: Bool
  }
  deriving (Show)

egraph :: IO a -> MyEGraph a
egraph = Control.Monad.State.Strict.lift

printFun :: [DataSet] -> [DataSet] -> Distribution -> PrintResults -> MyEGraph String 
printFun _          _         _    (MultiExprs eids) = printSimpleMultiExprs eids
printFun datatrains datatests loss (SingleExpr eid)  = printExpr datatrains datatests loss eid 
printFun _          _         _    (Counts pats)     = printMultiCounts pats
printFun _          _         _    (SimpleStr str)   = pure str 
printFun _          _         _    NoPrint           = pure ""


runIfRight cmd = case cmd of
                    Left err -> pure $ "wrong command format."
                    Right c  -> run c >>= printFun [] [] Gaussian 

--topCmd :: [String] -> Repl ()
topCmd []    = helpCmd ["top"]
topCmd args  = do
  let cmd = parseCmd parseTop (B.pack $ unwords args)
  runIfRight cmd

--distCmd :: [String] -> Repl ()
distCmd []   = helpCmd ["distribution"]
distCmd args = do
  let cmd = parseCmd parseDist (B.pack $ unwords args)
  runIfRight cmd

--reportCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
reportCmd _ _ _ [] = helpCmd ["report"]
reportCmd dist trainData testData args =
  case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> run (Report n (dist, trainData, testData)) >>= printFun trainData testData dist 

--optimizeCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
optimizeCmd _ _ _ [] = helpCmd ["optimize"]
optimizeCmd dist trainData testData args =
  case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> do let nIters = if length args > 1 then fromMaybe 100 (readMaybe @Int (args !! 1)) else 100
                  run (Optimize n nIters (dist, trainData, trainData)) >>= printFun trainData testData dist

--subtreesCmd :: [String] -> Repl ()
subtreesCmd [] = helpCmd ["subtrees"]
subtreesCmd (arg:_) = case readMaybe @Int arg of
                        Nothing -> pure "The argument must be an integer."
                        Just n  -> (run (Subtrees n) >>= printFun [] [] Gaussian)

--insertCmd :: Distribution -> [DataSet] -> [DataSet] -> [String] -> Repl ()
insertCmd dist trainData testData [] = helpCmd ["insert"]
insertCmd dist trainData testData args = do
  let etree = parseSR TIR "" False $ B.pack (unwords args)
  case etree of
    Left _     -> pure $ "no parse for " <> unwords args
    Right tree -> do ec <- fromTree myCost tree
                     (run (Optimize ec 100 (dist, trainData, trainData)) >>= printFun trainData testData dist)

--paretoCmd :: [String] -> Repl ()
paretoCmd []   = run (Pareto ByFitness) >>= printFun [] [] Gaussian
paretoCmd args = case (Prelude.map toLower $ unwords args) of
                    "by fitness" -> (run (Pareto ByFitness ) >>= printFun [] [] Gaussian)
                    "by dl"      -> (run (Pareto ByDL) >>= printFun [] [] Gaussian)
                    _            -> helpCmd ["pareto"]

--countPatCmd :: [String] -> Repl ()
countPatCmd []   = helpCmd ["count-pattern"]
countPatCmd args = run (CountPat (unwords args)) >>= printFun [] [] Gaussian

--saveCmd :: [String] -> Repl ()
saveCmd [] = helpCmd ["save"]
saveCmd args = run (Save (unwords args)) >>= printFun [] [] Gaussian

--loadCmd :: [String] -> Repl ()
loadCmd [] = helpCmd ["load"]
loadCmd args = run (Load (unwords args)) >>= printFun [] [] Gaussian

--importCmd :: Distribution -> String -> [String] -> Repl ()
importCmd dist varnames (fname:params:_) = run (Import fname dist varnames (Prelude.read params)) >>= printFun [] [] dist
importCmd dist varnames _   = helpCmd ["import"]

distTokensCmd args = run DistTokens >>= printFun [] [] Gaussian

extractPatCmd args = case readMaybe @Int (head args) of
    Nothing -> pure "The id must be an integer."
    Just n  -> run (ExtractPat n) >>= printFun [] [] Gaussian

commands = ["help", "top", "report", "optimize", "subtrees", "insert", "count-pattern", "distribution", "pareto", "save", "load", "import", "extract-pattern", "distribution-tokens"]

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
                            , "subtrees N: shows the subtrees for the tree rotted with id N."
                            , "insert EXPR: inserts a new expression EXPR and evaluates."
                            , countHlp
                            , distHlp
                            , "pareto [by fitness| by dl]: shows the pareto front where the first objective is the criteria (default: fitness) and the second objective is model size."
                            , "save FILE: save current e-graph to a file named FILE."
                            , "load FILE: load current e-graph from a file named FILE."
                            , "displays the disbution of tokens"
                            , "extract the patterns from a single expression"
                            ]

-- Evaluation
--cmd :: Map String ([String] -> Repl ()) -> String -> Repl ()
cmd cmdMap input = do let (cmd':args) = words input
                      case cmdMap Map.!? cmd' of
                        Nothing -> pure $ "Command not found!!!"
                        Just f  -> f args

helpCmd xs = pure $ hlpMap Map.! (head xs)

reggression myCmd dataset testData loss' loadFrom dumpTo parseCSV' parseParams calcDL = do
  let loss        = fromJust $ readMaybe loss'
      args = Args dataset testData loss dumpTo loadFrom parseCSV' parseParams calcDL

  g <- getStdGen
  let datasets = words (_dataset args)
  dataTrainsWP' <- Prelude.mapM (flip loadDataset True) datasets
  let dataTrainsWP = Prelude.map (\((a, b, _, _), (c, _), v, _) -> ((a,b,c), v)) dataTrainsWP'

  let dataTrains = Prelude.map fst dataTrainsWP
      varnames   = snd . head $ dataTrainsWP

  dataTests  <- if null (_testData args)
                  then pure dataTrains
                  else (Prelude.mapM (flip loadTrainingOnly True) $ words (_testData args))
  eg <- if (not.null) (_loadFrom args)
           then withFile (_loadFrom args) ReadMode  \h -> do
                        bs <- BS.hGetContents h
                        BS.length bs `seq` pure (decode bs)
           else if (not. null) (_parseCSV args)
                 then parseCSV (_loss args) (_parseCSV args) varnames (_parseParams args)
                 else pure emptyGraph
  let loss = _loss args
      funs = [ helpCmd
             , topCmd
             , reportCmd loss dataTrains dataTests
             , optimizeCmd loss dataTrains dataTests
             , subtreesCmd
             , insertCmd loss dataTrains dataTests
             , countPatCmd
             , distCmd
             , paretoCmd
             , saveCmd
             , loadCmd
             , importCmd loss varnames
             , extractPatCmd
             , distTokensCmd
             ]
      cmdMap = Map.fromList $ Prelude.zip commands funs

      repl = cmd cmdMap myCmd
      crRun :: MyEGraph String
      crRun = do createDBBest
                 when (_calcDL args) $ fillDL loss dataTrains
                 rebuildAllRanges
                 output <- repl
                 when ((not.null) (_dumpTo args)) $ do eg <- get
                                                       io $ BS.writeFile (_dumpTo args) (encode eg)
                 pure output
  evalStateT crRun eg

