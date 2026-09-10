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
import Data.Maybe (fromJust)
import Data.SRTree
import Data.SRTree.Recursion
import Data.SRTree.Datasets
import Data.SRTree.Eval
import Data.SRTree.Print hiding ( printExpr )
import System.Random
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.IntMap.Strict as IntMap
import Data.Char ( toLower, toUpper )

import Util
import Commands
import Data.List ( isInfixOf )
import Text.Read hiding (get)
import Control.Monad ( when )
import Data.Binary ( encode )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as B

import Algorithm.EqSat.SearchSR hiding (io, myCost)
import Text.Read (readMaybe)

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



-- | Split a possibly colon-separated "egraph_path:fit_path" pair.
-- If there is no colon, both paths are set to the same value (backward compat).
splitColon :: String -> (String, String)
splitColon s = case break (== ':') s of
  (path, ':':fitPath) -> (path, fitPath)
  _                   -> (s, s)

-- | Check if a command string is DB-native (opens its own SQLite connections
-- and does not need the in-memory EGraph state or binary round-trip).
-- All commands are now DB-native.
isDBCommand :: String -> Bool
isDBCommand _ = True

persistCmd varnames (fname:ds:_) = let (f, fp) = splitColon fname in run (Persist f fp ds) >>= printFun varnames [] [] (NLL Gaussian)
persistCmd varnames _ = helpCmd ["persist"]

loadCmd varnames (fname:ds:_) = let (f, fp) = splitColon fname in run (LoadDB f fp ds) >>= printFun varnames [] [] (NLL Gaussian)
loadCmd varnames _ = helpCmd ["load"]

importCmd varnames loss vars (db:eqs:ds:_) = run (ImportDB db eqs ds loss vars True) >>= printFun varnames [] [] loss
importCmd varnames _ _ _ = helpCmd ["import"]

eqSatCmd varnames (fname:ds:n:rs:_) = case readMaybe @Int n of
                                        Nothing -> pure "The n must be an integer."
                                        Just k  -> let (f, fp) = splitColon fname in run (DBEqSat f fp ds k rs) >>= printFun varnames [] [] (NLL Gaussian)
eqSatCmd varnames _ = helpCmd ["eqsat"]

eqSatFrontierCmd varnames (fname:ds:n:rs:_) = case readMaybe @Int n of
                                        Nothing -> pure "The n must be an integer."
                                        Just k  -> let (f, fp) = splitColon fname in run (DBEqSatFrontier f fp ds k rs) >>= printFun varnames [] [] (NLL Gaussian)
eqSatFrontierCmd varnames _ = helpCmd ["eqsat-frontier"]

insertCmd varnames (fname:ds:alg:args) = let (f, fp) = splitColon fname in run (DBInsert f fp ds alg (unwords args)) >>= printFun varnames [] [] (NLL Gaussian)
insertCmd varnames _ = helpCmd ["insert"]

setFitCmd varnames (fname:ds:eid:fit:_) = case (readMaybe @Int eid, readMaybe @Double fit) of
    (Just e, Just f) -> let (fp, fp') = splitColon fname in run (DBSetFit fp fp' ds e f) >>= printFun varnames [] [] (NLL Gaussian)
    _                -> pure "set-fit requires EID FITNESS as numbers"
setFitCmd varnames _ = helpCmd ["set-fit"]

topCmd varnames (fname:ds:n:_) = case readMaybe @Int n of
                                    Nothing -> pure "The n must be an integer."
                                    Just k  -> let (f, fp) = splitColon fname in run (DBTop f fp ds k varnames False Nothing) >>= printFun varnames [] [] (NLL Gaussian)
topCmd varnames _ = helpCmd ["top"]

distCmd varnames (fname:ds:n:_) = case readMaybe @Int n of
                                    Nothing -> pure "The n must be an integer."
                                    Just k  -> let (f, fp) = splitColon fname in run (DBDist f fp ds k) >>= printFun varnames [] [] (NLL Gaussian)
distCmd varnames _ = helpCmd ["distribution"]

countCmd varnames (fname:op:_) = let (f, fp) = splitColon fname in run (DBCount f fp op) >>= printFun varnames [] [] (NLL Gaussian)
countCmd varnames _ = helpCmd ["count"]

paretoCmd varnames (fname:ds:rest) = let (f, fp) = splitColon fname
                                         byFitness = "by fitness" `isInfixOf` unwords rest || null rest
                                     in run (DBPareto f fp ds False byFitness Nothing) >>= printFun varnames [] [] (NLL Gaussian)
paretoCmd varnames _ = helpCmd ["pareto"]

streamCmd varnames (fname:op:n:_) = case readMaybe @Int n of
                                        Nothing -> pure "The n must be an integer."
                                        Just k  -> let (f, fp) = splitColon fname in run (DBStream f fp op k) >>= printFun varnames [] [] (NLL Gaussian)
streamCmd varnames _ = helpCmd ["stream"]

pushFitCmd varnames (fname:ds:_) = let (f, fp) = splitColon fname in run (PushFit f fp ds) >>= printFun varnames [] [] (NLL Gaussian)
pushFitCmd varnames _ = helpCmd ["push-fit"]

refreshFitCmd varnames (fname:ds:_) = let (f, fp) = splitColon fname in run (RefreshFit f fp ds) >>= printFun varnames [] [] (NLL Gaussian)
refreshFitCmd varnames _ = helpCmd ["refresh-fitness"]

reportCmd varnames (fname:ds:eid:rest) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname
                   ci = "ci" `elem` rest
               in run (DBReport f fp ds n ci) >>= printFun varnames [] [] (NLL Gaussian)
reportCmd varnames _ = helpCmd ["report"]

optimizeCmd varnames (fname:ds:eid:rest) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname
                   lossName = if null rest then "Gaussian" else head rest
               in run (DBOptimize f fp ds n False lossName) >>= printFun varnames [] [] (NLL Gaussian)
optimizeCmd varnames _ = helpCmd ["optimize"]

subtreesCmd varnames (fname:ds:eid:_) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname in run (DBSubtrees f fp ds n) >>= printFun varnames [] [] (NLL Gaussian)
subtreesCmd varnames _ = helpCmd ["subtrees"]

getNExprsCmd varnames (fname:ds:n:eid:_) = case (readMaybe @Int n, readMaybe @Int eid) of
    (Just k, Just e) -> let (f, fp) = splitColon fname in run (DBGetNExprs f fp ds k e) >>= printFun varnames [] [] (NLL Gaussian)
    _ -> pure "getNExprs requires N EID as integers"
getNExprsCmd varnames _ = helpCmd ["getNExprs"]

getNEclassesCmd varnames (fname:ds:n:eid:_) = case (readMaybe @Int n, readMaybe @Int eid) of
    (Just k, Just e) -> let (f, fp) = splitColon fname in run (DBGetNEclasses f fp ds k e) >>= printFun varnames [] [] (NLL Gaussian)
    _ -> pure "getNEclasses requires N EID as integers"
getNEclassesCmd varnames _ = helpCmd ["getNEclasses"]

eclassTerminalsCmd varnames (fname:ds:eid:_) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname in run (DBEClassTerminals f fp ds n) >>= printFun varnames [] [] (NLL Gaussian)
eclassTerminalsCmd varnames _ = helpCmd ["eclass-terminals"]

topPatternCmd varnames (fname:ds:n:rest) = case readMaybe @Int n of
    Nothing -> pure "The n must be an integer."
    Just k  -> let (f, fp) = splitColon fname
                   -- Separate pattern from flags: "root" and "not" are flags, rest is pattern
                   isRoot = "root" `elem` rest
                   negate = "not" `elem` rest
                   pat = unwords (filter (`notElem` ["root", "not"]) rest)
               in run (DBTopPattern f fp ds k pat isRoot negate Nothing) >>= printFun varnames [] [] (NLL Gaussian)
topPatternCmd varnames _ = helpCmd ["top-pattern"]

distributionPatternCmd varnames (fname:ds:n:_) = case readMaybe @Int n of
    Nothing -> pure "The n must be an integer."
    Just k  -> let (f, fp) = splitColon fname in run (DBDistribution f fp ds k) >>= printFun varnames [] [] (NLL Gaussian)
distributionPatternCmd varnames _ = helpCmd ["distribution-pattern"]

modularityCmd varnames (fname:ds:n:_) = case readMaybe @Int n of
    Nothing -> pure "The n must be an integer."
    Just k  -> let (f, fp) = splitColon fname in run (DBModularity f fp ds k) >>= printFun varnames [] [] (NLL Gaussian)
modularityCmd varnames _ = helpCmd ["modularity"]

countPatCmd varnames (fname:ds:pat:n:_) = case readMaybe @Int n of
    Nothing -> pure "The n must be an integer."
    Just k  -> let (f, fp) = splitColon fname in run (DBCountPat f fp ds pat k) >>= printFun varnames [] [] (NLL Gaussian)
countPatCmd varnames _ = helpCmd ["count-pattern"]

patternMapCmd varnames (fname:ds:rest) = case readMaybe @Int (last rest) of
    Nothing -> helpCmd ["pattern-map"]
    Just k  -> let (f, fp) = splitColon fname
                   n = show k
                   pat = unwords (init rest)
               in run (DBPatternMap f fp ds pat k) >>= printFun varnames [] [] (NLL Gaussian)
patternMapCmd varnames _ = helpCmd ["pattern-map"]

extractPatCmd varnames (fname:ds:eid:_) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname in run (DBExtractPat f fp ds n) >>= printFun varnames [] [] (NLL Gaussian)
extractPatCmd varnames _ = helpCmd ["extract-pattern"]

distTokensCmd varnames (fname:ds:n:_) = case readMaybe @Int n of
                                        Nothing -> pure "The n must be an integer."
                                        Just k  -> let (f, fp) = splitColon fname in run (DBDistTokens f fp ds k) >>= printFun varnames [] [] (NLL Gaussian)
distTokensCmd varnames _ = helpCmd ["distribution-tokens"]

profileDataCmd varnames (fname:ds:eid:dataPath:_) = case readMaybe @Int eid of
    Nothing -> pure "The id must be an integer."
    Just n  -> let (f, fp) = splitColon fname in run (DBProfileData f fp ds n dataPath) >>= printFun varnames [] [] (NLL Gaussian)
profileDataCmd varnames _ = helpCmd ["profile-data"]

commands = ["help", "top", "distribution", "distribution-pattern", "count", "modularity", "pareto", "report", "optimize", "eqsat", "eqsat-frontier", "insert", "set-fit", "stream", "push-fit", "refresh-fitness", "subtrees", "getNExprs", "getNEclasses", "eclass-terminals", "top-pattern", "count-pattern", "pattern-map", "extract-pattern", "distribution-tokens", "profile-data", "load", "import", "persist"]

topHlp = "top FILE DATASET N [with ci]: top-N e-classes by fitness from the SQLite database FILE."

distHlp = "distribution FILE DATASET N: number of evaluated e-classes per model size (size <= N) from the SQLite database FILE."

modHlp = "modularity FILE DATASET N: find reusable sub-components in top-N expressions from the SQLite database FILE."

countHlp = "count FILE OP: number of e-classes containing an e-node with operator OP (e.g. EAdd, EMul, LogAbs) from the SQLite database FILE."

hlpMap = Map.fromList $ Prelude.zip commands
                            [ "help <cmd>: shows a brief explanation for the command."
                            , "top FILE DATASET N [with ci]: top-N e-classes by fitness from the SQLite database FILE."
                            , "distribution FILE DATASET N: number of evaluated e-classes per model size (size <= N) from the SQLite database FILE."
                            , "distribution-pattern FILE DATASET N: pattern enumeration over top-N expressions from the SQLite database FILE."
                            , "count FILE OP: number of e-classes containing an e-node with operator OP (e.g. EAdd, EMul, LogAbs) from the SQLite database FILE."
                            , "modularity FILE DATASET N: find reusable sub-components in top-N expressions from the SQLite database FILE."
                            , "pareto FILE DATASET: Pareto front over (fitness, size) from the SQLite database FILE."
                            , "report FILE DATASET N: display a detailed report for e-class N from the SQLite database FILE."
                            , "optimize FILE DATASET N: (re)optimize e-class N using NLopt, write fitness back to the SQLite database FILE."
                            , "eqsat FILE DATASET ITER [RULES]: run ITER equality-saturation iterations out-of-core against the lazily loaded e-graph in SQLite FILE."
                            , "eqsat-frontier FILE DATASET ITER [RULES]: re-saturate only the frontier of a lazily loaded e-graph in SQLite FILE."
                            , "insert FILE DATASET EXPR: insert a single expression into the DB-backed e-graph in FILE."
                            , "set-fit FILE DATASET EID FITNESS: record the fitness of e-class EID in the fit database."
                            , "stream FILE OP N: stream the enode table by operator through a SQLite cursor, report count and first N matches."
                            , "push-fit FILE DATASET: write fitness/DL metrics into the fit table of the SQLite database FILE."
                            , "refresh-fitness FILE DATASET: overwrite in-memory fitness values with those stored in the fit table of the SQLite database FILE."
                            , "subtrees FILE DATASET N: list all e-class IDs in the best expression tree rooted at N from the SQLite database FILE."
                            , "getNExprs FILE DATASET N EID: get up to N equivalent expressions from e-class EID in the SQLite database FILE."
                            , "getNEclasses FILE DATASET N EID: get e-class ID sets for up to N expression variants from EID in the SQLite database FILE."
                            , "eclass-terminals FILE DATASET EID: list all unique terminals inside e-class EID from the SQLite database FILE."
                            , "top-pattern FILE DATASET N PATTERN [root] [not]: top-N expressions matching PATTERN, with wildcard bindings (v0, v1, ...)."
                            , "count-pattern FILE DATASET PATTERN N: count structural pattern matches in top-N expressions from the SQLite database FILE."
                            , "pattern-map FILE DATASET PATTERN N: show wildcard bindings (v0, v1, ...) for each match of PATTERN in top-N expressions."
                            , "extract-pattern FILE DATASET EID: enumerate patterns in a single expression from the SQLite database FILE."
                            , "distribution-tokens FILE DATASET N: count token frequencies in top-N expressions from the SQLite database FILE."
                            , "profile-data FILE DATASET EID DATAFILE: compute profile-likelihood data (taus, thetas, contours) for e-class EID using DATAFILE."
                            , "load FILE: load an e-graph previously persisted in the SQLite database FILE."
                            , "import DBFILE EXPRS: build an e-graph out-of-core directly in the SQLite database DBFILE by streaming the expressions in EXPRS."
                            , "persist FILE: save the current e-graph to the SQLite database FILE."
                            ]

-- Evaluation
--cmd :: Map String ([String] -> Repl ()) -> String -> Repl ()
cmd cmdMap input = do let (cmd':args) = words input
                      case cmdMap Map.!? cmd' of
                        Nothing -> pure $ "Command not found!!!"
                        Just f  -> f args

helpCmd xs = pure $ hlpMap Map.! (head xs)

reggression myCmd dataset testData loss' loadFrom dumpTo parseCSV' parseParams calcDL calcFit varnames =
  reggressionDB myCmd dataset loss' varnames

-- | DB-native path: no binary load/dump, no in-memory EGraph.
-- Commands open their own SQLite connections inside 'run'.
reggressionDB :: String -> String -> String -> [String] -> IO String
reggressionDB myCmd dataset lossStr varnames = do
  let loss = fromJust $ readLoss lossStr
      funs = [ helpCmd
             , topCmd varnames
             , distCmd varnames
             , distributionPatternCmd varnames
             , countCmd varnames
             , modularityCmd varnames
             , paretoCmd varnames
             , reportCmd varnames
             , optimizeCmd varnames
             , eqSatCmd varnames
             , eqSatFrontierCmd varnames
             , insertCmd varnames
             , setFitCmd varnames
             , streamCmd varnames
             , pushFitCmd varnames
             , refreshFitCmd varnames
             , subtreesCmd varnames
             , getNExprsCmd varnames
             , getNEclassesCmd varnames
             , eclassTerminalsCmd varnames
             , topPatternCmd varnames
             , countPatCmd varnames
             , patternMapCmd varnames
             , extractPatCmd varnames
             , distTokensCmd varnames
             , profileDataCmd varnames
             , loadCmd varnames
             , importCmd varnames loss []
             , persistCmd varnames
             ]
      cmdMap = Map.fromList $ Prelude.zip commands funs
  evalStateT (cmd cmdMap myCmd) emptyGraph

-- | Legacy path removed — all commands are now DB-native.

