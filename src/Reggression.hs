{-# LANGUAGE  BlockArguments #-}
{-# LANGUAGE  TupleSections #-}
{-# LANGUAGE  MultiWayIf #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}

module Reggression where

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

import Foreign.C (CInt (..), CDouble (..))
import Foreign.C.String (CString, newCString, withCString, peekCString, peekCAString, newCAString)
import Paths_reggression (version)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import Text.Read (readMaybe)
import Data.Version (showVersion)
import Control.Exception (Exception (..), SomeException (..), handle)
import System.IO

import CLI
import Py

foreign import ccall unsafe_py_write_stdout :: CString -> IO ()

py_write_stdout :: String -> IO ()
py_write_stdout str = withCString str unsafe_py_write_stdout

foreign import ccall unsafe_py_write_stderr :: CString -> IO ()

py_write_stderr :: String -> IO ()
py_write_stderr str = withCString str unsafe_py_write_stderr

foreign export ccall hs_reggression_version :: IO CString

hs_reggression_version :: IO CString
hs_reggression_version =
  newCString (showVersion version)

foreign export ccall hs_reggression_main :: IO CInt

exitHandler :: ExitCode -> IO CInt
exitHandler ExitSuccess = return 0
exitHandler (ExitFailure n) = return (fromIntegral n)

uncaughtExceptionHandler :: SomeException -> IO CInt
uncaughtExceptionHandler (SomeException e) =
  py_write_stderr (displayException e) >> return 1

hs_reggression_main :: IO CInt
hs_reggression_main =
  handle uncaughtExceptionHandler $
    handle exitHandler $ do
      cli
      return 0


foreign export ccall hs_reggression_run :: CString -> CString -> CString -> CString -> CString -> CString -> CString -> CInt -> CInt -> CInt -> IO CString

hs_reggression_run :: CString -> CString -> CString -> CString -> CString -> CString -> CString -> CInt -> CInt -> CInt -> IO CString
hs_reggression_run myCmd dataset testData loss loadFrom dumpTo parseCSV parseParams' calcDL' calcFit' = do
  myCmd' <- peekCString myCmd
  dataset' <- peekCString dataset
  testData' <- peekCString testData
  loss' <- peekCString loss
  dumpTo' <- peekCString dumpTo
  loadFrom' <- peekCString loadFrom
  parseCSV' <- peekCString parseCSV
  out  <- reggression myCmd' dataset' testData' loss' loadFrom' dumpTo' parseCSV' (parseParams' /= 0) (calcDL' /= 0) (calcFit' /= 0)
  newCString out
