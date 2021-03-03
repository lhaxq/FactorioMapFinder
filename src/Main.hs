{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import FactorIO
import Patch
import MMap
import Resource
import Filter

import Control.Monad
import Data.List
import Data.Ord

import Graphics.Image
import Graphics.Image.Interface

import Graphics.Image.Interface.Repa

import System.Console.CmdArgs

data Args = Single { seed :: Maybe String } | Batch { n :: Maybe Int, startAt :: Int }
  deriving (Show, Data, Typeable)

single = Single { seed = def &= help "Seed to test" }
batch = Batch { n = def &= help "Number of seeds (default 1000)", startAt = def &= help "" }

main :: IO ()
main = do
  args <- cmdArgs $ modes [single, batch &= auto]
  case args of
    Single s -> maybe (putStrLn "Need to supply a seed!") testSeed s
    Batch x startAt -> do
      let n = maybe 1000 id x
      h <- seedListHandle
      void $ replicateM n (getAndCheckSeed h startAt Nothing)
