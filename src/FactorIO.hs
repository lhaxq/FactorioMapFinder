module FactorIO where

import Graphics.Image
import Graphics.Image.Interface

import Data.Vector
import System.IO

import Resource
import Map
import Filter

type FactorIO = IO

previews :: String
previews = "phase2-previews/"

seedListHandle :: IO Handle
seedListHandle = do
  h <- openFile "phase2.csv" ReadMode
  hGetLine h -- ignore first line
  return h

getSeed :: Handle -> IO String
getSeed h = fst <$> Prelude.break (',' ==) <$> hGetLine h

seedPath :: String -> Maybe MapSettings -> String
seedPath s settings = s <> ".png"

findSeed :: String -> Maybe MapSettings -> FactorIO Map
findSeed s settings = do
  img <- readImageRGB VS (previews <> seedPath s settings)
  return $ Map (dims img) (fmap determineResource (Data.Vector.convert $ toVector $ toWord8I img))

saveSeed :: String -> Maybe MapSettings -> String -> FactorIO ()
saveSeed s settings f = appendFile f (s <> "\n")

checkSeed :: String -> Maybe MapSettings -> FactorIO ()
checkSeed s settings = do
  putStr ("Seed " <> s <> ": ")
  m <- findSeed s settings
  res <- accept <$> filterSeed m
  putStrLn (if res then "accepted" else "rejected")
  saveSeed s settings $ if res then "acceptedSeeds" else "rejectedSeeds"

getAndCheckSeed :: Handle -> Int -> Maybe MapSettings -> FactorIO ()
getAndCheckSeed h startAt settings = do
  s <- getSeed h
  if Prelude.read s >= startAt
    then checkSeed s settings
    else return ()

testSeed :: String -> FactorIO ()
testSeed s = do
  m <- findSeed s Nothing
  res <- filterSeed m
  putStr $ showSeedResult res
