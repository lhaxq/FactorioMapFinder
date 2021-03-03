module Filter where

import Map
import Resource
import Patch

import Data.Ord
import Data.Maybe

import Control.Monad
import Control.Monad.Primitive

data ScoredPatch = ScoredPatch Patch Double

goodPatch :: ScoredPatch -> Bool
goodPatch (ScoredPatch _ s) = s > 0

dropScore :: ScoredPatch -> Patch
dropScore (ScoredPatch p _) = p

incrementScore :: Double -> ScoredPatch -> ScoredPatch
incrementScore s' (ScoredPatch p s) = ScoredPatch p $ s + s'

showScoredPatch :: ScoredPatch -> String
showScoredPatch (ScoredPatch p s) =
  "Distance: " <> show (distanceFromSpawn p) <> " Size: " <> show (size p) <> " Score: " <> show s

data SeedResult = SeedResult
  { accept :: Bool
  , coalScores :: [ScoredPatch]
  , waterScores :: [ScoredPatch]
  , ironScores :: [ScoredPatch]
  , copperScores :: [ScoredPatch]
  , largeCopperScores :: [ScoredPatch]}

showSeedResult :: SeedResult -> String
showSeedResult s = unlines $
  ["Coal:"]
  <> (showScoredPatch <$> coalScores s)
  <> ["Water:"]
  <> (showScoredPatch <$> waterScores s)
  <> ["Iron:"]
  <> (showScoredPatch <$> ironScores s)
  <> ["Copper:"]
  <> (showScoredPatch <$> copperScores s)
  <> ["Large Copper:"]
  <> (showScoredPatch <$> largeCopperScores s)
  <> ["Result: " <> if accept s then "accepted" else "rejected"]

emptyReject :: SeedResult
emptyReject = SeedResult False [] [] [] [] []

startingAreaRadius :: Float
startingAreaRadius = 12

closeTo :: (Floating a, Ord a) => a -> Patch -> Patch -> Bool
closeTo d p p' = distancePatchPatch p p' < d

distanceFromSpawn :: Floating a => Patch -> a
distanceFromSpawn p@(Patch _ _ m) = distancePatchPoint p (origin m)

closeToSpawn :: Map -> Patch -> Bool
closeToSpawn m p = distanceFromSpawn p < startingAreaRadius

filterByResource :: Resource -> [Patch] -> [Patch]
filterByResource r = filter (\(Patch r' _ _) -> r == r')

scoreCoalPatch :: Map -> Patch -> ScoredPatch
scoreCoalPatch m p = ScoredPatch p $
  sizeFactor * ((fromIntegral $ min maxSize $ size p) - minSize)
  + distanceFactor * (distanceModifier - distanceFromSpawn p)
  where
    minSize = 8
    maxSize = 20
    sizeFactor = 0.5
    distanceFactor = 1
    distanceModifier = 6

scoreIronPatch :: [Patch] -> Patch -> ScoredPatch
scoreIronPatch c p = ScoredPatch p $
  sizeFactor * ((fromIntegral $ size p) - minSize)
  + distanceFactor * (distanceModifier - distanceFromSpawn p)
  + coalDistanceFactor * (maxCoalDistance - distanceToCoal)
  where
    minSize = 30
    sizeFactor = 0.2
    distanceFactor = 1
    distanceModifier = 6
    maxCoalDistance = 1
    distanceToCoal = minimum $ map (distancePatchPatch p) c
    coalDistanceFactor = 5

scoreCopperPatch :: Patch -> ScoredPatch
scoreCopperPatch p = ScoredPatch p $
  (fromIntegral $ size p) - minSize
  where
    minSize = 10

scoreLargeCopperPatch :: Patch -> ScoredPatch
scoreLargeCopperPatch p = ScoredPatch p $
  (min maxSize $ fromIntegral $ size p) - minSize + distanceFactor * (min maxDistanceScore (distanceModifier - distanceFromSpawn p))
  where
    minSize = 50
    maxSize = 70
    distanceFactor = 2
    maxDistanceScore = 5
    distanceModifier = 35

coalWaterScore :: (Floating a, Ord a) => Patch -> [Patch] -> a
coalWaterScore p w = factor * (maxDistance - distanceToWater)
  where
    factor = 5
    distanceToWater = minimum $ map (distancePatchPatch p) w
    maxDistance = 2

scoreCoalPatchWithWater :: Map -> [Patch] -> ScoredPatch -> Maybe ScoredPatch
scoreCoalPatchWithWater m w sp@(ScoredPatch p s) =
  let s = coalWaterScore p w
  in if s > 0 then Just (incrementScore s sp) else Nothing

scorePatchTriangle :: Patch -> Patch -> Patch -> Double
scorePatchTriangle c i o =
  maxDistance - (distancePatchPatch c i + distancePatchPatch i o + distancePatchPatch o c)
  where
    maxDistance = 4

filterSeed :: Map -> IO SeedResult
filterSeed m = do
  patches <- markAllPatches m
  let coalPatches = filter goodPatch $ (scoreCoalPatch m) <$> filterByResource Coal patches
  let largeCopperPatches = filter goodPatch $ scoreLargeCopperPatch <$> filterByResource Copper patches
  if null coalPatches || null largeCopperPatches then return emptyReject
    else do
    let waterPatches = filterByResource Water patches
    let ironPatches = filterByResource Iron patches
    let copperPatches = filter goodPatch $ scoreCopperPatch <$> filterByResource Copper patches
    let coalWaterPatches = catMaybes $ (scoreCoalPatchWithWater m waterPatches) <$> coalPatches
    let goodIronPatches = filter goodPatch $ map (scoreIronPatch (dropScore <$> coalPatches)) ironPatches
    let goodTriangles = [(c, i, o) | c <- dropScore <$> coalPatches, i <- dropScore <$> goodIronPatches, o <- dropScore <$> copperPatches, scorePatchTriangle c i o > 0]
    return $ SeedResult (not $ (null goodTriangles || null coalWaterPatches))
      coalPatches
      coalWaterPatches
      goodIronPatches
      copperPatches
      largeCopperPatches
