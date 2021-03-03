module Patch where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Maybe
import Data.Vector.Generic.Mutable

import Resource
import Map
import MMap

data Patch = Patch Resource [Index] Map

instance Show Patch where
  show (Patch r b m) = show r <> ", Boundary: " <> show b <> "\n" <> show m

resource :: Patch -> Resource
resource (Patch r _ _) = r

size :: Patch -> Int
size (Patch _ _ m) = countResource m (/= None)

-- -- this assumes that all the maps are of the same size
-- combinePatches :: [Patch] -> Map
-- combinePatches ((Patch r _ m):ps) = combinePatchMap r m $ cobinePatches 

getPatchAt :: PrimMonad m => MMap (PrimState m) -> Index -> Int -> m Patch
getPatchAt m@(MMap d _) i k = do
  m' <- newMMap d
  r <- readMap m i
  markTile m m' i k
  bs <- markPatch m m' r i k
  p <- freezeMap m'
  if bs == [] then error ("bug: " <> show (Patch r bs p)) else return $ Patch r bs p
  where
    markPatch :: PrimMonad m => MMap (PrimState m) -> MMap (PrimState m) -> Resource -> Index -> Int -> m [Index]
    markPatch m@(MMap d _) m' r i k = do
      res <- forM (getNeighbours d i) $ \j -> do
        r' <- readMap m j
        if (r == r')
          then do
            markTile m m' j k
            bs <- markPatch m m' r j k
            return (True, bs)
          else return (r' == Mark k, [])
      let (b, bs) = foldl (\(x, l) (x', l') -> (x && x', l <> l')) (True, []) res
      return (bs <> if b then [] else [i])

    markTile :: PrimMonad m => MMap (PrimState m) -> MMap (PrimState m) -> Index -> Int -> m ()
    markTile m m' i k = writeMap m i (Mark k) >> writeMap m' i (Mark k)

markAllPatches :: PrimMonad m => Map -> m [Patch]
markAllPatches m@(Map d _) = do
  tm <- thawMap m
  res <- forCoords d $ \i -> do
    r <- readMap tm i
    case r of
      None -> return Nothing
      (Mark _) -> return Nothing
      _ -> Just <$> getPatchAt tm i (convertIndex d i)
  return $ catMaybes res

distancePatchPoint :: Floating a => Patch -> Index -> a
distancePatchPoint (Patch _ _ m@(Map d _)) (x, y) =
  let res = runIdentity $ forCoords d (\i ->
                                         case resourceAt m i of
                                           (Mark _) -> return $ Just i
                                           _ -> return Nothing)
  in sqrt $ fromIntegral $ minimum $ map (\(x', y') -> sqNorm (x - x', y - y')) $ catMaybes res

distancePatchPatch :: Floating a => Patch -> Patch -> a
distancePatchPatch (Patch _ r _) (Patch _ r' _) = sqrt $ fromIntegral $ squareDistanceRings r r'
  where
    -- this is inefficient: because the rings are sorted by one
    -- coordinate, we can remember the current minimum and just look
    -- at points that are further than that mimimum in the direction
    -- they are sorted in
    squareDistanceRings :: [Index] -> [Index] -> Int
    squareDistanceRings r r' = minimum $ map (squareDistanceRingPoint r) r'

    squareDistanceRingPoint :: [Index] -> Index -> Int
    squareDistanceRingPoint r (x, y) = minimum $ map (\(x', y') -> sqNorm (x - x', y - y')) r
