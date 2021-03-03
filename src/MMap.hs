module MMap where

import Data.Vector
import Data.Vector.Generic.Mutable
import Control.Monad.Primitive

import Resource
import Map

data MMap s = MMap Index (Data.Vector.MVector s Resource)

thawMap :: (PrimMonad m) => Map -> m (MMap (PrimState m))
thawMap (Map i m) = MMap i <$> thaw m

freezeMap :: PrimMonad m => MMap (PrimState m) -> m Map
freezeMap (MMap i m) = Map i <$> freeze m

writeMap :: (PrimMonad m) => MMap (PrimState m) -> Index -> Resource -> m ()
writeMap (MMap d m) i r = write m (convertIndex d i) r

readMap :: PrimMonad m => MMap (PrimState m) -> Index -> m Resource
readMap (MMap d m) i = Data.Vector.Generic.Mutable.read m (convertIndex d i)

exchangeTile :: PrimMonad m => MMap (PrimState m) -> Index -> Resource -> m Resource
exchangeTile (MMap d m) i r = exchange m (convertIndex d i) r

cloneMMap :: PrimMonad m => MMap (PrimState m) -> m (MMap (PrimState m))
cloneMMap (MMap d m) = MMap d <$> clone m

newMMap :: PrimMonad m => Index -> m (MMap (PrimState m))
newMMap d@(w, h) = do
  m <- Data.Vector.Generic.Mutable.replicate (w * h) None
  return $ MMap d m
