module Map where

import Resource
import Data.Vector
import Control.Monad

data MapSettings

type Index = (Int, Int)
data Map = Map Index (Vector Resource)

sqNorm :: Index -> Int
sqNorm (x, y) = x^2 + y^2

shape :: Map -> Index
shape (Map i _) = i

origin :: Map -> Index
origin (Map (w, h) _) = (w `div` 2, h `div` 2)

convertIndex :: Index -> Index -> Int
convertIndex (w, h) (x, y) = x + w * y

resourceAt :: Map -> Index -> Resource
resourceAt (Map d m) i = m ! convertIndex d i

inBounds :: Index -> Index -> Bool
inBounds (w, h) (x, y) = x >= 0 && y >= 0 && x < w && y < h

getNeighbours :: Index -> Index -> [Index]
getNeighbours d (x, y) = Prelude.filter (inBounds d) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

forCoords :: Monad m => Index -> (Index -> m a) -> m [a]
forCoords (w, h) = Control.Monad.forM [(x, y) | x <- [0..w-1], y <- [0..h-1]]

countResource :: Map -> (Resource -> Bool) -> Int
countResource (Map _ m) p = Data.Vector.length $ Data.Vector.filter p m

instance Show Map where
  show m =
    let (dimx, dimy) = shape m
    in unlines [unwords [abbrevShow (resourceAt m (x, y)) | x <- [0..dimx-1]] | y <- [0..dimy-1]]
