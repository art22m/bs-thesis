{-# LANGUAGE BangPatterns #-}

module Data.RTreeWrapped where

import qualified Data.RTree as RT
import qualified Data.RTree.MBB as MBB
import qualified Data.PackedMemoryQuadtree as PMQ

newtype RTreeWrapped a = RTreeWrapped {getRT :: RT.RTree a}
  deriving (Show)

empty :: RTreeWrapped v
empty = RTreeWrapped {getRT = RT.empty}

insert :: PMQ.Coords n -> v -> RTreeWrapped v -> RTreeWrapped v
insert (PMQ.Coords x y) v !rt = RTreeWrapped {getRT = RT.insert mbb v (getRT rt)}
  where
    mbb = RT.mbb (fromIntegral x) (fromIntegral y) (fromIntegral x) (fromIntegral y)

rangeLookup :: PMQ.Coords n -> PMQ.Coords n -> RTreeWrapped v -> [(PMQ.Coords n, v)]
rangeLookup (PMQ.Coords x1 y1) (PMQ.Coords x2 y2) rt = rangeLookup' from to rt 
  where
    from = (PMQ.Coords (min x1 x2) (min y1 y2))
    to = (PMQ.Coords (max x1 x2) (max y1 y2))

rangeLookup' ::  PMQ.Coords n -> PMQ.Coords n -> RTreeWrapped v -> [(PMQ.Coords n, v)]
rangeLookup' (PMQ.Coords x1 y1) (PMQ.Coords x2 y2) rt =  map mbbToCoord res
  where
    mbb = RT.mbb (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2)
    res = RT.lookupRangeWithKey mbb (getRT rt)

mbbToCoord :: (RT.MBB, v) -> (PMQ.Coords n, v)
mbbToCoord (mbb, v) = ((PMQ.Coords (round (MBB.getUlx mbb)) (round (MBB.getUly mbb))), v)