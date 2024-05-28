{-# LANGUAGE BangPatterns #-}

module Data.QuadTreeWrapped where

import qualified Data.QuadTree as QT
import qualified Data.PackedMemoryQuadtree as PMQ

newtype QuadTreeWrapped a = QuadTreeWrapped {getQT :: QT.QuadTree a}
  deriving (Show)

empty :: Eq v => Int -> Int -> (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v)
empty w h initialElement = QuadTreeWrapped {getQT = QT.makeTree (w, h) initialElement }

insert :: Eq v => PMQ.Coords n -> (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v)
insert (PMQ.Coords x y) v !qt = QuadTreeWrapped {getQT = QT.setLocation (x, y) v (getQT qt)}

rangeLookup :: PMQ.Coords n -> PMQ.Coords n -> QuadTreeWrapped (PMQ.Coords n, v) -> [(PMQ.Coords n, v)]
rangeLookup (PMQ.Coords x1 y1) (PMQ.Coords x2 y2) qt = rangeLookup' zl zr qt
  where
    zl = PMQ.toZIndex (PMQ.Coords (min x1 x2) (min y1 y2))
    zr = PMQ.toZIndex (PMQ.Coords (max x1 x2) (max y1 y2))

rangeLookup' :: PMQ.ZIndex n -> PMQ.ZIndex n -> QuadTreeWrapped (PMQ.Coords n, v) -> [(PMQ.Coords n, v)]
rangeLookup' zl zr qt = QT.filterTree (\(coord, val) -> PMQ.isRelevant zl zr (PMQ.toZIndex coord)) (getQT qt)

lookup :: Eq v => PMQ.Coords n -> QuadTreeWrapped (PMQ.Coords n, v) -> (PMQ.Coords n, v)
lookup (PMQ.Coords x y) !qtw = QT.getLocation (x, y) (getQT qtw)