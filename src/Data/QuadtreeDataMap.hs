{-# LANGUAGE BangPatterns #-}

module Data.QuadtreeDataMap where

import qualified Data.Map as DMap
import qualified Data.PackedMemoryQuadtree as PMQ

newtype QuadtreeDMap a = QuadtreeDMap {getMap :: DMap.Map Int a}
  deriving (Show)

empty :: QuadtreeDMap v
empty = QuadtreeDMap {getMap = DMap.empty}

insert :: PMQ.Coords n -> v -> QuadtreeDMap v -> QuadtreeDMap v
insert c v !qt = QuadtreeDMap {getMap = DMap.insert zid v (getMap qt)}
  where
    PMQ.ZIndex zid = PMQ.toZIndex c

rangeLookup :: PMQ.Coords n -> PMQ.Coords n -> QuadtreeDMap v -> [(PMQ.Coords n, v)]
rangeLookup (PMQ.Coords x1 y1) (PMQ.Coords x2 y2) qt = rangeLookup' zl zr qt
  where
    zl = PMQ.toZIndex (PMQ.Coords (min x1 x2) (min y1 y2))
    zr = PMQ.toZIndex (PMQ.Coords (max x1 x2) (max y1 y2))

rangeLookup' :: PMQ.ZIndex n -> PMQ.ZIndex n -> QuadtreeDMap v -> [(PMQ.Coords n, v)]
rangeLookup' (PMQ.ZIndex zl) (PMQ.ZIndex zr) qt = go (getMap qt)
  where
    go :: DMap.Map Int v -> [(PMQ.Coords n, v)]
    go dmap = map (\(c, v) -> (PMQ.fromZIndex' c, v)) (DMap.toList filteredMap)
      where
        (_, rmap) = DMap.split (zl - 1) dmap
        (lmap, _) = DMap.split (zr + 1) rmap
        filteredMap = DMap.filterWithKey (\k _ -> PMQ.isRelevant' zl zr k) lmap