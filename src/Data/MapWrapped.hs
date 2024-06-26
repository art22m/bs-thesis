{-# LANGUAGE BangPatterns #-}

module Data.MapWrapped where

import qualified Data.Map as DMap
import qualified Data.PackedMemoryQuadtree as PMQ

newtype MapWrapped a = MapWrapped {getMap :: DMap.Map Int a}
  deriving (Show)

empty :: MapWrapped v
empty = MapWrapped {getMap = DMap.empty}

insert :: PMQ.Coords n -> v -> MapWrapped v -> MapWrapped v
insert c v !qt = MapWrapped {getMap = DMap.insert zid v (getMap qt)}
  where
    PMQ.ZIndex zid = PMQ.toZIndex c

rangeLookup :: PMQ.Coords n -> PMQ.Coords n -> MapWrapped v -> [(PMQ.Coords n, v)]
rangeLookup (PMQ.Coords x1 y1) (PMQ.Coords x2 y2) qt = rangeLookup' zl zr qt
  where
    zl = PMQ.toZIndex (PMQ.Coords (min x1 x2) (min y1 y2))
    zr = PMQ.toZIndex (PMQ.Coords (max x1 x2) (max y1 y2))

rangeLookup' :: PMQ.ZIndex n -> PMQ.ZIndex n -> MapWrapped v -> [(PMQ.Coords n, v)]
rangeLookup' (PMQ.ZIndex zl) (PMQ.ZIndex zr) qt = go (getMap qt)
  where
    go :: DMap.Map Int v -> [(PMQ.Coords n, v)]
    go dmap = map (\(c, v) -> (PMQ.fromZIndex' c, v)) (DMap.toList filteredMap)
      where
        (_, rmap) = DMap.split (zl - 1) dmap
        (lmap, _) = DMap.split (zr + 1) rmap
        filteredMap = DMap.filterWithKey (\k _ -> PMQ.isRelevant' zl zr k) lmap


lookup :: PMQ.Coords n -> MapWrapped v -> Maybe v
lookup c mw = DMap.lookup zid m
  where
    m = getMap mw
    PMQ.ZIndex zid = PMQ.toZIndex c