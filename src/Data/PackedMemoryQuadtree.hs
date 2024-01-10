{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Data.PackedMemoryQuadtree where

import Data.Bits
import Data.List
import qualified Data.Map as DMap
import Data.PackedMemoryArray (PMA)
import qualified Data.PackedMemoryArray as PMA
import Data.PackedMemoryArrayMap (Map)
import qualified Data.PackedMemoryArrayMap as Map
import qualified Data.Vector as Vector hiding ((++))
import GHC.TypeLits (Nat)

---- Constants

_MISSES_THRESHOLD :: Int
_MISSES_THRESHOLD = 10

---- ZIndex

data Coords (n :: Nat) = Coords Int Int
  deriving (Show)

newtype ZIndex (n :: Nat) = ZIndex Int
  deriving (Eq, Ord, Show)

toZIndex :: Coords n -> ZIndex n
toZIndex (Coords x y) = ZIndex val
  where
    shifts = [0 .. finiteBitSize x - 1]
    val = Data.List.foldl' (\acc i -> acc .|. (shiftL (bitAt x i) (2 * i) .|. shiftL (bitAt y i) (2 * i + 1))) 0 shifts

-- TODO: Refactor
fromZIndex' :: Int -> Coords n
fromZIndex' v = fromZIndex (ZIndex v)

fromZIndex :: ZIndex n -> Coords n
fromZIndex (ZIndex val) = Coords x y
  where
    shifts = [0 .. (finiteBitSize val `div` 2 - 1)]
    x = Data.List.foldl' (\acc i -> acc .|. shiftL (bitAt val (2 * i)) i) 0 shifts
    y = Data.List.foldl' (\acc i -> acc .|. shiftL (bitAt val (2 * i + 1)) i) 0 shifts

bitAt :: Int -> Int -> Int
bitAt x i = shiftR x i .&. 1

isRelevant :: ZIndex n -> ZIndex n -> ZIndex n -> Bool
isRelevant minZIndex maxZIndex testZIndex =
  (minX <= testX && testX <= maxX) && (minY <= testY && testY <= maxY)
  where
    Coords minX minY = fromZIndex minZIndex
    Coords maxX maxY = fromZIndex maxZIndex
    Coords testX testY = fromZIndex testZIndex

-- bounds calculates LitMax and BigMin values.
-- LitMax’s value of a and b can be calculated as ‘all common most significant bits’ in a and b followed by a 0 and then 1’s,
-- and the BigMin’s value of a and b would be ‘all common most significant bits’ in a and b followed by 1 and then 0’s.
bounds :: Int -> Int -> (Int, Int)
bounds l r = (litMax, bigMin)
  where
    lxorb = xor l r
    landb = l .&. r
    intBitSize = finiteBitSize l

    leftPartSize = countLeadingZeros lxorb
    rightPartSize = intBitSize - leftPartSize

    leftPart = resetBits landb rightPartSize
    bigMinRight = resetBits lxorb (rightPartSize - 1) -- 00..00_100..00
    litMaxRight = bigMinRight - 1 --- 00..00_011..11
    litMax = leftPart .|. litMaxRight
    bigMin = leftPart .|. bigMinRight

    -- resetBits 0x1001010 3 => 0x1001000 (zeroes 3 bits from right)
    resetBits :: Int -> Int -> Int
    resetBits num n = shiftL (shiftR num n) n

splitRegion :: ZIndex n -> ZIndex n -> (ZIndex n, ZIndex n)
splitRegion (ZIndex l) (ZIndex r)
  | isHorizontalSplit = (toZIndex (Coords xr litMax), toZIndex (Coords xl bigMin))
  | otherwise = (toZIndex (Coords litMax yr), toZIndex (Coords bigMin yl))
  where
    Coords xl yl = fromZIndex (ZIndex l)
    Coords xr yr = fromZIndex (ZIndex r)

    isHorizontalSplit = even (countLeadingZeros (xor l r))
    (litMax, bigMin) = if isHorizontalSplit then bounds yl yr else bounds xl xr

-- TODO: delete
splitRegion' :: Int -> Int -> (Int, Int)
splitRegion' l r = (nl, nr)
  where
    (ZIndex nl, ZIndex nr) = splitRegion (ZIndex l) (ZIndex r)

---- Data Structure

newtype Quadtree a = Quadtree {getPMAMap :: Map Int a}
  deriving (Show)

null :: Quadtree v -> Bool
null qt = Map.null (getPMAMap qt)

empty :: Quadtree v
empty = Quadtree {getPMAMap = Map.empty}

singleton :: Coords n -> v -> Quadtree v
singleton c v = Quadtree {getPMAMap = Map.singleton zid v}
  where
    ZIndex zid = toZIndex c

lookup :: Coords n -> Quadtree v -> Maybe v
lookup c qt = Map.lookup zid pm
  where
    pm = getPMAMap qt
    ZIndex zid = toZIndex c

rangeLookupDummy :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookupDummy (Coords x1 y1) (Coords x2 y2) qt = rangeLookupDummy' (toZIndex cl) (toZIndex cr) qt
  where
    cl = Coords (min x1 x2) (min y1 y2)
    cr = Coords (max x1 x2) (max y1 y2)

rangeLookupDummy' :: ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookupDummy' (ZIndex zl) (ZIndex zr) qt = go zl zr (getPMAMap qt) []
  where
    go :: Int -> Int -> Map Int v -> [(Coords n, v)] -> [(Coords n, v)]
    go l r pm tmp
      | l <= r && shouldLookup = case Map.lookup r pm of
          Just val -> go l (r - 1) pm ((fromZIndex (ZIndex r), val) : tmp)
          Nothing -> go l (r - 1) pm tmp
      | l <= r = go l (r - 1) pm tmp
      | otherwise = tmp
      where
        shouldLookup = isRelevant (ZIndex zl) (ZIndex zr) (ZIndex r)

rangeLookupSeq :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookupSeq (Coords x1 y1) (Coords x2 y2) qt = rangeLookupSeq' zl zr zl zr qt
  where
    zl = toZIndex (Coords (min x1 x2) (min y1 y2))
    zr = toZIndex (Coords (max x1 x2) (max y1 y2))

rangeLookupSeq' :: ZIndex n -> ZIndex n -> ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookupSeq' (ZIndex zl') (ZIndex zr') (ZIndex zl) (ZIndex zr) qt =
  rangePMA (Map.getPMA pmaMap) ++ rangeDMap (Map.getMap pmaMap) ++ rangeNS (Map.getNS pmaMap)
  where
    pmaMap = getPMAMap qt

    rangePMA :: PMA Int v -> [(Coords n, v)]
    rangePMA pma = go pma pos []
      where
        pos' = PMA.binsearch zl (PMA.cells (Map.getPMA (getPMAMap qt)))
        pos
          | pos' < 0 = 0
          | otherwise = pos'

        go :: PMA Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go pma' p tmp
          | inBounds && inRange && shouldLookup = case mval of
              Just val -> go pma' (p + 1) ((fromZIndex' key, val) : tmp)
              Nothing -> go pma' (p + 1) tmp
          | inBounds = go pma' (p + 1) tmp -- TODO: fix, must work with inRange
          | otherwise = tmp
          where
            (key, mval) = case PMA.cells pma' Vector.! p of
              (Just (k, v)) -> (k, Just v)
              Nothing -> (zl, Nothing)
            inBounds = 0 <= p && p < Vector.length (PMA.cells pma')
            inRange = zl <= key && key <= zr
            shouldLookup = isRelevant (ZIndex zl') (ZIndex zr') (ZIndex key)

    rangeDMap :: DMap.Map Int v -> [(Coords n, v)]
    rangeDMap dmap = go dmap zr []
      where
        go :: DMap.Map Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go dmap' p tmp
          | inRange && shouldLookup = case DMap.lookup p dmap' of
              Just val -> go dmap' (p - 1) ((fromZIndex' p, val) : tmp)
              Nothing -> go dmap' (p - 1) tmp
          | inRange = go dmap' (p - 1) tmp
          | otherwise = tmp
          where
            inRange = zl <= p && p <= zr
            shouldLookup = isRelevant (ZIndex zl') (ZIndex zr') (ZIndex p)

    rangeNS :: Map.NS Int v -> [(Coords n, v)]
    rangeNS Map.M0 = []
    rangeNS (Map.M1 as) = rangeChunk as
    rangeNS (Map.M2 as bs _ rest) = rangeChunk as ++ rangeChunk bs ++ rangeNS rest
    rangeNS (Map.M3 as bs cs _ rest) = rangeChunk as ++ rangeChunk bs ++ rangeChunk cs ++ rangeNS rest

    -- TODO: Rewrite with one binsearch + linear
    rangeChunk :: Map.Chunk Int v -> [(Coords n, v)]
    rangeChunk ch = go ch zl []
      where
        go :: Map.Chunk Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go ch' p tmp
          | inRange && shouldLookup = case Map.lookup1 p ch' Nothing of
              Just val -> go ch' (p + 1) ((fromZIndex' p, val) : tmp)
              Nothing -> go ch' (p + 1) tmp
          | inRange = go ch' (p + 1) tmp
          | otherwise = tmp
          where
            inRange = zl <= p && p <= zr
            shouldLookup = isRelevant (ZIndex zl') (ZIndex zr') (ZIndex p)

rangeLookup :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookup (Coords x1 y1) (Coords x2 y2) qt = rangeLookup' (toZIndex cl) (toZIndex cr) qt
  where
    cl = Coords (min x1 x2) (min y1 y2)
    cr = Coords (max x1 x2) (max y1 y2)

rangeLookup' :: ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookup' zl zr qt = go zl zr qt ranges []
  where
    ranges = calculateRanges zl zr
    -- TODO: Probably call rangeLookupSeq' durion ranges calculation
    go :: ZIndex n -> ZIndex n -> Quadtree v -> [(ZIndex n, ZIndex n)] -> [(Coords n, v)] -> [(Coords n, v)]
    go zl' zr' qt' (r : rs) tmp = rangeLookupSeq' zl' zr' (fst r) (snd r) qt' ++ go zl' zr' qt' rs tmp
    go _ _ _ [] tmp = tmp

-- TODO: remove
calculateRanges' :: Int -> Int -> [(ZIndex n, ZIndex n)]
calculateRanges' ul br = calculateRanges (ZIndex ul) (ZIndex br)

calculateRanges :: ZIndex n -> ZIndex n -> [(ZIndex n, ZIndex n)]
calculateRanges (ZIndex ul) (ZIndex br) = go ul br ul 0 []
  where
    go :: Int -> Int -> Int -> Int -> [(ZIndex n, ZIndex n)] -> [(ZIndex n, ZIndex n)]
    go l r p m tmp -- Upper left, bottom right, current position, misses count, temp. result
      | inBounds && shouldLookup = go l r (p + 1) 0 tmp
      | m >= _MISSES_THRESHOLD && (litmax < p && p < bigmin) =
          go bigmin r bigmin 0 tmp ++ [(ZIndex l, ZIndex litmax)]
      | m >= _MISSES_THRESHOLD && (p < litmax) =
          go l litmax p m tmp ++ go bigmin r bigmin 0 tmp
      | m >= _MISSES_THRESHOLD && (bigmin < p) = -- 71
          go bigmin r p m tmp ++ [(ZIndex l, ZIndex litmax)]
      | inBounds = go l r (p + 1) (m + 1) tmp
      | otherwise = (ZIndex l, ZIndex r) : tmp
      where
        inBounds = l <= p && p <= r
        shouldLookup = isRelevant (ZIndex l) (ZIndex r) (ZIndex p)
        (litmax, bigmin) = splitRegion' l r

insertP :: Coords n -> v -> Quadtree v -> Quadtree v
insertP c v qt = Quadtree {getPMAMap = Map.insertP zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c

insertE :: Coords n -> v -> Quadtree v -> Quadtree v
insertE c v qt = Quadtree {getPMAMap = Map.insert zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c