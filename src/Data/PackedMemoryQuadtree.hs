{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.PackedMemoryQuadtree where

import Data.Bits
import Data.List
import qualified Data.Map as DMap
import Data.Maybe (fromMaybe)
import Data.PackedMemoryArray (PMA)
import qualified Data.PackedMemoryArray as PMA
import Data.PackedMemoryArrayMap (Map)
import qualified Data.PackedMemoryArrayMap as Map
import Data.Vector hiding ((++))
import qualified Data.Vector as Vector hiding ((++))
import GHC.TypeLits (Nat)

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
  testX >= minX && testX <= maxX && testY >= minY && testY <= maxY
  where
    Coords minX minY = fromZIndex minZIndex
    Coords maxX maxY = fromZIndex maxZIndex
    Coords testX testY = fromZIndex testZIndex

-- bounds calculates LitMax and BigMin values.
-- LitMax’s value can be calculated as ‘all common most significant bits’ in a and b followed by a 0 and then 1’s,
-- and the BigMin’s y value would be ‘all common most significant bits’ in a and b followed by 1 and then 0’s.
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

rangeLookupDummiest :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookupDummiest cl cr qt = go (min zl zr) (max zl zr) (getPMAMap qt) []
  where
    ZIndex zl = toZIndex cl
    ZIndex zr = toZIndex cr

    go :: Int -> Int -> Map Int v -> [(Coords n, v)] -> [(Coords n, v)]
    go l r pm tmp
      | l <= r = case Map.lookup r pm of
          Just val -> go l (r - 1) pm ((fromZIndex (ZIndex r), val) : tmp)
          Nothing -> go l (r - 1) pm tmp
      | otherwise = tmp

rangeLookupDummy :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookupDummy cl cr qt = 
  rangePMA (Map.getPMA pmaMap) ++ rangeDMap (Map.getMap pmaMap) ++ rangeNS(Map.getNS pmaMap)
  where
    ZIndex zl = toZIndex cl
    ZIndex zr = toZIndex cr

    pmaMap = getPMAMap qt

    rangePMA :: PMA Int v -> [(Coords n, v)]
    rangePMA pma = go pma pmaPos []
      where
        pmaPos = PMA.binsearch zl (PMA.cells (Map.getPMA (getPMAMap qt)))

        go :: PMA Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go pma' p tmp
          | shouldContinue = case mval of
              Just val -> go pma' (p + 1) ((fromZIndex' key, val) : tmp)
              Nothing -> go pma' (p + 1) tmp
          | otherwise = tmp
          where
            (key, mval) = case PMA.cells pma' Vector.! p of
              (Just (k, v)) -> (k, Just v)
              Nothing -> (zl, Nothing)
            shouldContinue = (0 <= p && p < Vector.length (PMA.cells pma')) && (zl <= key && key <= zr)

    rangeDMap :: DMap.Map Int v -> [(Coords n, v)]
    rangeDMap dmap = go dmap zr []
      where
        go :: DMap.Map Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go dmap' p tmp
          | zl <= p && p <= zr = case DMap.lookup p dmap' of
              Just val -> go dmap' (p - 1) ((fromZIndex' p, val) : tmp)
              Nothing -> go dmap' (p - 1) tmp
          | otherwise = tmp

    rangeNS :: Map.NS Int v -> [(Coords n, v)]
    rangeNS  Map.M0                  = []
    rangeNS (Map.M1 as)              = rangeChunk as
    rangeNS (Map.M2 as bs _ rest)    = rangeChunk as ++ rangeChunk bs ++ rangeNS rest
    rangeNS (Map.M3 as bs cs _ rest) = rangeChunk as ++ rangeChunk bs ++ rangeChunk cs ++ rangeNS rest

    -- TODO: Rewrite with one binsearch + linear then 
    rangeChunk :: Map.Chunk Int v -> [(Coords n, v)]
    rangeChunk ch = go ch zr []
      where 
        go :: Map.Chunk Int v -> Int -> [(Coords n, v)] -> [(Coords n, v)]
        go ch' p tmp 
          | zl <= p && p <= zr = case Map.lookup1 p ch' Nothing of
              Just val -> go ch' (p - 1) ((fromZIndex' p, val) : tmp)
              Nothing -> go ch' (p - 1) tmp
          | otherwise = tmp

insertP :: Coords n -> v -> Quadtree v -> Quadtree v
insertP c v qt = Quadtree {getPMAMap = Map.insertP zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c

insertE :: Coords n -> v -> Quadtree v -> Quadtree v
insertE c v qt = Quadtree {getPMAMap = Map.insert zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c