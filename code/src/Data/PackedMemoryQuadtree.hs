{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.PackedMemoryQuadtree where

import Data.Bits
import Data.List
import qualified Data.PackedMemoryArrayMap as PMAMap
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
    val = foldl' (\acc i -> acc .|. (shiftL (bitAt x i) (2 * i) .|. shiftL (bitAt y i) (2 * i + 1))) 0 shifts

fromZIndex :: ZIndex n -> Coords n
fromZIndex (ZIndex val) = Coords x y
  where
    shifts = [0 .. (finiteBitSize val `div` 2 - 1)]
    x = foldl' (\acc i -> acc .|. shiftL (bitAt val (2 * i)) i) 0 shifts
    y = foldl' (\acc i -> acc .|. shiftL (bitAt val (2 * i + 1)) i) 0 shifts

bitAt :: Int -> Int -> Int
bitAt x i = shiftR x i .&. 1

isRelevant :: ZIndex n -> ZIndex n -> ZIndex n -> Bool
isRelevant minZIndex maxZIndex testZIndex =
  testX >= minX && testX <= maxX && testY >= minY && testY <= maxY
  where
    Coords minX minY = fromZIndex minZIndex
    Coords maxX maxY = fromZIndex maxZIndex
    Coords testX testY = fromZIndex testZIndex

isHorizontalSplit :: ZIndex n -> ZIndex n -> Bool
isHorizontalSplit (ZIndex l) (ZIndex r) = odd (go shifts)
  where
    shifts = reverse [0 .. finiteBitSize l - 1]

    go :: [Int] -> Int
    go [] = 0
    go (x : xs)
      | bitAt l x == bitAt r x = go xs
      | otherwise = x

bounds :: Int -> Int -> (Int, Int)
bounds l r = (litMax, bigMin)
  where
    lxorb = xor l r
    landb = l .&. r
    bitSize = finiteBitSize l

    leftPartSize = countLeadingZeros lxorb
    rightPartSize = bitSize - leftPartSize

    leftPart = zeroBits landb rightPartSize
    bigMinRight = zeroBits lxorb (rightPartSize - 1) -- 00..00_100..00
    litMaxRight = bigMinRight - 1 --- 00..00_011..11
    litMax = leftPart .|. litMaxRight
    bigMin = leftPart .|. bigMinRight

    -- zeroBits 0x1001010 3 => 0x1001000 (zeroes 3 bits from right)
    zeroBits :: Int -> Int -> Int
    zeroBits num n = shiftL (shiftR num n) n -- num >> n << n

splitRegion :: ZIndex n -> ZIndex n -> (ZIndex n, ZIndex n)
splitRegion l r
  | isHorizontal = (toZIndex (Coords xr litMax), toZIndex (Coords xl bigMin))
  | otherwise = (toZIndex (Coords litMax yr), toZIndex (Coords bigMin yl))
  where
    Coords xl yl = fromZIndex l
    Coords xr yr = fromZIndex r

    isHorizontal = isHorizontalSplit l r
    (litMax, bigMin) = if isHorizontal then bounds yl yr else bounds xl xr

---- Data Structure

data Quadtree a = Quadtree {getPMAMap :: PMAMap.Map Int a}
  deriving (Show)

null :: Quadtree v -> Bool
null qt = PMAMap.null (getPMAMap qt)

empty :: Quadtree v
empty = Quadtree {getPMAMap = PMAMap.empty}

singleton :: Coords n -> v -> Quadtree v
singleton c v = Quadtree {getPMAMap = PMAMap.singleton zid v}
  where
    ZIndex zid = toZIndex c

lookup :: Coords n -> Quadtree v -> Maybe v
lookup c qt = PMAMap.lookup zid pm
  where
    pm = getPMAMap qt
    ZIndex zid = toZIndex c

insert :: Coords n -> v -> Quadtree v -> Quadtree v
insert c v qt = Quadtree {getPMAMap = PMAMap.insertP zid v pm}
  where
    pm = getPMAMap qt
    ZIndex zid = toZIndex c