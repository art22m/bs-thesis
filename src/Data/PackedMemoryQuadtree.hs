{-# LANGUAGE BangPatterns #-}
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
import System.Random (mkStdGen, randomRs)

---- Constants

_MISSES_THRESHOLD :: Int
_MISSES_THRESHOLD = 4

---- ZIndex

data Coords (n :: Nat) = Coords Int Int
  deriving (Show)

newtype ZIndex (n :: Nat) = ZIndex Int
  deriving (Eq, Ord, Show)

toZIndex' :: Int -> Int -> ZIndex n
toZIndex' x y = toZIndex (Coords x y)

toZIndex :: Coords n -> ZIndex n
toZIndex (Coords x y) = ZIndex val
  where
    shifts = [0 .. finiteBitSize x - 1]
    val = Data.List.foldl' (\acc i -> acc .|. (shiftL (bitAt x i) (2 * i) .|. shiftL (bitAt y i) (2 * i + 1))) 0 shifts

bits' :: Int -> [Int]
bits' x = reverse [0 .. finiteBitSize x - 1]

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

nextZIndex' :: Int -> Int -> Int -> Int
nextZIndex' curr rmin rmax = go 10 rmin rmax 0
  where
    go :: Int -> Int -> Int -> Int -> Int
    go (-1) _ _ bigmin = bigmin
    go i mn mx bigmin = case (bitAt curr i, bitAt mn i, bitAt mx i) of
      (0, 0, 0) -> go (i - 1) mn mx bigmin
      (0, 0, 1) -> go (i - 1) mn mx' bigmin'
      (0, 1, 0) -> error "This case not possible because MIN <= MAX"
      (0, 1, 1) -> mn
      (1, 0, 0) -> bigmin
      (1, 0, 1) -> go (i - 1) bigmin' mx bigmin
      (1, 1, 0) -> error "This case not possible because MIN <= MAX"
      (1, 1, 1) -> go (i - 1) mn mx bigmin
      (_, _, _) -> error "unexpected values"
      where
        (mx', bigmin') = splitRegion' mn mx

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
      | l <= r && shouldLookup = case Map.lookup l pm of
          Just val -> go (l + 1) r pm ((fromZIndex (ZIndex l), val) : tmp)
          Nothing -> go (l + 1) r pm tmp
      | l <= r = go (l + 1) r pm tmp
      | otherwise = tmp
      where
        shouldLookup = isRelevant (ZIndex zl) (ZIndex zr) (ZIndex l)

rangeLookupSeq :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookupSeq (Coords x1 y1) (Coords x2 y2) qt = rangeLookupSeq' zl zr zl zr qt
  where
    zl = toZIndex (Coords (min x1 x2) (min y1 y2))
    zr = toZIndex (Coords (max x1 x2) (max y1 y2))

rangeLookupSeq' :: ZIndex n -> ZIndex n -> ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookupSeq' (ZIndex zl') (ZIndex zr') (ZIndex zl) (ZIndex zr) qt =
  []
    ++ rangePMA (Map.getPMA pmaMap)
    ++ rangeDMap (Map.getMap pmaMap)
    ++ rangeNS (Map.getNS pmaMap)
  where
    pmaMap = getPMAMap qt

    rangePMA :: PMA Int v -> [(Coords n, v)]
    rangePMA pma = map (\(c, v) -> (fromZIndex' c, v)) (Vector.toList filteredPMA)
      where
        pmaCells = Vector.catMaybes (PMA.cells pma)
        filteredPMA = Vector.filter (\(zind, _) -> (zl <= zind && zind <= zr && isRelevant (ZIndex zl') (ZIndex zr') (ZIndex zind))) pmaCells

    rangeDMap :: DMap.Map Int v -> [(Coords n, v)]
    rangeDMap dmap = map (\(c, v) -> (fromZIndex' c, v)) (DMap.toList filteredMap)
      where
        (_, rmap) = DMap.split (zl - 1) dmap
        (lmap, _) = DMap.split (zr + 1) rmap
        filteredMap = DMap.filterWithKey (\k _ -> isRelevant (ZIndex zl') (ZIndex zr') (ZIndex k)) lmap

    rangeNS :: Map.NS Int v -> [(Coords n, v)]
    rangeNS Map.M0 = []
    rangeNS (Map.M1 as) = rangeChunk as
    rangeNS (Map.M2 as bs _ rest) = rangeChunk as ++ rangeChunk bs ++ rangeNS rest
    rangeNS (Map.M3 as bs cs _ rest) = rangeChunk as ++ rangeChunk bs ++ rangeChunk cs ++ rangeNS rest

    rangeChunk :: Map.Chunk Int v -> [(Coords n, v)]
    rangeChunk ch = map (\(c, v) -> (fromZIndex' c, v)) (Vector.toList filteredVector)
      where
        filteredVector = Vector.filter (\(zind, _) -> (zl <= zind && zind <= zr && isRelevant (ZIndex zl') (ZIndex zr') (ZIndex zind))) ch

rangeLookup :: Coords n -> Coords n -> Quadtree v -> [(Coords n, v)]
rangeLookup (Coords x1 y1) (Coords x2 y2) qt = rangeLookup'' (toZIndex cl) (toZIndex cr) qt
  where
    cl = Coords (min x1 x2) (min y1 y2)
    cr = Coords (max x1 x2) (max y1 y2)

rangeLookup' :: ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookup' zl zr qt = go zl zr qt ranges []
  where
    ranges = calculateRanges zl zr
    go :: ZIndex n -> ZIndex n -> Quadtree v -> [(ZIndex n, ZIndex n)] -> [(Coords n, v)] -> [(Coords n, v)]
    go zl' zr' qt' (r : rs) tmp = rangeLookupSeq' zl' zr' (fst r) (snd r) qt' ++ go zl' zr' qt' rs tmp
    go _ _ _ [] tmp = tmp

rangeLookup'' :: ZIndex n -> ZIndex n -> Quadtree v -> [(Coords n, v)]
rangeLookup'' (ZIndex zl) (ZIndex zr) qt = go qt zl zr zl 0 []
  where
    go :: Quadtree v -> Int -> Int -> Int -> Int -> [(Coords n, v)] -> [(Coords n, v)]
    go qt' l r p m tmp -- Upper left, bottom right, current position, misses count, temp. result
      | inBounds && shouldLookup = go qt' l r (p + 1) 0 tmp
      | m >= _MISSES_THRESHOLD && (litmax < p && p < bigmin) =
          go qt' bigmin r bigmin 0 tmp ++ rangeLookupSeq' (ZIndex zl) (ZIndex zr) (ZIndex l) (ZIndex litmax) qt'
      | m >= _MISSES_THRESHOLD && (p < litmax) =
          go qt' l litmax p m tmp ++ go qt' bigmin r bigmin 0 tmp
      | m >= _MISSES_THRESHOLD && (bigmin < p) -- 71
        =
          go qt' bigmin r p m tmp ++ rangeLookupSeq' (ZIndex zl) (ZIndex zr) (ZIndex l) (ZIndex litmax) qt'
      | inBounds = go qt' l r (p + 1) (m + 1) tmp
      | otherwise = rangeLookupSeq' (ZIndex zl) (ZIndex zr) (ZIndex l) (ZIndex r) qt' ++ tmp
      where
        inBounds = l <= p && p <= r
        shouldLookup = isRelevant (ZIndex l) (ZIndex r) (ZIndex p)
        (litmax, bigmin) = splitRegion' l r

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
      | m >= _MISSES_THRESHOLD && (bigmin < p) -- 71
        =
          go bigmin r p m tmp ++ [(ZIndex l, ZIndex litmax)]
      | inBounds = go l r (p + 1) (m + 1) tmp
      | otherwise = (ZIndex l, ZIndex r) : tmp
      where
        inBounds = l <= p && p <= r
        shouldLookup = isRelevant (ZIndex l) (ZIndex r) (ZIndex p)
        (litmax, bigmin) = splitRegion' l r

-- inserts data persistently
insertP :: Coords n -> v -> Quadtree v -> Quadtree v
insertP c v qt = Quadtree {getPMAMap = Map.insertP zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c

-- inserts data ephemerally
insertE :: Coords n -> v -> Quadtree v -> Quadtree v
insertE c v qt = Quadtree {getPMAMap = Map.insert zid v (getPMAMap qt)}
  where
    ZIndex zid = toZIndex c

randomPositions :: Int -> Int -> Int -> IO [(Int, Int)]
randomPositions count width height = do
  let gen = mkStdGen 126735
  return $ take count $ randomRs ((0, 0), (width - 1, height - 1)) gen

insertPoints :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPoints ((x, y) : points) val qt = insertPoints points val (insertE (Coords x y) val qt)
insertPoints [] _ qt = qt

randomPMQ :: Int -> Int -> Int -> IO (Quadtree String)
randomPMQ count width height = do
  positions <- randomPositions count width height
  return (insertPoints positions "test" empty)