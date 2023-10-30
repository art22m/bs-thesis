{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PackedMemoryArray where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude

-- | Data type
data PMA k v = PMA
  { capacity              :: Int                   -- ^ Total capacity of PMA.
  , segmentCapacity       :: Int                   -- ^ Size of each segment.
  , height                :: Int                   -- ^ Height of the binary tree for elements.
  , elements              :: Vector (Maybe (k, v)) -- ^ Vector of all cells (elements or gaps).
  , cardinality           :: Int                   -- ^ Number of elements contained.
  , segmentsCount         :: Int                   -- ^ Number of segments
  , segmentsCardinalities :: Vector Int            -- ^ Number of elements contained in each segment.
  } deriving (Show, Eq, Foldable)

-- | Constants
minCapacity :: Integer
minCapacity = 8

-- Density tresholds
-- p0 < ... < ph < th < ... < t0
ph :: Double
ph = 0.3

th :: Double
th = 0.75

-- | Constructors

-- init construct PMA with given capacity
init :: Int -> PMA k a
init c = PMA
    {
        capacity = cap,
        segmentCapacity = cap,
        height = 1,
        elements = Vector.replicate cap Nothing,
        cardinality = 0,
        segmentsCount = 1,
        segmentsCardinalities = Vector.singleton 0 -- O(1) A vector with exactly one element.
    }
    where
        cap = max 8 (ceilPower2 c)


-- | Accessors 

null :: PMA k a -> Bool
null pma = (cardinality pma) == 0

size :: PMA k a -> Int
size pma = cardinality pma

-- | Methods

-- * lookup

lookup :: Ord k => k -> PMA k a -> Maybe a
lookup key pma = do
      Just (k, v) <- elementsVector Vector.!? index
      if k == key then Just v else Nothing
  where
    elementsVector = elements pma
    index = findPlaceIndex key pma


(!?) :: Ord k => PMA k a -> k -> Maybe a
(!?) = flip Data.PackedMemoryArray.lookup

-- | Helpers
log2 :: Int -> Double
log2 value = logBase 2 (fromIntegral value)

-- Round up to closest power of 2
ceilPower2 :: Int -> Int
ceilPower2 value = 2 ^ (ceiling (log2 value))

-- finds index of segment where and element is stored
findSegment :: forall k v. Ord k => PMA k v -> k -> Int
findSegment pma key 
  | Data.PackedMemoryArray.null pma = 0
  | otherwise = find pma 0 (segmentsCount pma - 1)
  where
    find :: PMA k v -> Int -> Int -> Int
    find pma lhs rhs 
      | lhs < rhs = find pma newlhs newrhs
      | otherwise = lhs
      where
        mid = (lhs + rhs) `div` 2
        elementIndex = mid * (segmentCapacity pma)
        (newlhs, newrhs) 
          | Just key < fmap fst ((elements pma) Vector.! elementIndex) = (lhs, mid - 1)
          | Just key <= fmap fst ((elements pma) Vector.! (elementIndex + ((segmentsCardinalities pma) Vector.! mid) - 1)) = (mid, mid)
          | otherwise = (mid + 1, rhs)

-- finds index where an element should be put
findPlaceIndex :: Ord k => k -> PMA k a -> Int
findPlaceIndex val pma = start + linSearch (Vector.slice start len ((fmap fst) <$> elements pma))
  where
    segmentId = findSegment pma val
    start = segmentId * segmentCapacity pma
    len = segmentCapacity pma

    -- TODO: BinSearch
    linSearch slice = fromMaybe 
        (length slice - 1)
        (Vector.findIndex (>= Just val) slice)