{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PackedMemoryArray where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude

-- | Data type
data PMA k v = PMA
  { capacity              :: Int              -- ^ Total capacity of PMA.
  , segmentCapacity       :: Int              -- ^ Size of each segment.
  , height                :: Int              -- ^ Height of the binary tree for elements.
  , elements              :: Vector (Maybe (k, v)) -- ^ Vector of all cells (elements or gaps).
  , cardinality           :: Int              -- ^ Number of elements contained.
  , segmentsCount         :: Int              -- ^ Number of segments
  , segmentsCardinalities :: Vector Int       -- ^ Number of elements contained in each segment.
  } deriving (Show, Eq, Foldable)

-- | Constants
minCapacity :: Integer
minCapacity = 8

-- Density tresholds
-- 0 <= p1 < ph <= th < t1
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

-- Length information 

null :: PMA k a -> Bool
null pma = (cardinality pma) == 0

size :: PMA k a -> Int
size pma = cardinality pma



-- | Methods

-- * insert

-- insertLookupWithKey :: forall k a. (Ord k) 
--                     => (k -> a -> a -> a) 
--                     -> k 
--                     -> a 
--                     -> PMA k a 
--                     -> (Maybe a, PMA k a)
-- insertLookupWithKey combine key val pma =
--     case lookupInsert of Nothing            -> (Nothing, updatedPMA)
--                          Just (pma, oldVal) -> (Just oldVal, pma)
--   where
--     lookupInsert = do
--       oldVal <- pma !? key
--       let combined = combine key val oldVal 
--       let insertIndex = (findPlaceIndex key pma)
--       let toInsert = Just (key, combined)
--       return (pma { 
--         elements = Vector.update (elements pma) (Vector.singleton (insertIndex, toInsert)) 
--         }, oldVal)

--     updatedPMA = if (((segmentsCardinalities newPMA) Vector.! segmentId) == (segmentCapacity pma)) then (rebalance newPMA segmentId) else newPMA

--     segmentId = findSegment pma key

--     (elements', posToInsert) = findPos (elements pma) ((segmentCapacity pma)*(segmentId) + ((segmentsCardinalities pma) Vector.! segmentId))
--       where
--         findPos :: Vector (Maybe (k, a)) -> Int -> (Vector (Maybe (k, a)), Int)
--         findPos vec pos = if (pos > 0) && ((Just key) < fmap fst (vec Vector.! (pos - 1)))
--                         then findPos (Vector.update vec (Vector.fromList [(pos - 1, Nothing), (pos, vec Vector.! (pos - 1))])) (pos - 1)
--                         else (vec, pos)

--     newElements = Vector.update elements' (Vector.singleton (posToInsert, Just (key, val)))

--     newPMA = PMA
--             { capacity = capacity pma
--             , segmentCapacity = segmentCapacity pma
--             , height = height pma
--             , elements = newElements
--             , cardinality = (cardinality pma) + 1
--             , segmentsCnt = segmentsCnt pma
--             , segmentsCardinalities = Vector.update (segmentsCardinalities pma) (Vector.singleton (segmentId, ((segmentsCardinalities pma) Vector.! segmentId) + 1))
--             }


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
findSegment pma key = if Data.PackedMemoryArray.null pma then 0 else find pma 0 (segmentsCount pma - 1)
  where
    find :: PMA k v -> Int -> Int -> Int
    find pma lb ub = if (lb < ub) then (find pma newLB newUB) else lb
      where
        mid = (lb + ub) `div` 2
        (newLB, newUB) =  if ((Just key) < fmap fst ((elements pma) Vector.! (mid * (segmentCapacity pma))))
                        then (lb, mid - 1)
                        else  if ((Just key) <= fmap fst ((elements pma) Vector.! ((mid * (segmentCapacity pma)) + ((segmentsCardinalities pma) Vector.! mid) - 1)))
                            then (mid, mid)
                            else (mid + 1, ub)

-- finds index where an element should be put
-- complexity – O (log (n / log n) + log n) = O (2 log (n) - log log n)
-- replace linear search to binary – get O (log (n / log n) + log log n) = O (log n)
findPlaceIndex :: Ord k => k -> PMA k a -> Int
findPlaceIndex val pma = start + linSearch (Vector.slice start len ((fmap fst) <$> elements pma))
  where
    segmentId = findSegment pma val
    start = segmentId * segmentCapacity pma
    len = segmentCapacity pma

    linSearch slice = fromMaybe
        (length slice - 1)
        (Vector.findIndex (>= Just val) slice)