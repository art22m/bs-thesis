{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.PackedMemoryArrayMap where

import Prelude hiding (lookup)
import qualified Data.PackedMemoryArray as PMA
import Data.PackedMemoryArray (PMA)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Control.Monad.ST (runST)
import qualified Data.Map as DMap

_THRESHOLD :: Int
_THRESHOLD = 1

data Stream a = forall s . Stream !(s -> Step a s) !s

instance Show a => Show (Stream a) where
  show s = show . toList_s $ s

data Step a s
  = Done
  | Yield a s
  | Skip s
  deriving (Show, Functor)

{-# INLINE empty_s #-}
empty_s :: Stream a
empty_s = Stream (const Done) ()

{-# INLINE cons_s #-}
cons_s :: a -> Stream a -> Stream a
cons_s !x (Stream next !s) = Stream next' (Left x)
  where
    next' (Left y)  = Yield y (Right s)
    next' (Right s) = case next s of
      Done       -> Done
      Skip s'    -> Skip (Right s')
      Yield a s' -> Yield a (Right s')

{-# INLINE toList_s #-}
toList_s :: Stream a -> [a]
toList_s (Stream next !s0) = unfold s0
  where
    unfold !s = case next s of
      Done       -> []
      Skip s'    -> unfold s'
      Yield x s' -> x : unfold s'

dumpStream :: (Ord k) => Map k a -> Map k a
dumpStream !m = m { getPMA = insertStream (inputStream m) (getPMA m), inputStream = empty_s }

insertStream :: (Ord k) => Stream (k, a) -> PMA k a -> PMA k a
insertStream !stream !pma = PMA.fromList . toList_s $ stream

data Map k a = Map
  { getNS       :: !(NS k a)
  , getNsSize   :: !Int
  , getPMA      :: !(PMA k a)
  , getMap      :: !(DMap.Map k a)
  , inputStream :: !(Stream (k, a))
  } deriving Show

type Chunk k a = Vector (k, a)

data NS k a
  = M0
  | M1 !(Vector (k, a))
  | M2 !(Vector (k, a)) !(Vector (k, a)) !(Vector (k, a)) !(NS k a)
  | M3 !(Vector (k, a)) !(Vector (k, a)) !(Vector (k, a)) !(Vector (k, a)) !(NS k a)

instance (Show k, Show a) => Show (NS k a) where
  show M0                = "0 "
  show (M1 as)           = "1 " ++ show (length as) ++ " "
  show (M2 as bs _ m)    = "2 " ++ show (length as) ++ " " ++ show (length bs) ++ " " ++ show m
  show (M3 as bs cs _ m) = "3 " ++ show (length as) ++ " " ++ show (length bs) ++ " " ++ show (length cs) ++ " " ++ show m

null :: Map k v -> Bool
null !m = DMap.null (getMap m) && PMA.null (getPMA m) && nullNS (getNS m)
  where
    nullNS M0 = True
    nullNS _  = False

empty :: Map k v
empty = Map
  { getNS       = M0
  , getNsSize   = 0
  , getPMA      = PMA.empty
  , getMap      = DMap.empty
  , inputStream = empty_s
  }

singleton :: k -> v -> Map k v
singleton !k !v = Map
  { getNS       = M0
  , getNsSize   = 1
  , getPMA      = PMA.empty
  , getMap      = DMap.singleton k v
  , inputStream = empty_s
  }

{-# INLINE find #-}
find :: (Ord k) => k -> Map k v -> Maybe v
find !k !m = lookup k . dumpStream . dumpPMA $ m

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup !k !m = maybe fromNS Just (maybe fromPMA Just fromMap)
  where
    fromMap = DMap.lookup k (getMap m)
    fromPMA = PMA.lookup k (getPMA m)
    fromNS = go (getNS m)

    go !M0                = Nothing
    go !(M1 as)           = lookup1 k as Nothing
    go !(M2 as bs _ rest) = lookup1 k as $ lookup1 k bs $ go rest
    go !(M3 as bs cs _ rest) = lookup1 k as $ lookup1 k bs $ lookup1 k cs $ go rest

lookup1 :: (Ord k) => k -> Chunk k v -> Maybe v -> Maybe v
lookup1 !k !chunk !r
  | Just v <- linSearch k chunk = Just v
  | otherwise = r
  where
    linSearch !tf !vector = snd <$> Vector.find (\(k', _) -> k' == tf) vector

merge :: (Ord k) => Chunk k a -> Chunk k a -> Chunk k a
merge !v1 !v2 = runST $ do
  mv1 <- Vector.thaw v1
  mv2 <- Vector.thaw v2
  fv <- MVector.new (MVector.length mv1 + MVector.length mv2)
  mergeM mv1 mv2 fv
  Vector.freeze fv
  where
    mergeM !mv1 !mv2 !fv
      | MVector.null mv1 && MVector.null mv2 = return ()
      | MVector.null mv1 = do
        val <- MVector.read mv2 0
        MVector.write fv 0 val
        mergeM mv1 (MVector.tail mv2) (MVector.tail fv)
      | MVector.null mv2 = do
        val <- MVector.read mv1 0
        MVector.write fv 0 val
        mergeM (MVector.tail mv1) mv2 (MVector.tail fv)
      | otherwise = do
        val1 <- MVector.read mv1 0
        val2 <- MVector.read mv2 0
        if fst val1 < fst val2
          then do
            MVector.write fv 0 val1
            mergeM (MVector.tail mv1) mv2 (MVector.tail fv)
          else do
            MVector.write fv 0 val2
            mergeM mv1 (MVector.tail mv2) (MVector.tail fv)

chunkFromMap :: (Ord k) => DMap.Map k v -> Chunk k v
chunkFromMap = Vector.fromList . DMap.assocs

insertP :: (Ord k) => k -> v -> Map k v -> Map k v
insertP !k0 !v0 !m
  | n0 < _THRESHOLD = dumpPMA $! m { getMap = DMap.insert k0 v0 (getMap m) }
  | otherwise       = dumpPMA $! m { getMap = DMap.singleton k0 v0
                                   , getNS  = summ (chunkFromMap (getMap m)) (getNS m)
                                   , getNsSize = getNsSize m + 1
                                   }
 where
  n0 = DMap.size (getMap m)

insertE :: (Ord k) => k -> a -> Map k a -> Map k a
insertE !k !a !m 
  | position (PMA.cardinality (getPMA m) + 1) > order (getNS m) = insertE k a $! dumpPMA m
  | otherwise                                                   = newMap
  where
    order M0                = 0
    order (M1 _)            = 0
    order (M2 _ _ _ rest)   = order rest + 1
    order (M3 _ _ _ _ rest) = order rest + 1
    newMap = m { getPMA = PMA.insert k a (getPMA m) }

{-# INLINE insert #-}
insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert !k !a !map' = new $! insertPMA k a (dumpPMA map')

{-# NOINLINE insertPMA #-}
insertPMA :: (Ord k) => k -> a -> Map k a -> Map k a
insertPMA !k !a !map = map { getPMA = PMA.insert k a (getPMA map) }

{-# NOINLINE new #-}
new :: Map k a -> Map k a
new !pma = pma

dumpPMA :: (Ord k) => Map k a -> Map k a
dumpPMA !m
  | len < _THRESHOLD = m
  | otherwise        = m { getNS  = skipModify (position len) (summ $ pmaToVec pma) ns
                         , getPMA = PMA.empty
                         , getNsSize = getNsSize m + 2 ^ (position len)
                         }
  where
    pmaToVec = Vector.catMaybes . PMA.elements

    ns = getNS m
    pma = getPMA m
    len = PMA.cardinality pma

    skipModify 0 !f !ns'                  = f ns'
    skipModify _ _ M0                   = error "too far skip"
    skipModify _ _ (M1 _)               = error "too far skip"
    skipModify n !f (M2 as bs ab's rest)    = M2 as bs ab's (skipModify (n-1) f rest)
    skipModify n !f (M3 as bs cs bc's rest) = M3 as bs cs bc's (skipModify (n-1) f rest)

position :: Int -> Int
position !size = floor (logBase (2 :: Double) (fromIntegral (size `div` _THRESHOLD)))

summ :: (Ord k) => Chunk k a -> NS k a -> NS k a
summ !as M0                 = M1 as
summ !as (M1 bs)            = M2 as bs (merge as bs) M0
summ !as (M2 bs cs bcs xs)  = M3 as bs cs bcs xs
summ !as (M3 bs _ _ cds xs) = cds `seq` M2 as bs (merge as bs) (summ cds xs)
