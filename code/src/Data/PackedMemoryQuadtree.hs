{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PackedMemoryQuadtree where

import Data.Bits 
import Data.List
import qualified Data.PackedMemoryArrayMap as PMAMap
import GHC.TypeLits (Nat)

---- Indexing

data Coords (n :: Nat) = Coords Int Int

newtype ZIndex (n :: Nat) = ZIndex Int
  deriving (Eq, Ord, Show)

zindexOf :: Coords n ->  ZIndex n
zindexOf (Coords x y) =
  ZIndex $ interleaveBinary x y

interleaveBinary :: Int -> Int -> Int
interleaveBinary x y = foldl' (\acc i -> acc .|. (shiftL (bitAt x i) (2 * i) .|. shiftL (bitAt y i) (2 * i + 1))) 0 shifts
  where
    shifts = [0..max (finiteBitSize x) (finiteBitSize y) - 1]
    
    bitAt :: Int -> Int -> Int 
    bitAt x' i = shiftR x' i .&. 1


---- Data Structure

data Quadtree a = Quadtree { getPMAMap :: PMAMap.Map Int a }
                deriving (Show)

null :: Quadtree v -> Bool
null qt = PMAMap.null (getPMAMap qt)

empty :: Quadtree v
empty = Quadtree { getPMAMap = PMAMap.empty }

singleton :: Coords n -> v -> Quadtree v
singleton c v = Quadtree { getPMAMap = PMAMap.singleton zid v }
    where 
        ZIndex zid = zindexOf c

lookup :: Coords n -> Quadtree v -> Maybe v
lookup c qt = PMAMap.lookup zid pm
    where 
        pm = getPMAMap qt
        ZIndex zid = zindexOf c

insert :: Coords n -> v -> Quadtree v -> Quadtree v
insert c v qt = Quadtree { getPMAMap = PMAMap.insertP zid v pm }
    where 
        pm = getPMAMap qt
        ZIndex zid = zindexOf c