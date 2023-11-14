{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PackedMemoryQuadtree where

import Data.Char (chr, ord)
import Data.Ord (comparing)
import qualified Data.PackedMemoryArrayMap as PMAMap
import GHC.TypeLits (KnownNat, Nat)
import Numeric (readInt, showIntAtBase)

---- Indexing

data Coords (n :: Nat) = Coords Int Int

newtype ZIndex (n :: Nat) = ZIndex Int
  deriving (Eq, Ord, Show)

showBinary :: Int -> String
showBinary n = showIntAtBase 2 (\d -> chr (d + ord '0')) n ""

readBinary :: String -> Int
readBinary = fst . head . readInt 2 (const True) (\c -> ord c - ord '0')

interleaveBinary :: String -> String -> String
interleaveBinary xs ys = concat (alignWith (\x y -> x : y : "") xs ys)
  where
    alignWith f [] ys' = map (f '0') ys'
    alignWith f xs' [] = map (`f` '0') xs'
    alignWith f (x : xs') (y : ys') = f x y : alignWith f xs' ys'

zindexOf :: Coords n -> ZIndex n
zindexOf (Coords x y) =
  ZIndex $
    readBinary (reverse (interleaveBinary (reverse (showBinary x)) (reverse (showBinary y))))

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