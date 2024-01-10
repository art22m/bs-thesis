module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import System.Random

main :: IO ()
main = do
  let pmq = generateNPoints 10 "test-data" PMQ.empty
  defaultMain
    [ bgroup
        "fences tests"
        [ bench "Size 1 Test Dummy" $ whnf testLookupDummy pmq,
          bench "Size 1 Test Seq" $ whnf testLookupSeq pmq,
          bench "Size 1 Test Eff" $ whnf testLookupEff pmq
        ]
    ]

generateNPoints :: Int -> v -> Quadtree v -> Quadtree v
generateNPoints n = go 0
  where
    go :: Int -> v -> Quadtree v -> Quadtree v
    go p v' qt'
      | p < n = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'

_UL :: Int 
_UL = 12345
_BR :: Int
_BR = 624241

testLookupEff :: Quadtree v -> Int
testLookupEff qt = length (PMQ.rangeLookup zl zr qt)
  where
    zl = PMQ.fromZIndex' _UL
    zr = PMQ.fromZIndex' _BR

testLookupDummy :: Quadtree v -> Int
testLookupDummy qt = length (PMQ.rangeLookupDummy zl zr qt)
  where
    zl = PMQ.fromZIndex' _UL
    zr = PMQ.fromZIndex' _BR

testLookupSeq :: Quadtree v -> Int
testLookupSeq qt = length (PMQ.rangeLookupSeq zl zr qt)
  where
    zl = PMQ.fromZIndex' _UL
    zr = PMQ.fromZIndex' _BR