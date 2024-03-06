module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import System.Random
import qualified GHC.Generics as PMQ

main :: IO ()
main = do
  points1 <- randomPositions 10 10000000 10000000
  points2 <- randomPositions 100 10000000 10000000
  points3 <- randomPositions 1000 10000000 10000000
  points4 <- randomPositions 10000 10000000 10000000
  points5 <- randomPositions 100000 10000000 10000000
  points6 <- randomPositions 1000000 10000000 10000000
  points7 <- randomPositions 10000000 10000000 10000000
  points8 <- randomPositions 100000000 10000000 10000000
  points9 <- randomPositions 1000000000 10000000 10000000
  points10 <- randomPositions 10000000000 10000000 10000000
  points11 <- randomPositions 100000000000 10000000 10000000
  points12 <- randomPositions 1000000000000 10000000 10000000

  let pmq1 = insertPoints points1 "test" PMQ.empty
  let pmq2 = insertPoints points2 "test" PMQ.empty
  let pmq3 = insertPoints points3 "test" PMQ.empty
  let pmq4 = insertPoints points4 "test" PMQ.empty
  let pmq5 = insertPoints points5 "test" PMQ.empty
  let pmq6 = insertPoints points6 "test" PMQ.empty
  let pmq7 = insertPoints points7 "test" PMQ.empty
  let pmq8 = insertPoints points8 "test" PMQ.empty
  let pmq9 = insertPoints points9 "test" PMQ.empty
  let pmq10 = insertPoints points10 "test" PMQ.empty
  let pmq11 = insertPoints points11 "test" PMQ.empty
  let pmq12 = insertPoints points12 "test" PMQ.empty

  defaultMain
    [ bgroup
        "1e7"
        [ bench "Test 10 seq" $ whnf testLookupSeq pmq1,
          bench "Test 10 eff" $ whnf testLookupEff pmq1,
          bench "Test 100 seq" $ whnf testLookupSeq pmq2,
          bench "Test 100 eff" $ whnf testLookupEff pmq2,
          bench "Test 1000 seq" $ whnf testLookupSeq pmq3,
          bench "Test 1000 eff" $ whnf testLookupEff pmq3,
          bench "Test 10_000 seq" $ whnf testLookupSeq pmq4,
          bench "Test 10_000 eff" $ whnf testLookupEff pmq4,
          bench "Test 100_000 seq" $ whnf testLookupSeq pmq5,
          bench "Test 100_000 eff" $ whnf testLookupEff pmq5,
          bench "Test 1_000_000 seq" $ whnf testLookupSeq pmq6,
          bench "Test 1_000_000 eff" $ whnf testLookupEff pmq6,
          bench "Test 10_000_000 seq" $ whnf testLookupSeq pmq7,
          bench "Test 10_000_000 eff" $ whnf testLookupEff pmq7,
          bench "Test 100_000_000 seq" $ whnf testLookupSeq pmq8,
          bench "Test 100_000_000 eff" $ whnf testLookupEff pmq8,
          bench "Test 1_000_000_000 seq" $ whnf testLookupSeq pmq9,
          bench "Test 1_000_000_000 eff" $ whnf testLookupEff pmq9,
          bench "Test 10_000_000_000 seq" $ whnf testLookupSeq pmq10,
          bench "Test 10_000_000_000 eff" $ whnf testLookupEff pmq10,
          bench "Test 100_000_000_000 seq" $ whnf testLookupSeq pmq11,
          bench "Test 100_000_000_000 eff" $ whnf testLookupEff pmq11,
          bench "Test 1_000_000_000_000 seq" $ whnf testLookupSeq pmq12,
          bench "Test 1_000_000_000_000 eff" $ whnf testLookupEff pmq12
        ]
    ]

randomPositions :: Int -> Int -> Int -> IO[(Int, Int)]
randomPositions count width height = do
  let gen = mkStdGen 12345
  return $ take count $ randomRs ((0,0), (width-1,height-1)) gen

insertPoints :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPoints ((x, y):points) val qt = insertPoints points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPoints [] _ qt = qt

generateNPoints :: Int -> v -> Quadtree v -> Quadtree v
generateNPoints n = go 0
  where
    go :: Int -> v -> Quadtree v -> Quadtree v
    go p v' qt'
      | p < n && even p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | p < n && odd p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'

_UL :: PMQ.Coords n
_UL = PMQ.Coords 1000 1000 
_BR :: PMQ.Coords n
_BR = PMQ.Coords 400000 600000 

testLookupEff :: Quadtree v -> Int
testLookupEff qt = length (PMQ.rangeLookup _UL _BR qt)

testLookupDummy :: Quadtree v -> Int
testLookupDummy qt = length (PMQ.rangeLookupDummy _UL _BR qt)

testLookupSeq :: Quadtree v -> Int
testLookupSeq qt = length (PMQ.rangeLookupSeq _UL _BR qt)