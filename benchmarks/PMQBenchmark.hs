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

  let pmq1 = insertPoints points1 "test" PMQ.empty
  let pmq2 = insertPoints points2 "test" PMQ.empty
  let pmq3 = insertPoints points3 "test" PMQ.empty
  let pmq4 = insertPoints points4 "test" PMQ.empty
  let pmq5 = insertPoints points5 "test" PMQ.empty
  let pmq6 = insertPoints points6 "test" PMQ.empty
  let pmq7 = insertPoints points7 "test" PMQ.empty
  let pmq8 = insertPoints points8 "test" PMQ.empty

  defaultMain
    [ bgroup
        "1e7"
        [ bench "Test 10" $ whnf testLookupSeq pmq1,
          bench "Test 100" $ whnf testLookupSeq pmq2,
          bench "Test 1000" $ whnf testLookupSeq pmq3,
          bench "Test 10_000" $ whnf testLookupSeq pmq4,
          bench "Test 100_000" $ whnf testLookupSeq pmq5,
          bench "Test 1_000_000" $ whnf testLookupSeq pmq6
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