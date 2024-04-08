module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.PackedMemoryQuadtree (Quadtree, QuadtreeDMap)
import qualified Data.PackedMemoryQuadtree as PMQ
import System.Random

_UL :: PMQ.Coords n
_UL = PMQ.Coords 1000 1000 -- 1047744

_BR :: PMQ.Coords n
_BR = PMQ.Coords 4000000 6000000 -- 644403355648

main :: IO ()
main = do
  -- benchDifferentRangeLookups
  benchDifferentQuadtrees

benchDifferentQuadtrees :: IO ()
benchDifferentQuadtrees = do
  pmq1 <- generateAndInsertPoints 10 10000000 10000000 "data"
  pmq2 <- generateAndInsertPoints 100 10000000 10000000 "data"
  pmq3 <- generateAndInsertPoints 1000 10000000 10000000 "data"
  pmq4 <- generateAndInsertPoints 10000 10000000 10000000 "data"
  pmq5 <- generateAndInsertPoints 100000 10000000 10000000 "data"
  pmq6 <- generateAndInsertPoints 1000000 10000000 10000000 "data"
  pmq7 <- generateAndInsertPoints 10000000 10000000 10000000 "data"
  pmq8 <- generateAndInsertPoints 100000000 10000000 10000000 "data"
  pmq9 <- generateAndInsertPoints 1000000000 10000000 10000000 "data"
  pmq10 <- generateAndInsertPoints 10000000000 10000000 10000000 "data"
  pmq11 <- generateAndInsertPoints 100000000000 10000000 10000000 "data"
  pmq12 <- generateAndInsertPoints 1000000000000 10000000 10000000 "data"
  pmq13 <- generateAndInsertPoints 10000000000000 10000000 10000000 "data"
  pmq14 <- generateAndInsertPoints 100000000000000 10000000 10000000 "data"
  pmq15 <- generateAndInsertPoints 1000000000000000 10000000 10000000 "data"

  qdm1 <- generateAndInsertPointsQDM 10 10000000 10000000 "data"
  qdm2 <- generateAndInsertPointsQDM 100 10000000 10000000 "data"
  qdm3 <- generateAndInsertPointsQDM 1000 10000000 10000000 "data"
  qdm4 <- generateAndInsertPointsQDM 10000 10000000 10000000 "data"
  qdm5 <- generateAndInsertPointsQDM 100000 10000000 10000000 "data"
  qdm6 <- generateAndInsertPointsQDM 1000000 10000000 10000000 "data"
  qdm7 <- generateAndInsertPointsQDM 10000000 10000000 10000000 "data"
  qdm8 <- generateAndInsertPointsQDM 100000000 10000000 10000000 "data"
  qdm9 <- generateAndInsertPointsQDM 1000000000 10000000 10000000 "data"
  qdm10 <- generateAndInsertPointsQDM 10000000000 10000000 10000000 "data"
  qdm11 <- generateAndInsertPointsQDM 100000000000 10000000 10000000 "data"
  qdm12 <- generateAndInsertPointsQDM 1000000000000 10000000 10000000 "data"
  qdm13 <- generateAndInsertPointsQDM 10000000000000 10000000 10000000 "data"
  qdm14 <- generateAndInsertPointsQDM 100000000000000 10000000 10000000 "data"
  qdm15 <- generateAndInsertPointsQDM 1000000000000000 10000000 10000000 "data"

  defaultMain
    [ bgroup
        "1e7"
        [ bench "Test 10 seq" $ whnf (testLookupSeq _UL _BR) pmq1,
          bench "Test 10 eff" $ whnf (testLookupEff _UL _BR) pmq1,
          bench "Test 10 qmap" $ whnf (testLookupQDM _UL _BR) qdm1,
          bench "Test 100 seq" $ whnf (testLookupSeq _UL _BR) pmq2,
          bench "Test 100 eff" $ whnf (testLookupEff _UL _BR) pmq2,
          bench "Test 100 qmap" $ whnf (testLookupQDM _UL _BR) qdm2,
          bench "Test 1000 seq" $ whnf (testLookupSeq _UL _BR) pmq3,
          bench "Test 1000 eff" $ whnf (testLookupEff _UL _BR) pmq3,
          bench "Test 1000 qmap" $ whnf (testLookupQDM _UL _BR) qdm3,
          bench "Test 10_000 seq" $ whnf (testLookupSeq _UL _BR) pmq4,
          bench "Test 10_000 eff" $ whnf (testLookupEff _UL _BR) pmq4,
          bench "Test 10_000 qmap" $ whnf (testLookupQDM _UL _BR) qdm4,
          bench "Test 100_000 seq" $ whnf (testLookupSeq _UL _BR) pmq5,
          bench "Test 100_000 eff" $ whnf (testLookupEff _UL _BR) pmq5,
          bench "Test 100_000 qmap" $ whnf (testLookupQDM _UL _BR) qdm5,
          bench "Test 1_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq6,
          bench "Test 1_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq6,
          bench "Test 1_000_000 qmap" $ whnf (testLookupQDM _UL _BR) qdm6,
          bench "Test 10_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq7,
          bench "Test 10_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq7,
          bench "Test 10_000_000 qmap" $ whnf (testLookupQDM _UL _BR) qdm7
        ]
    ]

benchDifferentRangeLookups :: IO ()
benchDifferentRangeLookups = do
  pmq1 <- generateAndInsertPoints 10 10000000 10000000 "data"
  pmq2 <- generateAndInsertPoints 100 10000000 10000000 "data"
  pmq3 <- generateAndInsertPoints 1000 10000000 10000000 "data"
  pmq4 <- generateAndInsertPoints 10000 10000000 10000000 "data"
  pmq5 <- generateAndInsertPoints 100000 10000000 10000000 "data"
  pmq6 <- generateAndInsertPoints 1000000 10000000 10000000 "data"
  pmq7 <- generateAndInsertPoints 10000000 10000000 10000000 "data"
  pmq8 <- generateAndInsertPoints 100000000 10000000 10000000 "data"
  pmq9 <- generateAndInsertPoints 1000000000 10000000 10000000 "data"
  pmq10 <- generateAndInsertPoints 10000000000 10000000 10000000 "data"
  pmq11 <- generateAndInsertPoints 100000000000 10000000 10000000 "data"
  pmq12 <- generateAndInsertPoints 1000000000000 10000000 10000000 "data"
  pmq13 <- generateAndInsertPoints 10000000000000 10000000 10000000 "data"
  pmq14 <- generateAndInsertPoints 100000000000000 10000000 10000000 "data"
  pmq15 <- generateAndInsertPoints 1000000000000000 10000000 10000000 "data"

  defaultMain
    [ bgroup
        "1e7"
        [ bench "Test 10 seq" $ whnf (testLookupSeq _UL _BR) pmq1,
          bench "Test 10 eff" $ whnf (testLookupEff _UL _BR) pmq1,
          bench "Test 100 seq" $ whnf (testLookupSeq _UL _BR) pmq2,
          bench "Test 100 eff" $ whnf (testLookupEff _UL _BR) pmq2,
          bench "Test 1000 seq" $ whnf (testLookupSeq _UL _BR) pmq3,
          bench "Test 1000 eff" $ whnf (testLookupEff _UL _BR) pmq3,
          bench "Test 10_000 seq" $ whnf (testLookupSeq _UL _BR) pmq4,
          bench "Test 10_000 eff" $ whnf (testLookupEff _UL _BR) pmq4,
          bench "Test 100_000 seq" $ whnf (testLookupSeq _UL _BR) pmq5,
          bench "Test 100_000 eff" $ whnf (testLookupEff _UL _BR) pmq5,
          bench "Test 1_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq6,
          bench "Test 1_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq6,
          bench "Test 10_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq7,
          bench "Test 10_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq7,
          bench "Test 100_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq8,
          bench "Test 100_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq8
          -- bench "Test 1_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq9,
          -- bench "Test 1_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq9,
          -- bench "Test 10_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq10,
          -- bench "Test 10_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq10,
          -- bench "Test 100_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq11,
          -- bench "Test 100_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq11,
          -- bench "Test 1_000_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq12,
          -- bench "Test 1_000_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq12,
          -- bench "Test 10_000_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq13,
          -- bench "Test 10_000_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq13,
          -- bench "Test 100_000_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq14,
          -- bench "Test 100_000_000_000_000 eff" $ whnf testLookupEff  _UL _BR pmq14,
          -- bench "Test 1_000_000_000_000_000 seq" $ whnf testLookupSeq  _UL _BR pmq15,
          -- bench "Test 1_000_000_000_000_000 eff" $ whnf testLookupEff pmq15
        ]
    ]

insertPoints :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPoints ((x, y) : points) val qt = insertPoints points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPoints [] _ qt = qt

randomPositions :: Int -> Int -> Int -> IO [(Int, Int)]
randomPositions count width height = do
  let gen = mkStdGen 12345
  return $ take count $ randomRs ((0, 0), (width - 1, height - 1)) gen

generateAndInsertPoints :: Int -> Int -> Int -> v -> IO (Quadtree v)
generateAndInsertPoints count width height val = do
  points <- randomPositions count width height
  let qt = insertPoints points val PMQ.empty
  return qt

insertPointsQDM :: [(Int, Int)] -> v -> QuadtreeDMap v -> QuadtreeDMap v
insertPointsQDM ((x, y) : points) val qt = insertPointsQDM points val (PMQ.insertQDM (PMQ.Coords x y) val qt)
insertPointsQDM [] _ qt = qt

generateAndInsertPointsQDM :: Int -> Int -> Int -> v -> IO (QuadtreeDMap v)
generateAndInsertPointsQDM count width height val = do
  points <- randomPositions count width height
  let qt = insertPointsQDM points val PMQ.emptyQDM
  return qt

generateNPoints :: Int -> v -> Quadtree v -> Quadtree v
generateNPoints n = go 0
  where
    go :: Int -> v -> Quadtree v -> Quadtree v
    go p v' qt'
      | p < n && even p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | p < n && odd p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'

testLookupEff :: PMQ.Coords n -> PMQ.Coords n -> Quadtree v -> Int
testLookupEff l r qt = length (PMQ.rangeLookup l r qt)

testLookupDummy :: PMQ.Coords n -> PMQ.Coords n -> Quadtree v -> Int
testLookupDummy l r qt = length (PMQ.rangeLookupDummy l r qt)

testLookupSeq :: PMQ.Coords n -> PMQ.Coords n -> Quadtree v -> Int
testLookupSeq l r qt = length (PMQ.rangeLookupSeq l r qt)

testLookupQDM :: PMQ.Coords n -> PMQ.Coords n -> QuadtreeDMap v -> Int
testLookupQDM l r qt = length (PMQ.rangeLookupQDM l r qt)