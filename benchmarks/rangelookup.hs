{-# LANGUAGE BangPatterns #-}
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
  -- benchDifferentQuadtrees
  -- benchNoUpperLeft
  printPoints

printPoints :: IO() 
printPoints = do 
  let count = 500000
  ul <- randomPositions' count 0 0 134217728 134217728
  ur <- randomPositions' count 134217728 0 268435456 134217728
  bl <- randomPositions' count 0 134217728 134217728 268435456
  br <- randomPositions' count 134217728 134217728 268435456 268435456

  print (ul ++ ur ++ bl ++ br)

  -- pos <- randomPositions 10 268435456 268435456
  -- print pos

benchNoUpperLeft :: IO() 
benchNoUpperLeft = do 
  -- 2^28 x 2^28
  -- 268435456 >> 1 == 134217728
  -- no_<missed_quadrant>_<quadrants_in_range>_<number_of_data>
  let count = 5000000
  ul <- randomPositions' count 0 0 134217728 134217728
  ur <- randomPositions' count 134217728 0 268435456 134217728
  bl <- randomPositions' count 0 134217728 134217728 268435456
  br <- randomPositions' count 134217728 134217728 268435456 268435456

  let fromX = 67108864
  let fromY = 67108864
  let from = PMQ.Coords fromX fromY

  let toX = 201326592
  let toY = 201326592
  let to = PMQ.Coords toX toY

  let pmq1 = insertPoints ul "t" PMQ.empty
  let pmq2 = insertPoints br "t" pmq1
  let pmq3 = insertPoints bl "t" pmq2
  let pmq4 = insertPoints ur "t" pmq3

  let qdm1 = insertPointsQDM ul "t" PMQ.emptyQDM
  let qdm2 = insertPointsQDM br "t" qdm1
  let qdm3 = insertPointsQDM bl "t" qdm2
  let qdm4 = insertPointsQDM ur "t" qdm3

  -- let pmqTest = insertPoints ur "t" PMQ.empty
  -- print (testLookupSeq from to pmqTest)
  -- print (testLookupEff from to pmqTest)

  print (testLookupSeq from to pmq2)
  print (testLookupEff from to pmq2)
  print (testLookupQDM from to qdm2)

  print (testLookupSeq from to pmq4)
  print (testLookupEff from to pmq4)
  print (testLookupQDM from to qdm4)

  let benchName = "(" ++ show fromX ++"," ++ show fromY ++ "), (" ++ show toX ++ "," ++ show toY ++ "), count=" ++ show count
  defaultMain
    [ bgroup
        benchName
        [ bench "Test seq without bl, ur" $ whnf (testLookupSeq from to) pmq2,
          bench "Test eff withour bl, ur" $ whnf (testLookupEff from to) pmq2,
          bench "Test qdm withour bl, ur" $ whnf (testLookupQDM from to) qdm2,

          bench "Test seq with bl, ur" $ whnf (testLookupSeq from to) pmq4,
          bench "Test eff with bl, ur" $ whnf (testLookupEff from to) pmq4,
          bench "Test qdm with bl, ur" $ whnf (testLookupQDM from to) qdm4
        ]
    ]

benchDifferentQuadtrees :: IO ()
benchDifferentQuadtrees = do
  pmq1 <- generateAndInsertPoints 10 10000000 10000000 "data"
  pmq2 <- generateAndInsertPoints 100 10000000 10000000 "data"
  pmq3 <- generateAndInsertPoints 1000 10000000 10000000 "data"
  pmq4 <- generateAndInsertPoints 10000 10000000 10000000 "data"
  pmq5 <- generateAndInsertPoints 100000 10000000 10000000 "data"
  pmq6 <- generateAndInsertPoints 1000000 10000000 10000000 "data"
  pmq7 <- generateAndInsertPoints 10000000 10000000 10000000 "data"

  qdm1 <- generateAndInsertPointsQDM 10 10000000 10000000 "data"
  qdm2 <- generateAndInsertPointsQDM 100 10000000 10000000 "data"
  qdm3 <- generateAndInsertPointsQDM 1000 10000000 10000000 "data"
  qdm4 <- generateAndInsertPointsQDM 10000 10000000 10000000 "data"
  qdm5 <- generateAndInsertPointsQDM 100000 10000000 10000000 "data"
  qdm6 <- generateAndInsertPointsQDM 1000000 10000000 10000000 "data"
  qdm7 <- generateAndInsertPointsQDM 10000000 10000000 10000000 "data"

  -- print (testLookupSeq _UL _BR pmq7)
  -- print (testLookupEff _UL _BR pmq7)
  -- print (testLookupQDM _UL _BR qdm7)
  
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
        ]
    ]

insertPoints :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPoints ((x, y) : points) val !qt = insertPoints points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPoints [] _ !qt = qt

randomPositions :: Int -> Int -> Int -> IO [(Int, Int)]
randomPositions count width height = do
  let gen = mkStdGen 42
  return $ take count $ randomRs ((0, 0), (width - 1, height - 1)) gen

randomPositions' :: Int -> Int -> Int -> Int -> Int -> IO [(Int, Int)]
randomPositions' count xl yl xr yr = do
    let gen1 = mkStdGen 42
    let gen2 = mkStdGen 24
    let xs = randomRs (xl, xr) gen1
        ys = randomRs (yl, yr) gen2
    return $ take count $ zip xs ys

generateAndInsertPoints :: Int -> Int -> Int -> v -> IO (Quadtree v)
generateAndInsertPoints count width height val = do
  points <- randomPositions count width height
  let qt = insertPoints points val PMQ.empty
  return qt

generateAndInsertPoints' :: Int -> Int -> Int -> Int -> Int -> v -> IO (Quadtree v)
generateAndInsertPoints' count xl yl xr yr val = do
  points <- randomPositions' count xl yl xr yr
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