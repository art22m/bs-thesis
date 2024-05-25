{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ

import Data.MapWrapped (MapWrapped)
import qualified Data.MapWrapped as MW

import Data.RTreeWrapped (RTreeWrapped)
import qualified Data.RTreeWrapped as RTW

import Data.QuadTreeWrapped (QuadTreeWrapped)
import qualified Data.QuadTreeWrapped as QTW

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
  benchInserts
  -- printPoints

printPoints :: IO() 
printPoints = do 
  let count = 5000
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
  let count = 1500000
  ul <- randomPositions' count 0 0 134217728 134217728
  ur <- randomPositions' count 134217728 0 268435456 134217728
  bl <- randomPositions' count 0 134217728 134217728 268435456
  br <- randomPositions' count 134217728 134217728 268435456 268435456

  let fromX = 67108864
  let fromY = 67108864
  let from = PMQ.Coords fromX fromY

  -- let toX = 201326592 - 1
  -- let toY = 201326592 - 1
  let toX = 70000000 
  let toY = 70000000
  let to = PMQ.Coords toX toY

  let pmq1 = insertPointsPMQ ul "t" PMQ.empty
  let pmq2 = insertPointsPMQ br "t" pmq1
  let pmq3 = insertPointsPMQ bl "t" pmq2
  let pmq4 = insertPointsPMQ ur "t" pmq3

  let mw1 = insertPointsMW ul "t" MW.empty
  let mw2 = insertPointsMW br "t" mw1
  let mw3 = insertPointsMW bl "t" mw2
  let mw4 = insertPointsMW ur "t" mw3

  let rtw1 = insertPointsRTW ul "t" RTW.empty
  let rtw2 = insertPointsRTW br "t" rtw1
  let rtw3 = insertPointsRTW bl "t" rtw2
  let rtw4 = insertPointsRTW ur "t" rtw3

  let qtw1 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") (QTW.empty 268435457 268435457 ((PMQ.Coords 0 0), "."))
  let qtw2 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw1
  let qtw3 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw2
  let qtw4 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw3

  -- let pmqTest = insertPointsPMQ ur "t" PMQ.empty
  -- print (testLookupSeq from to pmqTest)
  -- print (testLookupEff from to pmqTest)

  -- print (testLookupSeq from to pmq2)
  print (testLookupEff from to pmq2)
  -- print (testLookupMW from to mw2)
  print (testLookupRTW from to rtw2)
  print (testLookupQTW from to qtw2)

  -- print (testLookupSeq from to pmq4)
  -- print (testLookupEff from to pmq4)
  -- print (testLookupMW from to mw4)
  -- print (testLookupRTW from to rtw4)

  let benchName = "(" ++ show fromX ++"," ++ show fromY ++ "), (" ++ show toX ++ "," ++ show toY ++ "), count=" ++ show count
  defaultMain
    [ bgroup
        benchName
        [ 
          -- bench "Test PMQ Seq without bl, ur" $ whnf (testLookupSeq from to) pmq2,
          bench "Test PMQ nextZIndex withour bl, ur" $ whnf (testLookupEff from to) pmq2,
          -- bench "Test Map withour bl, ur" $ whnf (testLookupMW from to) mw2,
          bench "Test Data.RTree withour bl, ur" $ whnf (testLookupRTW from to) rtw2,
          bench "Test Data.QuadTree withour bl, ur" $ whnf (testLookupQTW from to) qtw2,

          -- bench "Test PMQ Seq with bl, ur" $ whnf (testLookupSeq from to) pmq4,
          bench "Test PMQ nextZIndex with bl, ur" $ whnf (testLookupEff from to) pmq4,
          -- bench "Test Map with bl, ur" $ whnf (testLookupMW from to) mw4,
          bench "Test Data.RTree with bl, ur" $ whnf (testLookupRTW from to) rtw4,
          bench "Test Data.QuadTree with bl, ur" $ whnf (testLookupQTW from to) qtw4
        ]
    ]

benchInserts :: IO() 
benchInserts = do 
  let count = 1000000
  !dt <- randomPositions' count 0 0 268435456 268435456

  let benchName = "count=" ++ show count
  defaultMain
    [ bgroup
        benchName
        [ 
          bench "Test PMQ inserts" $ whnf (testInsertPMQ dt "t") PMQ.empty,
          bench "Test Data.RTree inserts" $ whnf (testInsertRTW dt "t") RTW.empty
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

  mw1 <- generateAndInsertPointsMW 10 10000000 10000000 "data"
  mw2 <- generateAndInsertPointsMW 100 10000000 10000000 "data"
  mw3 <- generateAndInsertPointsMW 1000 10000000 10000000 "data"
  mw4 <- generateAndInsertPointsMW 10000 10000000 10000000 "data"
  mw5 <- generateAndInsertPointsMW 100000 10000000 10000000 "data"
  mw6 <- generateAndInsertPointsMW 1000000 10000000 10000000 "data"
  mw7 <- generateAndInsertPointsMW 10000000 10000000 10000000 "data"

  -- print (testLookupSeq _UL _BR pmq7)
  -- print (testLookupEff _UL _BR pmq7)
  -- print (testLookupMW _UL _BR mw7)
  
  defaultMain
    [ bgroup
        "1e7"
        [ bench "Test 10 seq" $ whnf (testLookupSeq _UL _BR) pmq1,
          bench "Test 10 eff" $ whnf (testLookupEff _UL _BR) pmq1,
          bench "Test 10 qmap" $ whnf (testLookupMW _UL _BR) mw1,
          bench "Test 100 seq" $ whnf (testLookupSeq _UL _BR) pmq2,
          bench "Test 100 eff" $ whnf (testLookupEff _UL _BR) pmq2,
          bench "Test 100 qmap" $ whnf (testLookupMW _UL _BR) mw2,
          bench "Test 1000 seq" $ whnf (testLookupSeq _UL _BR) pmq3,
          bench "Test 1000 eff" $ whnf (testLookupEff _UL _BR) pmq3,
          bench "Test 1000 qmap" $ whnf (testLookupMW _UL _BR) mw3,
          bench "Test 10_000 seq" $ whnf (testLookupSeq _UL _BR) pmq4,
          bench "Test 10_000 eff" $ whnf (testLookupEff _UL _BR) pmq4,
          bench "Test 10_000 qmap" $ whnf (testLookupMW _UL _BR) mw4,
          bench "Test 100_000 seq" $ whnf (testLookupSeq _UL _BR) pmq5,
          bench "Test 100_000 eff" $ whnf (testLookupEff _UL _BR) pmq5,
          bench "Test 100_000 qmap" $ whnf (testLookupMW _UL _BR) mw5,
          bench "Test 1_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq6,
          bench "Test 1_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq6,
          bench "Test 1_000_000 qmap" $ whnf (testLookupMW _UL _BR) mw6,
          bench "Test 10_000_000 seq" $ whnf (testLookupSeq _UL _BR) pmq7,
          bench "Test 10_000_000 eff" $ whnf (testLookupEff _UL _BR) pmq7,
          bench "Test 10_000_000 qmap" $ whnf (testLookupMW _UL _BR) mw7
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

insertPointsPMQ :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPointsPMQ ((x, y) : points) val !qt = insertPointsPMQ points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPointsPMQ [] _ !qt = qt

randomPositions :: Int -> Int -> Int -> IO [(Int, Int)]
randomPositions count width height = do
    let gen1 = mkStdGen 42
    let gen2 = mkStdGen 24
    let xs = randomRs (0, width) gen1
        ys = randomRs (0, height) gen2
    return $ take count $ zip xs ys

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
  let qt = insertPointsPMQ points val PMQ.empty
  return qt

generateAndInsertPoints' :: Int -> Int -> Int -> Int -> Int -> v -> IO (Quadtree v)
generateAndInsertPoints' count xl yl xr yr val = do
  points <- randomPositions' count xl yl xr yr
  let qt = insertPointsPMQ points val PMQ.empty
  return qt

insertPointsMW :: [(Int, Int)] -> v -> MapWrapped v -> MapWrapped v
insertPointsMW ((x, y) : points) val mw = insertPointsMW points val (MW.insert (PMQ.Coords x y) val mw)
insertPointsMW [] _ mw = mw

generateAndInsertPointsMW :: Int -> Int -> Int -> v -> IO (MapWrapped v)
generateAndInsertPointsMW count width height val = do
  points <- randomPositions count width height
  let mw = insertPointsMW points val MW.empty
  return mw

insertPointsRTW :: [(Int, Int)] -> v -> RTreeWrapped v -> RTreeWrapped v
insertPointsRTW ((x, y) : points) val rtw = insertPointsRTW points val (RTW.insert (PMQ.Coords x y) val rtw)
insertPointsRTW [] _ rtw = rtw

generateAndInsertPointsRTW :: Int -> Int -> Int -> v -> IO (RTreeWrapped v)
generateAndInsertPointsRTW count width height val = do
  points <- randomPositions count width height
  let rtw = insertPointsRTW points val RTW.empty
  return rtw

insertPointsQTW :: Eq v => [(Int, Int)] -> (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v)
insertPointsQTW ((x, y) : points) (coord, val) qtw = insertPointsQTW points (coord, val) (QTW.insert (PMQ.Coords x y) ((PMQ.Coords x y), val) qtw)
insertPointsQTW [] _ qtw = qtw

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

testLookupMW :: PMQ.Coords n -> PMQ.Coords n -> MapWrapped v -> Int
testLookupMW l r mw = length (MW.rangeLookup l r mw)

testLookupRTW :: PMQ.Coords n -> PMQ.Coords n -> RTreeWrapped v -> Int
testLookupRTW l r rtw = length (RTW.rangeLookup l r rtw)

testLookupQTW :: PMQ.Coords n -> PMQ.Coords n -> QuadTreeWrapped (PMQ.Coords n, v) -> Int
testLookupQTW l r qtw = length (QTW.rangeLookup l r qtw)


testInsertPMQ :: [(Int, Int)] -> v -> Quadtree v -> Int
testInsertPMQ points val !qt = 1
  where
    !qt' = insertPointsPMQ points val qt

-- testInsertMW :: MapWrapped v -> Int
testInsertRTW :: [(Int, Int)] -> v -> RTreeWrapped v -> Int
testInsertRTW points val !rtw = 1
  where
    !rtw' = insertPointsRTW points val rtw
-- testLookupQTW :: QuadTreeWrapped (PMQ.Coords n, v) -> Int