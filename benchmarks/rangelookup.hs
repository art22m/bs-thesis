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

main :: IO ()
main = do
--  benchDifferentQuadtrees
 benchDifferentQuadtreesWithDiffCount
  -- printPoints

printPoints :: IO() 
printPoints = do 
  let count = 5000
  ul <- randomPositions' count 0 0 (2^27) (2^27)
  ur <- randomPositions' count (2^27) 0 (2^28) (2^27)
  bl <- randomPositions' count 0 (2^27) (2^27) (2^28)
  br <- randomPositions' count (2^27) (2^27) (2^28) (2^28)

  print (ul ++ ur ++ bl ++ br)

benchDifferentQuadtrees :: IO()
benchDifferentQuadtrees = do
 -- 2^28 x 2^28
 let count = 25000
 ul <- randomPositions' count 0 0 (2^27) (2^27)
 ur <- randomPositions' count (2^27) 0 (2^28) (2^27)
 bl <- randomPositions' count 0 (2^27) (2^27) (2^28)
 br <- randomPositions' count (2^27) (2^27) (2^28) (2^28)

 let fromX = (2^26)
 let fromY = (2^26)
 let from = PMQ.Coords fromX fromY

 let toX = (2^27) - 1
 let toY = (2^27) - 1
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

 let qtw1 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
 let qtw2 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw1
 let qtw3 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw2
 let qtw4 = insertPointsQTW ul ((PMQ.Coords 0 0), "t") qtw3

 let benchName = "(" ++ show fromX ++"," ++ show fromY ++ "), (" ++ show toX ++ "," ++ show toY ++ "), count=" ++ show (count * 4)
 defaultMain
   [ bgroup
       benchName
       [
         bench "Test PMQ Seq without bl, ur" $ whnf (testLookupSeq from to) pmq2,
         bench "Test PMQ nextZIndex withour bl, ur" $ whnf (testLookupEff from to) pmq2,
         bench "Test Map withour bl, ur" $ whnf (testLookupMW from to) mw2,
         bench "Test Data.RTree withour bl, ur" $ whnf (testLookupRTW from to) rtw2,
         bench "Test Data.QuadTree withour bl, ur" $ whnf (testLookupQTW from to) qtw2,

         bench "Test PMQ Seq" $ whnf (testLookupSeq from to) pmq4,
         bench "Test PMQ Eff" $ whnf (testLookupEff from to) pmq4,
         bench "Test Map" $ whnf (testLookupMW from to) mw4,
         bench "Test Data.RTree" $ whnf (testLookupRTW from to) rtw4,
         bench "Test Data.QuadTree" $ whnf (testLookupQTW from to) qtw4
       ]
   ]

benchDifferentQuadtreesWithDiffCount :: IO()
benchDifferentQuadtreesWithDiffCount = do
  -- 2^28 x 2^28
  let fromX = (2^27) + 10
  let fromY = (2^26)
  let from = PMQ.Coords fromX fromY

  let toX = ((2^28) + (2^27)) `div` 2
  let toY = (2^28) `div` 2 - 10
  let to = PMQ.Coords toX toY

  points1k <- generatePoints4Quadrants 250
  let !pmq1k = insertPointsPMQ points1k "t" PMQ.empty
  let !mw1k = insertPointsMW points1k "t" MW.empty
  let !rtw1k = insertPointsRTW points1k "t" RTW.empty
  let !qtw1k = insertPointsQTW points1k ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))

  points10k <- generatePoints4Quadrants 2500
  let !pmq10k = insertPointsPMQ points10k "t" PMQ.empty
  let !mw10k = insertPointsMW points10k "t" MW.empty
  let !rtw10k = insertPointsRTW points10k "t" RTW.empty
  let !qtw10k = insertPointsQTW points10k ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))

  points100k <- generatePoints4Quadrants 25000
  let !pmq100k = insertPointsPMQ points100k "t" PMQ.empty
  let !mw100k = insertPointsMW points100k "t" MW.empty
  let !rtw100k = insertPointsRTW points100k "t" RTW.empty
  let !qtw100k = insertPointsQTW points100k ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))

  points1kk <- generatePoints4Quadrants 250000
  let !pmq1kk = insertPointsPMQ points1kk "t" PMQ.empty
  let !mw1kk = insertPointsMW points1kk "t" MW.empty
  let !rtw1kk = insertPointsRTW points1kk "t" RTW.empty
  let !qtw1kk = insertPointsQTW points1kk ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))

  points10kk <- generatePoints4Quadrants 2500000
  let !pmq10kk = insertPointsPMQ points10kk "t" PMQ.empty
  let !mw10kk = insertPointsMW points10kk "t" MW.empty
  let !rtw10kk = insertPointsRTW points10kk "t" RTW.empty

  defaultMain
    [
        bgroup "1k"
        [
          bench "PMQ Seq" $ whnf (testLookupSeq from to) pmq1k,
          bench "PMQ Eff" $ whnf (testLookupEff from to) pmq1k,
          bench "Data.Map" $ whnf (testLookupMW from to) mw1k,
          bench "Data.RTree" $ whnf (testLookupRTW from to) rtw1k,
          bench "Data.QuadTree" $ whnf (testLookupQTW from to) qtw1k
        ],

        bgroup "10k"
        [
          bench "PMQ Seq" $ whnf (testLookupSeq from to) pmq10k,
          bench "PMQ Eff" $ whnf (testLookupEff from to) pmq10k,
          bench "Data.Map" $ whnf (testLookupMW from to) mw10k,
          bench "Data.RTree" $ whnf (testLookupRTW from to) rtw10k,
          bench "Data.QuadTree" $ whnf (testLookupQTW from to) qtw10k
        ],

        bgroup "100k"
        [
          bench "PMQ Seq" $ whnf (testLookupSeq from to) pmq100k,
          bench "PMQ Eff" $ whnf (testLookupEff from to) pmq100k,
          bench "Data.Map" $ whnf (testLookupMW from to) mw100k,
          bench "Data.RTree" $ whnf (testLookupRTW from to) rtw100k,
          bench "Data.QuadTree" $ whnf (testLookupQTW from to) qtw100k
        ],

        bgroup "1kk"
        [
          bench "PMQ Seq" $ whnf (testLookupSeq from to) pmq1kk,
          bench "PMQ Eff" $ whnf (testLookupEff from to) pmq1kk,
          bench "Data.Map" $ whnf (testLookupMW from to) mw1kk,
          bench "Data.RTree" $ whnf (testLookupRTW from to) rtw1kk,
          bench "Data.QuadTree" $ whnf (testLookupQTW from to) qtw1kk
        ],

        bgroup "10kk"
        [
          bench "PMQ Seq" $ whnf (testLookupSeq from to) pmq10kk,
          bench "PMQ Eff" $ whnf (testLookupEff from to) pmq10kk,
          bench "Data.Map" $ whnf (testLookupMW from to) mw10kk,
          bench "Data.RTree" $ whnf (testLookupRTW from to) rtw10kk
        ]

    ]

generatePoints4Quadrants :: Int -> IO [(Int, Int)]
generatePoints4Quadrants count = do
    ul <- randomPositions' count 0 0 (2^27) (2^27)
    ur <- randomPositions' count (2^27) 0 (2^28) (2^27)
    bl <- randomPositions' count 0 (2^27) (2^27) (2^28)
    br <- randomPositions' count (2^27) (2^27) (2^28) (2^28)
    return (ul ++ ur ++ bl ++ br)

generateNPoints :: Int -> v -> Quadtree v -> Quadtree v
generateNPoints n = go 0
  where
    go :: Int -> v -> Quadtree v -> Quadtree v
    go p v' qt'
      | p < n && even p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | p < n && odd p = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'
      
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

insertPointsPMQ :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPointsPMQ ((x, y) : points) val !qt = insertPointsPMQ points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPointsPMQ [] _ !qt = qt

generateAndInsertPointsPMQ :: Int -> Int -> Int -> v -> IO (Quadtree v)
generateAndInsertPointsPMQ count width height val = do
  points <- randomPositions count width height
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