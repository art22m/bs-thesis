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
   benchInserts

benchInserts :: IO()
benchInserts = do
  points1k <- generatePoints4Quadrants 250
  points10k <- generatePoints4Quadrants 2500
  points100k <- generatePoints4Quadrants 25000
  points1kk <- generatePoints4Quadrants 250000
  points10kk <- generatePoints4Quadrants 2500000

  defaultMain
    [
        bgroup "1k"
        [
          bench "PMQ" $ whnf (testInsertPMQ points1k "t") PMQ.empty,
          bench "Data.Map" $ whnf (testInsertMW points1k "t") MW.empty,
          bench "Data.RTree" $ whnf (testInsertRTW points1k "t") RTW.empty,
          bench "Data.QuadTree" $ whnf (testInsertQTW points1k ((PMQ.Coords 0 0), "t")) (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
        ],

--        bgroup "10k"
--        [
--          bench "PMQ" $ whnf (testInsertPMQ points10k "t") PMQ.empty,
--          bench "Data.Map" $ whnf (testInsertMW points10k "t") MW.empty,
--          bench "Data.RTree" $ whnf (testInsertRTW points10k "t") RTW.empty,
--          bench "Data.QuadTree" $ whnf (testInsertQTW points10k ((PMQ.Coords 0 0), "t")) (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
--        ],
--
--        bgroup "100k"
--        [
--          bench "PMQ" $ whnf (testInsertPMQ points100k "t") PMQ.empty,
--          bench "Data.Map" $ whnf (testInsertMW points100k "t") MW.empty,
--          bench "Data.RTree" $ whnf (testInsertRTW points100k "t") RTW.empty,
--          bench "Data.QuadTree" $ whnf (testInsertQTW points100k ((PMQ.Coords 0 0), "t")) (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
--        ],
--
--        bgroup "1kk"
--        [
--          bench "PMQ" $ whnf (testInsertPMQ points1kk "t") PMQ.empty,
--          bench "Data.Map" $ whnf (testInsertMW points1kk "t") MW.empty,
--          bench "Data.RTree" $ whnf (testInsertRTW points1kk "t") RTW.empty,
--          bench "Data.QuadTree" $ whnf (testInsertQTW points1kk ((PMQ.Coords 0 0), "t")) (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
--        ],

        bgroup "10kk"
        [
          bench "PMQ" $ whnf (testInsertPMQ points10kk "t") PMQ.empty,
          bench "Data.Map" $ whnf (testInsertMW points10kk "t") MW.empty,
          bench "Data.RTree" $ whnf (testInsertRTW points10kk "t") RTW.empty,
          bench "Data.QuadTree" $ whnf (testInsertQTW points10kk ((PMQ.Coords 0 0), "t")) (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))
        ]

    ]

generatePoints4Quadrants :: Int -> IO [(Int, Int)]
generatePoints4Quadrants count = do
    ul <- randomPositions' count 0 0 (2^27) (2^27)
    ur <- randomPositions' count (2^27) 0 (2^28) (2^27)
    bl <- randomPositions' count 0 (2^27) (2^27) (2^28)
    br <- randomPositions' count (2^27) (2^27) (2^28) (2^28)
    return (ul ++ ur ++ bl ++ br)

randomPositions' :: Int -> Int -> Int -> Int -> Int -> IO [(Int, Int)]
randomPositions' count xl yl xr yr = do
    let gen1 = mkStdGen 42
    let gen2 = mkStdGen 24
    let xs = randomRs (xl, xr) gen1
        ys = randomRs (yl, yr) gen2
    return $ take count $ zip xs ys

testInsertPMQ :: [(Int, Int)] -> v -> Quadtree v -> Int
testInsertPMQ points val pmq = 1
  where
    !pmq' = insertPointsPMQ points val PMQ.empty

testInsertMW :: [(Int, Int)] -> v -> MapWrapped v -> Int
testInsertMW points val !mw = 1
  where
    !mw' = insertPointsMW points val mw

testInsertRTW :: [(Int, Int)] -> v -> RTreeWrapped v -> Int
testInsertRTW points val !rtw = 1
  where
    !rtw' = insertPointsRTW points val rtw

testInsertQTW :: [(Int, Int)] -> (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v) -> Int
testInsertQTW points val !qtw = 1
  where
    !qtw' = insertPointsQTW points ((PMQ.Coords 0 0), "t") (QTW.empty (2^28) (2^28) ((PMQ.Coords 0 0), "."))

insertPointsPMQ :: [(Int, Int)] -> v -> Quadtree v -> Quadtree v
insertPointsPMQ ((x, y) : points) val !qt = insertPointsPMQ points val (PMQ.insertE (PMQ.Coords x y) val qt)
insertPointsPMQ [] _ !qt = qt

insertPointsMW :: [(Int, Int)] -> v -> MapWrapped v -> MapWrapped v
insertPointsMW ((x, y) : points) val mw = insertPointsMW points val (MW.insert (PMQ.Coords x y) val mw)
insertPointsMW [] _ mw = mw

insertPointsRTW :: [(Int, Int)] -> v -> RTreeWrapped v -> RTreeWrapped v
insertPointsRTW ((x, y) : points) val rtw = insertPointsRTW points val (RTW.insert (PMQ.Coords x y) val rtw)
insertPointsRTW [] _ rtw = rtw

insertPointsQTW :: Eq v => [(Int, Int)] -> (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v) -> QuadTreeWrapped (PMQ.Coords n, v)
insertPointsQTW ((x, y) : points) (coord, val) qtw = insertPointsQTW points (coord, val) (QTW.insert (PMQ.Coords x y) ((PMQ.Coords x y), val) qtw)
insertPointsQTW [] _ qtw = qtw