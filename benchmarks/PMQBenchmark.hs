module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import System.Random
import qualified GHC.Generics as PMQ

main :: IO ()
main = do
  points <- randomPositions 200 800 800
  -- print points
  let pmq = insertPoints points "test" PMQ.empty

  print pmq

  defaultMain
    [ bgroup
        "50k 15k 15k 50 50"
        [ bench "Test Eff" $ whnf testLookupEff pmq,
          bench "Test Seq" $ whnf testLookupSeq pmq,
          bench "Test Dummy" $ whnf testLookupDummy pmq
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
_UL = PMQ.Coords 10 10 -- (5k, 8k) --16763904 -- (4000, 4000)
_BR :: PMQ.Coords n
_BR = PMQ.Coords 500 500 -- (10k, 10k) -- 66007360 -- (7000, 8000)

testLookupEff :: Quadtree v -> Int
testLookupEff qt = length (PMQ.rangeLookup _UL _BR qt)

testLookupDummy :: Quadtree v -> Int
testLookupDummy qt = length (PMQ.rangeLookupDummy _UL _BR qt)

testLookupSeq :: Quadtree v -> Int
testLookupSeq qt = length (PMQ.rangeLookupSeq _UL _BR qt)