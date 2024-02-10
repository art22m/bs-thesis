module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import System.Random

main :: IO ()
main = do
  points <- randomPositions 150000 50000 50000
  -- print points
  let pmq = insertPoints points "test" PMQ.empty

  print pmq

  defaultMain
    [ bgroup
        "fences tests"
        [ bench "Test Eff" $ whnf testLookupEff pmq,
          bench "Test Seq" $ whnf testLookupSeq pmq,
          bench "Test Dummy" $ whnf testLookupDummy pmq
        ]
    ]

randomPositions :: Int -> Int -> Int -> IO[(Int, Int)]
randomPositions count width height = do
  gen <- newStdGen
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

_UL :: Int
_UL = 265077696 -- (15k, 15k) --16763904 -- (4000, 4000)
_BR :: Int
_BR = 2049383168 -- (50k, 30k) -- 66007360 -- (7000, 8000)

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