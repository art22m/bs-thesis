module Main where

import Data.List
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import Test.QuickCheck
import System.Random

main :: IO ()
main = do
  -- test1
  -- test2
  test3
  -- test4

test1 :: IO ()
test1 = do
  let coords1 = PMQ.Coords 5 5
  let coords2 = PMQ.Coords 9 8
  let coords3 = PMQ.Coords 6 6
  let coords4 = PMQ.Coords 15 7

  let pmq1 = PMQ.empty
  let pmq2 = PMQ.insertE coords1 "data c1" pmq1
  let pmq3 = PMQ.insertE coords2 "data c2" pmq2
  let pmq4 = PMQ.insertE coords3 "data c3" pmq3
  let pmq5 = PMQ.insertE coords4 "data c4" pmq4

  print "------"

  print coords1
  print (PMQ.toZIndex coords1)
  print coords2
  print (PMQ.toZIndex coords2)
  print coords3
  print (PMQ.toZIndex coords3)
  print coords4
  print (PMQ.toZIndex coords4)

  print "------"

  print (PMQ.getPMAMap pmq5)

  print (PMQ.rangeLookupDummy coords1 coords2 pmq5)
  print (PMQ.rangeLookupSeq coords1 coords2 pmq5)

  print (PMQ.calculateRanges (PMQ.toZIndex coords1) (PMQ.toZIndex coords2))
  print (PMQ.rangeLookup coords1 coords2 pmq5)

test2 :: IO ()
test2 = do
  let points_num = 2

  let pmqEmpty = PMQ.empty
  let pmq = generateNPoints points_num "t" pmqEmpty

  let zl = PMQ.ZIndex 2
  let zr = PMQ.ZIndex 104

  let l = PMQ.fromZIndex zl
  let r = PMQ.fromZIndex zr

  print pmq
  print (PMQ.rangeLookupDummy l r pmq)
  print (PMQ.rangeLookupSeq l r pmq)
  print (PMQ.rangeLookup l r pmq)
  print (PMQ.calculateRanges zl zr)

test3 :: IO ()
test3 = quickCheck (within 1000000 (withMaxSuccess 50000 testRangeLookup))

test4 :: IO()
test4 = do
  points <- randomPositions 3000 10000 10000
  let pmq = insertPoints points "test" PMQ.empty
  quickCheck (within 1000000 (withMaxSuccess 50000 (testRangeLookup' pmq)))

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
      | p < n = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'

testRangeLookup :: Int -> Int -> Bool
testRangeLookup l r = compareResults resl resr
  where
    zl = PMQ.fromZIndex' l
    zr = PMQ.fromZIndex' r

    qt = generateNPoints 10001 "t" PMQ.empty

    resl = PMQ.rangeLookupDummy zl zr qt
    resr = PMQ.rangeLookup zl zr qt

testRangeLookup' :: Quadtree v -> (Int -> Int -> Bool)
testRangeLookup' qt = go
  where
    go :: Int -> Int -> Bool
    go l r = compareResults resl resr
      where
        zl = PMQ.fromZIndex' l
        zr = PMQ.fromZIndex' r

        resl = PMQ.rangeLookupDummy zl zr qt
        resr = PMQ.rangeLookup zl zr qt

compareResults :: [(PMQ.Coords n, v)] -> [(PMQ.Coords n, v)] -> Bool
compareResults l r = customEqual (sortBy customCompare l) (sortBy customCompare r)
  where
    customEqual :: [(PMQ.Coords n, v)] -> [(PMQ.Coords n, v)] -> Bool
    customEqual [] [] = True
    customEqual [] _ = False
    customEqual _ [] = False
    customEqual ((x,_):xs) ((y,_):ys) = PMQ.toZIndex x == PMQ.toZIndex y && customEqual xs ys

    customCompare :: (PMQ.Coords n, v) -> (PMQ.Coords n, v) -> Ordering
    customCompare (x,_) (y,_) = compare (PMQ.toZIndex x) (PMQ.toZIndex y)