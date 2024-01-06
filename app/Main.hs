module Main where

import qualified Data.PackedMemoryArray as PMA
import Data.PackedMemoryQuadtree (Quadtree)
import qualified Data.PackedMemoryQuadtree as PMQ
import HaskellSay (haskellSay)

generateNPoints :: Int -> v -> Quadtree v -> Quadtree v
generateNPoints n = go 0
  where
    go :: Int -> v -> Quadtree v -> Quadtree v
    go p v' qt'
      | p < n = go (p + 1) v' (PMQ.insertE (PMQ.Coords p p) v' qt')
      | otherwise = qt'

main :: IO ()
main = do
  putStrLn "start"
  --   test1
  test2
  putStrLn "end"

test1 :: IO ()
test1 = do
  let coords1 = PMQ.Coords 0 0
  let coords2 = PMQ.Coords 1 1
  let coords3 = PMQ.Coords 3 2
  let coords4 = PMQ.Coords 4 4

  let pmq1 = PMQ.empty
  let pmq2 = PMQ.insertE coords1 "data c1" pmq1
  let pmq3 = PMQ.insertP coords2 "data c2" pmq2
  let pmq4 = PMQ.insertP coords4 "data c4" pmq3

  print (PMQ.lookup coords1 pmq4)
  print (PMQ.lookup coords2 pmq4)
  print (PMQ.lookup coords3 pmq4)
  print (PMQ.lookup coords4 pmq4)

  print ("------")

  print (PMQ.toZIndex coords1)
  print (PMQ.toZIndex coords2)
  print (PMQ.toZIndex coords3)
  print (PMQ.toZIndex coords4)

  print ("------")

  print (PMQ.getPMAMap pmq4)

  print (PMQ.rangeLookupDummiest coords1 coords4 pmq4)
  print (PMQ.rangeLookupDummy coords1 coords4 pmq4)

test2 :: IO ()
test2 = do
  let points_num = 5

  let pmqEmpty = PMQ.empty
  let pmq = generateNPoints points_num "t" pmqEmpty

  let l = PMQ.Coords 0 0
  let r = PMQ.Coords (points_num - 1) (points_num - 1)

  print pmq
  print (PMQ.rangeLookupDummiest l r pmq)
  print (PMQ.rangeLookupDummy l r pmq)