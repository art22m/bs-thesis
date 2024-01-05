module Main where

import HaskellSay (haskellSay)
import System.IO

import qualified Data.Vector as V

import qualified Data.PackedMemoryArrayMap as Map
import qualified Data.PackedMemoryArray as PMA

import qualified Data.PackedMemoryQuadtree as PMQ


main :: IO ()
main = do
    putStrLn "start"
    
    let pma1 = PMA.empty
    let pma2 = PMA.insert 1 2 pma1

    print (pma2)

    print "----"

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

    print(PMQ.getPMAMap pmq4)

    print (PMQ.rangeLookupDummiest coords1 coords4 pmq4)
    print (PMQ.rangeLookupDummy coords1 coords4 pmq4)

    putStrLn "end"