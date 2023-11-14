module Main where

import HaskellSay (haskellSay)
import System.IO

import qualified Data.Vector as V

import qualified Data.PackedMemoryArrayMap as PMA

import qualified Data.PackedMemoryQuadtree as PMQ

main :: IO ()
main = do
    putStrLn "start"
    
    -- let pmc1 = PMC.empty
    -- let pmc2 = PMC.insertP 1 "data1" pmc1
    -- let pmc3 = PMC.insertE 2 "data2" pmc2
    -- let pmc4 = PMC.insertPMA 4 "data4" pmc3
    -- let pmc5 = PMC.insert 5 "data 5" pmc2

    let coords1 = PMQ.Coords 0 0
    let coords2 = PMQ.Coords 0 1
    let coords3 = PMQ.Coords 0 2
    let coords4 = PMQ.Coords 1 0

    let pmq1 = PMQ.empty
    let pmq2 = PMQ.insert coords1 "data c1" pmq1
    let pmq3 = PMQ.insert coords2 "data c2" pmq2

    print (PMQ.lookup coords1 pmq3)
    print (PMQ.lookup coords2 pmq3)
    print (PMQ.lookup coords3 pmq3)
    print (PMQ.lookup coords4 pmq3)

    putStrLn "end"