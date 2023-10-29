module Main where

import qualified Data.TestLib
import HaskellSay (haskellSay)
import qualified Data.PackedMemoryArray as PMA

main :: IO ()
main = do 
    Data.TestLib.helloWorld
    haskellSay "Hello, art22m!"