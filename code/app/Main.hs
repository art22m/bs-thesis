module Main where

import qualified Data.TestLib
import HaskellSay (haskellSay)
import qualified Data.PackedMemoryArray as PMA
import Data.IntMap (size)

main :: IO ()
main = do 
    Data.TestLib.helloWorld
    haskellSay "Hello, art22m!"

    let pma = PMA.init 8
    print (PMA.size pma)