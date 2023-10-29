module Main where

import qualified Data.TestLib
import HaskellSay (haskellSay)

main :: IO ()
main = do 
    Data.TestLib.helloWorld
    haskellSay "Hello, Haskell! You're using a function from another package!"