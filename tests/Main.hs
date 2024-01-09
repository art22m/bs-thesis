module Main where

import qualified System.Exit as Exit
import Test.HUnit
import Test.QuickCheck 
import Data.PackedMemoryQuadtree

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 3)

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess