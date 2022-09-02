module Main where

import SmallestPossibleSumSpec (smallestPossibleSumSpec)
import Test.Hspec

main :: IO ()
main = hspec smallestPossibleSumSpec
