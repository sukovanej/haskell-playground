module Main where

import Test.Hspec
import ListsAsFoldsSpec (listAsFoldsSpec)

main :: IO ()
main = hspec listAsFoldsSpec
