module Main where

import ApplicativeParserSpec (applicativeParserSpec)
import Test.Hspec

main :: IO ()
main = hspec applicativeParserSpec
