module Main where

import RegexParserSpec (regexParserSpec)
import Test.Hspec

main :: IO ()
main = hspec regexParserSpec
