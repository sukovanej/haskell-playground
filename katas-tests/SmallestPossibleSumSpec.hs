module SmallestPossibleSumSpec where

import SmallestPossibleSum (smallestPossibleSum)
import Test.Hspec

smallestPossibleSumSpec :: SpecWith ()
smallestPossibleSumSpec = do
  describe "smallest possible sum" $ do
    it "simple cases 1" $ do
      smallestPossibleSum [1 :: Integer] `shouldBe` 1
      smallestPossibleSum [9 :: Integer] `shouldBe` 9

    it "simple cases 2" $ do
      smallestPossibleSum [11 :: Integer, 11] `shouldBe` 22
      smallestPossibleSum [11 :: Integer, 22] `shouldBe` 22
      smallestPossibleSum [22 :: Integer, 11] `shouldBe` 22
      smallestPossibleSum [22 :: Integer, 11, 11] `shouldBe` 33
      smallestPossibleSum [33 :: Integer, 22, 11] `shouldBe` 33
      smallestPossibleSum [33 :: Integer, 11] `shouldBe` 22
      smallestPossibleSum [11 :: Integer, 33] `shouldBe` 22

    it "simple cases 3" $ do
      smallestPossibleSum [3 :: Integer, 5] `shouldBe` 2

    it "simple cases 4" $ do
      smallestPossibleSum [6 :: Integer, 9, 21] `shouldBe` 9

    it "example tests" $ do
      smallestPossibleSum [1 :: Integer, 21, 55] `shouldBe` 3
      smallestPossibleSum [3 :: Integer, 13, 23, 7, 83] `shouldBe` 5
      smallestPossibleSum [4 :: Integer, 16, 24] `shouldBe` 12
      smallestPossibleSum [30 :: Integer, 12] `shouldBe` 12
      smallestPossibleSum [60 :: Integer, 12, 96, 48, 60, 24, 72, 36, 72, 72, 48] `shouldBe` 132
      smallestPossibleSum [71 :: Integer, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71] `shouldBe` 923
