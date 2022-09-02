module SmallestPossibleSum where

-- Link: https://www.codewars.com/kata/52f677797c461daaf7000740/train/haskell
import Data.List (sort)

smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum [] = undefined
smallestPossibleSum xs =
  if allNumbersInListEqual xs
    then sum xs
    else smallestPossibleSumSorted . sort $ xs
  where
    smallestPossibleSumSorted :: (Integral a) => [a] -> a
    smallestPossibleSumSorted [] = undefined
    smallestPossibleSumSorted (y : ys) = smallestPossibleSum $ y : fmap (decreaseUsingSmallest y) ys

    decreaseUsingSmallest :: (Integral a) => a -> a -> a
    decreaseUsingSmallest smallest x = if r == 0 then smallest else r
      where
        r = x `mod` smallest

allNumbersInListEqual :: (Eq a) => [a] -> Bool
allNumbersInListEqual (x : y : xs) = x == y && allNumbersInListEqual (y : xs)
allNumbersInListEqual _ = True
