{-# LANGUAGE ScopedTypeVariables #-}

module RandomEasyOnes (humanReadable) where

import Data.Char
import Data.List

-- find cubes
findNb' :: Integer -> Integer -> Integer -> Integer
findNb' x n r 
  | r == x = n
  | r > x = -1
  | otherwise = findNb' x (n + 1) (r + (n + 1) ^ 3)

findNb :: Integer -> Integer
findNb x = findNb' x 1 1

-- sort odds

sortArray :: [Int] -> [Int]
sortArray xs = arrange xs (sort . filter odd $ xs)
  where 
    arrange s [] = s
    arrange [] s = s
    arrange (a:as) sorted@(b:bs) = 
      if even a 
        then a : arrange as sorted 
        else b : arrange as bs

-- 9 7 4 3
-- 3 4 7 9

-- reverseWords

reverseWords :: String -> String
reverseWords = mapWords reverse

mapWords :: (String -> String) -> String -> String
mapWords _ [] = []
mapWords f (' ':xs) = ' ' : mapWords f xs
mapWords f xs = f word <> mapWords f  rest
  where word = takeWhile (/= ' ') xs
        rest = drop (length word) xs

-- bus

number :: [(Int, Int)] -> Int
number = sum . map (uncurry (-))

-- humanReadable

humanReadable :: Int -> String
humanReadable s = showTime hours <> ":" <> showTime minutes <> ":" <> showTime seconds
  where hours = s `div` (60 * 60)
        minutes = (s `mod` (60 * 60)) `div` 60
        seconds = s `mod` 60
        showTime n = let str = show n in 
          if length str == 1 then '0' : str else str

-- 3 and 5

sumOf3And5 :: Integer -> Integer
sumOf3And5 xs = sum $ filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0) [1..xs-1]

-- uniqueInOrder

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder (x:xs)= foldl (\c n -> if last c == n then c else c ++ [n]) [x] xs

-- alphabetPosition

charAlphabetPosition :: Char -> Int
charAlphabetPosition ch 
  | ascii >= ord 'a' && ascii <= ord 'z' = ascii - ord 'a' + 1 
  | otherwise = 0
    where ascii = ord $ toLower ch

alphabetPosition :: String -> String
alphabetPosition = unwords . map show . filter (/= 0) . map charAlphabetPosition

-- narcissistic

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

narcissistic :: (Show n, Integral n) => n -> Bool
narcissistic n = fromIntegral n == sumOfDigits
  where digits = toDigits n
        sumOfDigits = sum $ map (^ length digits) digits

-- printerError

isValidColor :: Char -> Bool
isValidColor c = let o = ord c in o >= ord 'a' && o <= ord 'm'

printerError :: [Char] -> [Char]
printerError s = show invalids <> "/" <> (show . length) s
  where invalids = length $ filter (not . isValidColor) s

-- toNumber

toNumber :: [Int] -> Int
toNumber xs = sum $ zipWith (\c p -> c * 2 ^ p) xs (reverse [0..length xs - 1])