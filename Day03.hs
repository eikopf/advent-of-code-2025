{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day03 where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (singleton, subsequences)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solvePart1 :: [[Int]] -> Int
solvePart1 = sum . map maxPair
  where
    dropLast :: [a] -> [a]
    dropLast [x] = []
    dropLast (x : xs) = x : dropLast xs

    maxPair :: [Int] -> Int
    maxPair digits =
      let prefixMax = maximum (dropLast digits)
          suffix = drop 1 (dropWhile (/= prefixMax) digits)
          suffixMax = maximum suffix
       in prefixMax * 10 + suffixMax

solvePart2 :: [[Int]] -> Int
solvePart2 = sum . map (maxKSubsequence 12)
  where
    maxKSubsequence :: Int -> [Int] -> Int
    maxKSubsequence 0 _ = 0
    maxKSubsequence 1 digits = maximum digits
    maxKSubsequence k digits =
      let prefix = dropLastK (k - 1) digits
          maxPrefix = maximum prefix
          suffix = drop 1 (dropWhile (/= maxPrefix) digits)
       in maxPrefix * (10 ^ (k - 1)) + maxKSubsequence (k - 1) suffix

    dropLastK :: Int -> [a] -> [a]
    dropLastK k = reverse . drop k . reverse

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
