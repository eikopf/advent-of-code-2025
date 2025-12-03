module Day03 where

import Data.Char (digitToInt)

-- | Parses the input into a list of digit lists.
parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

-- | Solves part 1 by finding the sum of the largest subsequences of length 2.
solvePart1 :: [[Int]] -> Int
solvePart1 = sum . map maxPair
  where
    -- Drops the last element of a finite list (identity on infinite lists).
    dropLast :: [a] -> [a]
    dropLast [x] = []
    dropLast (x : xs) = x : dropLast xs

    -- Returns the value of the largest subsequence of length 2.
    maxPair :: [Int] -> Int
    maxPair digits =
      let prefixMax = maximum (dropLast digits)
          suffix = drop 1 (dropWhile (/= prefixMax) digits)
          suffixMax = maximum suffix
       in prefixMax * 10 + suffixMax

-- | Solves part 2 by finding the sum of the largest subsequences of length 12.
solvePart2 :: [[Int]] -> Int
solvePart2 = sum . map (maxKSubsequence 12)
  where
    -- Finds the largest subsequence of length k.
    maxKSubsequence :: Int -> [Int] -> Int
    maxKSubsequence 0 _ = 0
    maxKSubsequence 1 digits = maximum digits
    maxKSubsequence k digits =
      let prefix = dropLastK (k - 1) digits
          maxPrefix = maximum prefix
          suffix = drop 1 (dropWhile (/= maxPrefix) digits)
       in maxPrefix * (10 ^ (k - 1)) + maxKSubsequence (k - 1) suffix

    -- Drops the last k elements of a finite list.
    dropLastK :: Int -> [a] -> [a]
    dropLastK k = reverse . drop k . reverse

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
