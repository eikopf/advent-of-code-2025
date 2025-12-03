{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day03 where

import Data.Char (digitToInt)

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

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2
