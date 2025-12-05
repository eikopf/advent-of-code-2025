{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day05 where

import Data.Bifunctor (bimap)
import Data.Ix (inRange)
import Data.List (findIndex)

data Db = Db [(Int, Int)] [Int]
  deriving (Show)

parse :: String -> Db
parse = uncurry Db . bimap (map readRange) (map read . drop 1) . break null . lines
  where
    readRange :: String -> (Int, Int)
    readRange = bimap read (read . drop 1) . break (== '-')

solvePart1 :: Db -> Int
solvePart1 (Db rs is) = length $ filter (flip any rs . flip inRange) is

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
