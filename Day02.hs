module Day02 where

import Control.Monad (join)
import Data.Ix.Enum (range)
import Data.List.Split (splitWhen)

-- | Parses the puzzle input into a sequence of (start, end) pairs.
parse :: String -> [(Int, Int)]
parse = pairs . map read . splitWhen (`elem` "-,") . filter (/= '\n')

-- | Converts the longest even prefix of the input into a list of adjacent pairs. Note that this
-- will drop the last element of the list if the list is finite and of odd length.
pairs :: [a] -> [(a, a)]
pairs (x : y : zs) = (x, y) : pairs zs
pairs _ = []

-- | Returns `True` iff the given integer is an invalid ID in the first part of the problem.
--
-- An integer is an invalid ID if its decimal digits consist of a single sequence
-- repeated twice (e.g. 11, 1010, or 123123).
isInvalidPart1 :: Int -> Bool
isInvalidPart1 i =
  let len = decimalDigits i
      (prefix, suffix) = splitAt (len `div` 2) (show i)
   in prefix == suffix

-- | Returns `True` iff the given integer is an invalid ID in the second part of the problem.
--
-- Here an integer is invalid if there is some prefix of its decimal digits such that repeating the
-- prefix at least twice results in the number itself. A small optimisation here is to fail when the
-- prefix exceeds half the length of the entire digit string, since it must occur at least twice.
isInvalidPart2 :: Int -> Bool
isInvalidPart2 i =
  let digits = show i
      len = length digits
      prefix = take (len `div` 2) digits
   in go prefix digits len
  where
    go :: String -> String -> Int -> Bool
    go [] _ _ = False
    go prefix digits len =
      (digits == take len (cycle prefix) && (len `rem` length prefix) == 0)
        || go (take (length prefix - 1) prefix) digits len

-- | Returns the number of decimal digits in the input.
decimalDigits :: Int -> Int
decimalDigits = length . show -- this is probably stupidly inefficient

solvePart1 :: [(Int, Int)] -> Int
solvePart1 = sum . filter isInvalidPart1 . join . map range

solvePart2 :: [(Int, Int)] -> Int
solvePart2 = sum . filter isInvalidPart2 . join . map range

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
