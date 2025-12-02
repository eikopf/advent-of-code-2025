{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day02 where

import Control.Monad (join)
import Data.Ix.Enum (range)
import Data.List.Split (splitWhen)

parse :: String -> [(Int, Int)]
parse = pairs . map read . splitWhen (`elem` "-,") . filter (/= '\n')

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = error "odd number of elements"
pairs (x : y : zs) = (x, y) : pairs zs

solvePart1 :: [(Int, Int)] -> Int
solvePart1 = sum . filter isInvalidPart1 . join . map range

-- | Returns `True` iff the given integer is an invalid ID in the first part of the problem.
--
-- An integer is an invalid ID if its decimal digits consist of a single sequence
-- repeated twice (e.g. 11, 1010, or 123123).
isInvalidPart1 :: Int -> Bool
isInvalidPart1 i =
  let len = decimalDigits i
      (prefix, suffix) = splitAt (len `div` 2) (show i)
   in prefix == suffix

-- | Returns the number of decimal digits in the input.
decimalDigits :: Int -> Int
decimalDigits = length . show -- this is probably stupidly inefficient

main :: IO ()
main = interact (show . solvePart1 . parse)
