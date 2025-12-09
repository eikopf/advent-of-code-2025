module Day09 where

import Data.Bifunctor (bimap)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)

parse :: String -> [(Int, Int)]
parse = map (bimap read (read . drop 1) . break (== ',')) . lines

area :: (Num a, Eq a) => (a, a) -> (a, a) -> a
area (a, b) (c, d) = (1 + abs (a - c)) * (1 + abs (b - d))

pairs :: [a] -> [(a, a)]
pairs =
  concatMap (\(x :| xs) -> map (x,) xs)
    . mapMaybe nonEmpty
    . tails

solvePart1 :: [(Int, Int)] -> Int
solvePart1 = maximum . map (uncurry area) . pairs

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
