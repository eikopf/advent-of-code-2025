module Day09 where

import Data.Bifunctor (bimap)
import Data.List (find, sortOn, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

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

solvePart2 :: [(Int, Int)] -> Maybe Int
solvePart2 ps =
  let edges = adjacentPairs ps
   in fmap (uncurry area)
        . find (flip all edges . flip insideEdge)
        . sortOn (Down . uncurry area)
        . pairs
        $ ps
  where
    -- \| Checks that a rectangle is strictly to one side of an edge.
    insideEdge :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
    insideEdge edge@(e1, e2) rect@(p1, p2) =
      max (fst p1) (fst p2) <= min (fst e1) (fst e2)
        || min (fst p1) (fst p2) >= max (fst e1) (fst e2)
        || max (snd p1) (snd p2) <= min (snd e1) (snd e2)
        || min (snd p1) (snd p2) >= max (snd e1) (snd e2)

    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs xs = zip xs (drop 1 (cycle xs))

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
