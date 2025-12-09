{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day09 where

import Data.Bifoldable (biall)
import Data.Bifunctor (bimap)
import Data.Ix (Ix, range)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

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

solvePart2 :: [(Int, Int)] -> Int
solvePart2 ps =
  let edgeSet = buildPolygon ps
      vertexSet = Set.fromList ps
      width = maximum (map fst ps)
      p = inside vertexSet edgeSet width
   in maximum . map (uncurry area) . filter (biall p p . otherCorners) . pairs $ ps
  where
    otherCorners :: ((a, a), (a, a)) -> ((a, a), (a, a))
    otherCorners ((a, b), (c, d)) = ((a, d), (c, b))

    -- this works, but takes forever to solve part 2
    -- maybe there's a faster way to check this value? could i build a quadtree?

    inside :: Set (Int, Int) -> Set (Int, Int) -> Int -> (Int, Int) -> Bool
    inside vertexSet edgeSet width p@(px, py) =
      let vertexHits = length $ filter (`Set.member` vertexSet) $ map (,py) [px .. width]
          edgeHits = length $ filter (`Set.member` edgeSet) $ map (,py) [px .. width]
       in Set.member p edgeSet
            || odd vertexHits
            || (vertexHits == 0 && odd edgeHits)

    buildPolygon :: [(Int, Int)] -> Set (Int, Int)
    buildPolygon = Set.fromList . concatMap range' . adjacentPairs

    range' :: (Ix a) => (a, a) -> [a]
    range' (a, b) = if a < b then range (a, b) else range (b, a)

    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs xs = zip xs (drop 1 (cycle xs))

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
