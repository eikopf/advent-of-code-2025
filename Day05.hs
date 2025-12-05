module Day05 where

import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.Ix (inRange, rangeSize)
import Data.List (foldl')

data Db = Db [(Int, Int)] [Int]
  deriving (Show)

parse :: String -> Db
parse = uncurry Db . bimap (map readRange) (map read . drop 1) . break null . lines
  where
    readRange :: String -> (Int, Int)
    readRange = bimap read (read . drop 1) . break (== '-')

solvePart1 :: Db -> Int
solvePart1 (Db rs is) = length $ filter (flip any rs . flip inRange) is

solvePart2 :: Db -> Int
solvePart2 (Db rs _) =
  sum . map rangeSize $
    fix (\rec rs -> let rs' = foldl' upsertRange [] rs in if rs == rs' then rs else rec rs') rs
  where
    upsertRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    upsertRange [] r' = [r']
    upsertRange (r : rs) r' = case unionMaybe r r' of
      Nothing -> r : upsertRange rs r'
      Just r'' -> r'' : rs

    unionMaybe :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
    unionMaybe lhs@(l1, l2) rhs@(r1, r2) =
      if inRange lhs r1 || inRange rhs l1
        then Just (min l1 r1, max l2 r2)
        else Nothing

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
