{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day07 where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Ix (inRange)
import Data.List qualified as List

data Manifold = Manifold
  { rows :: Int,
    cols :: Int,
    startCol :: Int,
    splitters :: [(Int, Int)]
  }
  deriving (Show)

parse :: String -> Maybe Manifold
parse s = do
  let rawRows = lines s
  (firstRow, splitterRows) <- List.uncons rawRows
  startCol <- List.elemIndex 'S' firstRow
  let splitters = parseSplitters (zip splitterRows [1 ..])
  let rows = length rawRows
  let cols = length firstRow
  return Manifold {rows, cols, startCol, splitters}
  where
    parseSplitters :: [(String, Int)] -> [(Int, Int)]
    parseSplitters = List.concatMap (\(s, row) -> map (row,) (List.elemIndices '^' s))

solvePart1 :: Manifold -> Int
solvePart1 (Manifold {cols, startCol, splitters}) = go (IntSet.singleton startCol) (0, cols - 1) splitters
  where
    go :: IntSet -> (Int, Int) -> [(Int, Int)] -> Int
    go cols _ [] = 0
    go cols range ((_, col) : ss) =
      let (cols', split) = if IntSet.member col cols then (insertSplitCols cols range col, True) else (cols, False)
       in (if split then 1 else 0) + go cols' range ss

    insertSplitCols :: IntSet -> (Int, Int) -> Int -> IntSet
    insertSplitCols cols range col =
      let cols' = if inRange range (col - 1) then IntSet.insert (col - 1) cols else cols
          cols'' = if inRange range (col + 1) then IntSet.insert (col + 1) cols' else cols'
       in IntSet.delete col cols''

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = () -- solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
