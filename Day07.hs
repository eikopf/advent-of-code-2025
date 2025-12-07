module Day07 where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Ix (inRange)
import Data.List qualified as List
import Data.Maybe (maybe)

data Manifold = Manifold
  { startCol :: Int,
    splitters :: [(Int, Int)]
  }

parse :: String -> Maybe Manifold
parse s = do
  let rows = lines s
  (firstRow, splitterRows) <- List.uncons rows
  startCol <- List.elemIndex 'S' firstRow
  let splitters = parseSplitters (zip splitterRows [1 ..])
  return Manifold {startCol, splitters}
  where
    parseSplitters :: [(String, Int)] -> [(Int, Int)]
    parseSplitters = List.concatMap (\(s, row) -> map (row,) (List.elemIndices '^' s))

solvePart1 :: Manifold -> Int
solvePart1 (Manifold {startCol, splitters}) = go (IntSet.singleton startCol) splitters
  where
    go :: IntSet -> [(Int, Int)] -> Int
    go cols [] = 0
    go cols ((_, col) : ss) =
      let (cols', split) = if IntSet.member col cols then (insertSplitCols cols col, True) else (cols, False)
       in (if split then 1 else 0) + go cols' ss

    insertSplitCols :: IntSet -> Int -> IntSet
    insertSplitCols cols col =
      let cols' = IntSet.insert (col - 1) cols
          cols'' = IntSet.insert (col + 1) cols'
       in IntSet.delete col cols''

solvePart2 :: Manifold -> Int
solvePart2 (Manifold {startCol, splitters}) = sum . IntMap.elems $ go (IntMap.singleton startCol 1) splitters
  where
    go :: IntMap Int -> [(Int, Int)] -> IntMap Int
    go timelines [] = timelines
    go timelines ((_, col) : ss) = go (updateTimelines timelines col) ss

    updateTimelines :: IntMap Int -> Int -> IntMap Int
    updateTimelines timelines col = case IntMap.lookup col timelines of
      Nothing -> timelines
      Just n ->
        let timelines' = IntMap.alter (maybe (Just n) (pure . (+ n))) (col - 1) timelines
            timelines'' = IntMap.alter (maybe (Just n) (pure . (+ n))) (col + 1) timelines'
         in IntMap.delete col timelines''

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
