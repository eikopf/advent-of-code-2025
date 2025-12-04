{-# OPTIONS_GHC -Wno-x-partial #-}

module Day04 where

import Data.Array (Array, bounds, indices, listArray, (!), (//))
import Data.Ix (inRange)

data Cell
  = Occupied
  | Empty
  deriving (Show, Eq)

charToCell :: Char -> Cell
charToCell '.' = Empty
charToCell '@' = Occupied
charToCell _ = error "invalid cell"

parse :: String -> Array (Int, Int) Cell
parse s =
  let rows = lines s
      rowLen = length (head rows)
      colLen = length rows
      elems = map charToCell $ concat rows
   in listArray ((1, 1), (rowLen, colLen)) elems

countOccupiedNeighbors :: Array (Int, Int) Cell -> (Int, Int) -> Int
countOccupiedNeighbors grid i = length $ filter ((==) Occupied . (!) grid) $ neighbors grid i

neighbors :: Array (Int, Int) Cell -> (Int, Int) -> [(Int, Int)]
neighbors grid i = filter (inRange (bounds grid)) (offsets <*> [i])
  where
    offsets :: [(Int, Int) -> (Int, Int)]
    offsets =
      map
        (\(x, y) (a, b) -> (a + x, b + y))
        [ (-1, -1),
          (-1, 0),
          (-1, 1),
          (0, -1),
          (0, 1),
          (1, -1),
          (1, 0),
          (1, 1)
        ]

solvePart1 :: Array (Int, Int) Cell -> Int
solvePart1 grid = length $ filter ((>) 4 . countOccupiedNeighbors grid) $ filter ((==) Occupied . (!) grid) $ indices grid

solvePart2 :: Array (Int, Int) Cell -> Int
solvePart2 grid = case removableIndices grid of
  [] -> 0
  is -> length is + solvePart2 (grid // map (,Empty) is)
  where
    removableIndices :: Array (Int, Int) Cell -> [(Int, Int)]
    removableIndices grid = filter ((>) 4 . countOccupiedNeighbors grid) $ filter ((==) Occupied . (!) grid) $ indices grid

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
