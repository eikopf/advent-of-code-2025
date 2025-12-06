{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day06 where

import Data.List (foldl', transpose, unsnoc)

data Op = Add | Mul
  deriving (Show, Eq)

data Problem = Problem Op [Int]
  deriving (Show)

charToOp :: Char -> Op
charToOp '+' = Add
charToOp '*' = Mul
charToOp _ = error "invalid operator"

runOp :: (Num a) => Op -> a -> a -> a
runOp Add = (+)
runOp Mul = (*)

identity :: (Num a) => Op -> a
identity Add = 0
identity Mul = 1

processOpLine :: String -> [(Op, Int)]
processOpLine [] = []
processOpLine ('+' : cs) = let (spaces, tail) = span (== ' ') cs in (Add, length spaces) : processOpLine tail
processOpLine ('*' : cs) = let (spaces, tail) = span (== ' ') cs in (Mul, length spaces) : processOpLine tail
processOpLine _ = error "invalid operator"

takeLengths :: [Int] -> [a] -> [[a]]
takeLengths [] xs = []
takeLengths [l] xs = [take (l + 1) xs]
takeLengths (l : ls) xs = take l xs : takeLengths ls (drop (l + 1) xs)

parsePart1 :: String -> Maybe [Problem]
parsePart1 s = do
  (rows, lastRow) <- unsnoc $ lines s
  let ops = processOpLine lastRow
  let elems = transpose $ map (takeLengths (map snd ops)) rows
  let problems = zipWith Problem (map fst ops) (map (map read) elems)
  return problems

parsePart2 :: String -> Maybe [Problem]
parsePart2 s = do
  (rows, lastRow) <- unsnoc $ lines s
  let ops = processOpLine lastRow
  let elems = map transpose $ transpose $ map (takeLengths (map snd ops)) rows
  let problems = zipWith Problem (map fst ops) (map (map read) elems)
  return problems

solve :: [Problem] -> Int
solve = sum . map (\(Problem op is) -> foldl' (runOp op) (identity op) is)

main :: IO ()
main = interact $ \s ->
  let part1 = solve <$> parsePart1 s
      part2 = solve <$> parsePart2 s
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
