module Day06 where

import Data.List (foldl', transpose, unsnoc)

data Op = Add | Mul

data Problem = Problem Op [Int]

parseWith :: ([String] -> [String]) -> String -> Maybe [Problem]
parseWith f s = do
  (rows, lastRow) <- unsnoc $ lines s
  let ops = processOpLine lastRow
  let elems = map f $ transpose $ map (takeLengths (map snd ops)) rows
  let problems = zipWith Problem (map fst ops) (map (map read) elems)
  return problems
  where
    processOpLine :: String -> [(Op, Int)]
    processOpLine [] = []
    processOpLine ('+' : cs) = let (spaces, tail) = span (== ' ') cs in (Add, length spaces) : processOpLine tail
    processOpLine ('*' : cs) = let (spaces, tail) = span (== ' ') cs in (Mul, length spaces) : processOpLine tail
    processOpLine _ = error "invalid operator"

    takeLengths :: [Int] -> [a] -> [[a]]
    takeLengths [] xs = []
    takeLengths [l] xs = [take (l + 1) xs]
    takeLengths (l : ls) xs = take l xs : takeLengths ls (drop (l + 1) xs)

solve :: [Problem] -> Int
solve = sum . map (\(Problem op is) -> foldl' (runOp op) (identity op) is)
  where
    runOp Add = (+)
    runOp Mul = (*)

    identity Add = 0
    identity Mul = 1

main :: IO ()
main = interact $ \s ->
  let part1 = solve <$> parseWith id s
      part2 = solve <$> parseWith transpose s
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
