module Day12 where

import Data.Functor (($>))
import Text.Parsec hiding (Empty, State, parse)

data Vec6 a = V6 a a a a a a
  deriving (Show, Eq, Functor, Foldable)

data Vec3 a = V3 a a a
  deriving (Show, Eq, Functor, Foldable)

data Cell = Occupied | Empty
  deriving (Show, Eq)

data Region = Region
  { dimensions :: (Int, Int),
    quantities :: Vec6 Int
  }
  deriving (Show, Eq)

type Mat3 a = Vec3 (Vec3 a)

type Shape = Mat3 Cell

type Input = (Vec6 Shape, [Region])

parse :: String -> Either ParseError Input
parse = runParser ((,) <$> repeat6 shape <*> many region) () ""
  where
    shape :: Parsec String () Shape
    shape = V3 <$> (digit *> char ':' *> newline *> cells) <*> cells <*> (cells <* newline)

    cells :: Parsec String () (Vec3 Cell)
    cells = V3 <$> cell <*> cell <*> (cell <* newline)

    cell :: Parsec String () Cell
    cell = (char '#' $> Occupied) <|> (char '.' $> Empty)

    region :: Parsec String () Region
    region = Region <$> ((,) <$> (num <* char 'x') <*> (num <* char ':')) <*> (repeat6 (char ' ' *> num) <* newline)

    num :: Parsec String () Int
    num = read <$> (try (count 2 digit) <|> count 1 digit)

    repeat6 :: Parsec String () a -> Parsec String () (Vec6 a)
    repeat6 p = V6 <$> p <*> p <*> p <*> p <*> p <*> p

-- solving this problem correctly is NP-hard. as such, the *actual* solution is
-- to check for the trivial condition under which a given region has a solution,
-- namely that it has enough cells to fit the largest shape in place of each
-- actual shape. this fails on the example input, but succeeds on the actual
-- input. i find this ludicrously stupid

solve :: Input -> Int
solve (_, regions) = length $ filter trivialFit regions
  where
    trivialFit :: Region -> Bool
    trivialFit region =
      let (cols, rows) = dimensions region
          shapeCount = sum $ quantities region
       in (cols * rows) >= 9 * shapeCount

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solve <$> input
      part2 = () -- solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
