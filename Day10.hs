{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day10 where

import Data.Functor (($>))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', subsequences)
import Text.Parsec hiding (parse)

data LightState = Off | On
  deriving (Show, Eq)

data Desc = Desc
  { lights :: [LightState],
    buttons :: [[Int]],
    joltages :: [Int]
  }
  deriving (Show)

parse :: String -> Either ParseError [Desc]
parse = runParser (many $ Desc <$> lights <*> buttons <*> (space *> joltages <* newline)) () ""
  where
    lights :: Parsec String () [LightState]
    lights = char '[' *> many ((char '.' $> Off) <|> (char '#' $> On)) <* char ']'

    buttons :: Parsec String () [[Int]]
    buttons = many (try (space *> (char '(' *> sepBy (read <$> many1 digit) (char ',') <* char ')')))

    joltages :: Parsec String () [Int]
    joltages = char '{' *> sepBy (read <$> many1 digit) (char ',') <* char '}'

-- let n be the number of lights; then each button corresponds to an item in the
-- powerset of {0, 1, ..., n - 1}. considering each button as a vertex on the
-- n-dimensional hypercube, we want to find the shortest sequence of adjacent
-- vertices starting at ∅ and ending at the goal described by the light sequence

solvePart1 :: [Desc] -> Int
solvePart1 = _
  where
    compose :: IntSet -> IntSet -> IntSet
    compose lhs rhs = IntSet.difference (IntSet.union lhs rhs) (IntSet.intersection lhs rhs)

    setOfLights :: [LightState] -> IntSet
    setOfLights = IntSet.fromList . map snd . filter ((==) On . fst) . flip zip [0 ..]

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
