{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day10 where

import Control.Monad (filterM)
import Data.Functor (($>))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl')
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

solvePart1 :: [Desc] -> Int
solvePart1 =
  sum
    . map
      ( \desc ->
          let combinations = powerset . map IntSet.fromList . buttons $ desc
              values = map (foldl' compose IntSet.empty) combinations
              lightSet = setOfLights . lights $ desc
              validCombinations = filter ((==) lightSet . snd) (zip combinations values)
              sizes = map (length . fst) validCombinations
           in minimum sizes
      )
  where
    compose :: IntSet -> IntSet -> IntSet
    compose lhs rhs = IntSet.difference (IntSet.union lhs rhs) (IntSet.intersection lhs rhs)

    setOfLights :: [LightState] -> IntSet
    setOfLights = IntSet.fromList . map snd . filter ((==) On . fst) . flip zip [0 ..]

    powerset :: [a] -> [[a]]
    powerset = filterM (const [False, True])

-- we can view the button schematics as sparse binary vectors, i.e. for a system with four lights
-- the schematic (2, 3) is ⟨0, 0, 1, 1⟩. stacking all of these vertically gives us a binary matrix,
-- and we can then use this to solve each part of the problem.
--
-- in the first part, the lights describe another binary vector; e.g. [.##.] is ⟨0, 1, 1, 0⟩. we
-- need to find the smallest (by sum) positive integer vector x such that Bx = l for the button
-- matrix B and light vector l.
--
-- the second part replaces the binary vector l with a positive integer vector j defined by the
-- joltage level counters. again, we need to find the smallest x such that Bx = j.

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
