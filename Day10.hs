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

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = "<see ./other/Day10.jl>"
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ part2 ++ "\n"
