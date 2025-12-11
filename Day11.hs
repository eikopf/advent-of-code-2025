{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day11 where

import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Parsec hiding (parse)

type Node = (Char, Char, Char)

newtype Graph v = Graph {edges :: Map v [v]}
  deriving (Show)

graphFromEdges :: (Ord v) => [(v, [v])] -> Graph v
graphFromEdges = Graph . Map.fromList

parse :: String -> Either ParseError (Graph Node)
parse = runParser (graphFromEdges <$> many (line <* newline)) () ""
  where
    line :: Parsec String () (Node, [Node])
    line = (,) <$> (node <* char ':') <*> many (char ' ' *> node)

    node :: Parsec String () Node
    node = (,,) <$> letter <*> letter <*> letter

solvePart1 :: Graph Node -> Maybe Int
solvePart1 graph = go graph Set.empty ('y', 'o', 'u')
  where
    go :: Graph Node -> Set Node -> Node -> Maybe Int
    go graph _ ('o', 'u', 't') = return 1
    go graph visitedNodes start = do
      let visitedNodes' = Set.insert start visitedNodes
      successors <- edges graph !? start
      return . sum . mapMaybe (go graph visitedNodes') . filter (not . flip Set.member visitedNodes') $ successors

solvePart2 :: Graph Node -> Maybe Int
solvePart2 graph = go graph Set.empty (False, False) ('s', 'v', 'r')
  where
    go :: Graph Node -> Set Node -> (Bool, Bool) -> Node -> Maybe Int
    go graph visitedNodes state ('o', 'u', 't')
      | state == (True, True) = return 1
      | otherwise = return 0
    go graph visitedNodes (dacSeen, fftSeen) start = do
      let visitedNodes' = Set.insert start visitedNodes
      let dacSeen' = dacSeen || start == ('d', 'a', 'c')
      let fftSeen' = fftSeen || start == ('f', 'f', 't')
      successors <- edges graph !? start
      return . sum . mapMaybe (go graph visitedNodes' (dacSeen', fftSeen')) . filter (not . flip Set.member visitedNodes') $ successors

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
