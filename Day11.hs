module Day11 where

import Control.Monad.State.Strict
import Data.List (foldl')
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (State, parse)

-- | A graph node.
type Node = (Char, Char, Char)

-- | A directed graph based on an adjacency list.
newtype Graph v = Graph {edges :: Map v [v]}

-- | Parses a graph from a `String`.
parse :: String -> Either ParseError (Graph Node)
parse = runParser (Graph . Map.fromList <$> many (line <* newline)) () ""
  where
    line :: Parsec String () (Node, [Node])
    line = (,) <$> (node <* char ':') <*> many (char ' ' *> node)

    node :: Parsec String () Node
    node = (,,) <$> letter <*> letter <*> letter

-- | Returns the number of paths between the given points.
solve :: Graph Node -> Node -> Node -> Int
solve graph start end = evalState (go graph start) (Map.fromList [(end, 1)])
  where
    -- Memoizing helper function that converts a DFS from an arbitrary start node
    -- into an action that threads a (Map Node Int) through the computation.
    go :: Graph Node -> Node -> State (Map Node Int) Int
    go graph start = do
      tbl <- get
      case tbl !? start of
        Just n -> return n
        Nothing -> do
          let successors = fromMaybe [] (edges graph !? start)
          let actions = map (go graph) successors
          sum <- foldl' (liftA2 (+)) (pure 0) actions
          () <- modify (Map.insert start sum)
          return sum

-- | Returns the number of paths between "you" and "out".
solvePart1 :: Graph Node -> Int
solvePart1 graph = solve graph ('y', 'o', 'u') ('o', 'u', 't')

-- | Returns the number of paths between "svr" and "out" that include "dac"
-- and "fft" in any order.
solvePart2 :: Graph Node -> Int
solvePart2 graph =
  let svrToDac = solve graph ('s', 'v', 'r') ('d', 'a', 'c')
      svrToFft = solve graph ('s', 'v', 'r') ('f', 'f', 't')
      dacToFft = solve graph ('d', 'a', 'c') ('f', 'f', 't')
      fftToDac = solve graph ('f', 'f', 't') ('d', 'a', 'c')
      dacToOut = solve graph ('d', 'a', 'c') ('o', 'u', 't')
      fftToOut = solve graph ('f', 'f', 't') ('o', 'u', 't')
   in (svrToDac * dacToFft * fftToOut) + (svrToFft * fftToDac * dacToOut)

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
