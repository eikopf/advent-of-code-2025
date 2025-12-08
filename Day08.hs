{-# OPTIONS_GHC -Wno-typed-holes #-}

module Day08 where

import Data.Foldable (foldl')
import Data.List (partition, sortBy, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

data Vec3 a = Vec3 a a a
  deriving (Show, Ord, Eq)

-- | The squared Euclidean distance.
d2 :: (Num a) => Vec3 a -> Vec3 a -> a
d2 (Vec3 l1 l2 l3) (Vec3 r1 r2 r3) = (l1 - r1) ^ 2 + (l2 - r2) ^ 2 + (l3 - r3) ^ 2

parse :: String -> [Vec3 Int]
parse = map parseVec3 . lines
  where
    parseVec3 :: String -> Vec3 Int
    parseVec3 s0 =
      let (x, s') = span (/= ',') s0
          (y, s'') = span (/= ',') $ drop 1 s'
          z = drop 1 s''
       in Vec3 (read x) (read y) (read z)

pairs :: [a] -> Seq (a, a)
pairs =
  Seq.fromList
    . concatMap (\(x :| xs) -> map (x,) xs)
    . mapMaybe nonEmpty
    . tails

solvePart1 :: [Vec3 Int] -> Int
solvePart1 =
  foldl' (*) 1
    . take 3
    . sortBy (\lhs rhs -> compare (Down lhs) (Down rhs))
    . fmap Set.size
    . go []
    . Seq.take 1000
    . Seq.sortOn (uncurry d2)
    . pairs
  where
    go :: [Set (Vec3 Int)] -> Seq (Vec3 Int, Vec3 Int) -> [Set (Vec3 Int)]
    go circuits Empty = circuits
    go circuits ((pl, pr) :<| ps) =
      let (cs, cs') = partition (\set -> Set.member pl set || Set.member pr set) circuits
          c = Set.unions (Set.fromList [pl, pr] : cs)
       in go (c : cs') ps

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 input
      part2 = () -- solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
