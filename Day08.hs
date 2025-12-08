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

-- the solution for part 1 would be much faster using a union-find data structure, but since there
-- doesn't seem to be an implementation of one in the GHC libraries i'm just using a set of sets

solvePart1 :: Int -> [Vec3 Int] -> Int
solvePart1 i =
  foldl' (*) 1
    . take 3
    . sortBy (\lhs rhs -> compare (Down lhs) (Down rhs))
    . fmap Set.size
    . go []
    . Seq.take i
    . Seq.sortOn (uncurry d2)
    . pairs
  where
    go :: [Set (Vec3 Int)] -> Seq (Vec3 Int, Vec3 Int) -> [Set (Vec3 Int)]
    go circuits Empty = circuits
    go circuits ((pl, pr) :<| ps) =
      let (cs, cs') = partition (\set -> Set.member pl set || Set.member pr set) circuits
          c = Set.unions (Set.fromList [pl, pr] : cs)
       in go (c : cs') ps

solvePart2 :: [Vec3 Int] -> Int
solvePart2 ps = go [] (length ps) . Seq.sortOn (uncurry d2) . pairs $ ps
  where
    go :: [Set (Vec3 Int)] -> Int -> Seq (Vec3 Int, Vec3 Int) -> Int
    go _ pointCount Empty = error "unexpected end of list"
    go circuits pointCount ((pl@(Vec3 lx _ _), pr@(Vec3 rx _ _)) :<| ps) =
      let (cs, cs') = partition (\set -> Set.member pl set || Set.member pr set) circuits
          c = Set.unions (Set.fromList [pl, pr] : cs)
       in ( if null cs' && Set.size c == pointCount
              then lx * rx
              else go (c : cs') pointCount ps
          )

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 1000 input
      part2 = solvePart2 input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
