module Day01 where

import Control.Monad (join, (<=<))
import Data.Bifunctor (bimap)
import Data.Word (Word8)
import Text.Read (readEither)

data Side = L | R
  deriving (Eq, Show)

newtype Angle = Angle Int
  deriving (Eq, Show)

data Rotation = Rotation Side Angle
  deriving (Eq, Show)

data Error
  = BadSidePrefix Char
  | AngleExceeds100 Int
  | MalformedAngle String
  | EmptyLine
  deriving (Show)

parse :: String -> Either Error [Rotation]
parse = mapM parseRotation . lines

parseRotation :: String -> Either Error Rotation
parseRotation [] = Left EmptyLine
parseRotation (c : cs) = do
  side <- parseSide c
  angle <- parseAngle cs
  return (Rotation side angle)

parseSide :: Char -> Either Error Side
parseSide 'L' = Right L
parseSide 'R' = Right R
parseSide c = Left (BadSidePrefix c)

parseAngle :: String -> Either Error Angle
parseAngle s = bimap (const (MalformedAngle s)) Angle $ readEither s

absMod :: Int -> Int -> Int
absMod i m = snd $ i `divMod` m

applyRotation :: Rotation -> Int -> Int
applyRotation (Rotation L (Angle a)) b = (b - a) `absMod` 100
applyRotation (Rotation R (Angle a)) b = (b + a) `absMod` 100

solvePart1 :: [Rotation] -> Int
solvePart1 = solveWith 50 0
  where
    solveWith :: Int -> Int -> [Rotation] -> Int
    solveWith _ zeroCount [] = zeroCount
    solveWith state zeroCount (r : rs) =
      let newState = applyRotation r state
       in let newZeroCount = updateZeroCount newState zeroCount
           in solveWith newState newZeroCount rs

    updateZeroCount :: Int -> Int -> Int
    updateZeroCount state =
      if state `mod` 100 == 0
        then (+) 1
        else (+) 0

solvePart2 :: [Rotation] -> Int
solvePart2 rs = snd $ foldl go (50, 0) rs
  where
    go :: (Int, Int) -> Rotation -> (Int, Int)
    go (state, zeroCount) r@(Rotation side _) =
      let r' = rotationToInt r
          state' = (state + r') `absMod` 100
          zeroCount' =
            zeroCount + (if state' == 0 then 1 else 0) + case side of
              L -> abs $ (r' + state) `div` 100
              R -> abs $ (r' - state) `div` 100
       in (state', zeroCount')

    rotationToInt :: Rotation -> Int
    rotationToInt (Rotation L (Angle a)) = -a
    rotationToInt (Rotation R (Angle a)) = a

main :: IO ()
main = interact $ \s ->
  let input = parse s
      part1 = solvePart1 <$> input
      part2 = solvePart2 <$> input
   in "part 1: " ++ show part1 ++ "\npart 2: " ++ show part2 ++ "\n"
