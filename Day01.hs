{-# OPTIONS_GHC -Wno-typed-holes #-}

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

solvePart1 :: [Rotation] -> Either Error Int
solvePart1 = Right . solveWith 50 0

solveWith :: Int -> Int -> [Rotation] -> Int
solveWith _ zeroCount [] = zeroCount
solveWith state zeroCount (r : rs) =
  let newState = applyRotation r state
   in let newZeroCount = updateZeroCount newState zeroCount
       in solveWith newState newZeroCount rs

applyRotation :: Rotation -> Int -> Int
applyRotation (Rotation L (Angle a)) b = b - a
applyRotation (Rotation R (Angle a)) b = b + a

updateZeroCount :: Int -> Int -> Int
updateZeroCount state =
  if state `mod` 100 == 0
    then (+) 1
    else (+) 0

solvePart2 :: [Rotation] -> Either Error Int
solvePart2 = Right . solvePart2With 50 0

solvePart2With :: Int -> Int -> [Rotation] -> Int
solvePart2With _ zeroCount [] = zeroCount
solvePart2With state zeroCount (r : rs) =
  let newState = applyRotation r state
   in let newZeroCount = updateZeroCountPart2 r state newState zeroCount
       in solvePart2With newState newZeroCount rs

updateZeroCountPart2 :: Rotation -> Int -> Int -> Int -> Int
updateZeroCountPart2 (Rotation L (Angle a)) s1 0 = if a > s1 then (+) 2 else (+) 1
updateZeroCountPart2 (Rotation R (Angle a)) s1 0 = if a > (100 - s1) then (+) 2 else (+) 1
updateZeroCountPart2 (Rotation L (Angle a)) s1 s2 = if a > (s2 - s1) then (+) 1 else (+) 0
updateZeroCountPart2 (Rotation R (Angle a)) s1 s2 = if a > (s1 - s2) then (+) 1 else (+) 0

main :: IO ()
main = interact (show . (solvePart2 <=< parse))
