-- |
-- Module:         Day2
-- Description:    <https://adventofcode.com/2024/day/1 Day 2: Red-Nosed Reports>
module Day2 (part1) where

import Common (readEntire, readMany)
import Data.Either (fromLeft)
import Data.Text (Text)
import Data.Text qualified as T (lines)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = mapM (readEntire $ readMany T.decimal) . T.lines

isMonotonic :: (Ord a) => [a] -> Bool
isMonotonic [] = False
isMonotonic [_] = False
isMonotonic (x1 : x2 : xs)
  | and $ zipWith (<) (x2 : xs) (x1 : x2 : xs) = True
  | and $ zipWith (>) (x2 : xs) (x1 : x2 : xs) = True
  | otherwise = False

isInRange :: (Ord a, Num a) => [a] -> Bool
isInRange [] = True
isInRange [_] = True
isInRange (x1 : x2 : xs) = all (\d -> d >= 1 && d <= 3) (zipWith (\a b -> abs (b - a)) (x1 : x2 : xs) (x2 : xs))

isSafe :: (Ord a, Num a) => [a] -> Int
isSafe xs = if isMonotonic xs && isInRange xs then 1 else 0

part1 :: Text -> Either String Int
part1 input = case parse input of
  Right t -> pure $ sum $ map isSafe t
  other -> Left $ fromLeft "no parse" other
