-- |
-- Module:         Day1
-- Description:    <https://adventofcode.com/2024/day/1 Day 1: Boris Chernyshov>
module Day1 (part1) where

import Common (readEntire)
import Data.Either (fromLeft)
import Data.Function (on)
import Data.List (sort, transpose)
import Data.Text (Text)
import Data.Text qualified as T (lines, words)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = fmap transpose . mapM (mapM (readEntire T.decimal) . T.words) . T.lines

part1 :: Text -> Either String Int
part1 input = case parse input of
  Right [as, bs] -> pure $ sum $ abs <$> (zipWith (-) `on` sort) as bs
  other -> Left $ fromLeft "no parse" other
