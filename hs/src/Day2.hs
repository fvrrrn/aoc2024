-- |
-- Module:         Day2
-- Description:    <https://adventofcode.com/2024/day/1 Day 2: Red-Nosed Reports>
module Day2 (part1) where

import Common (readEntire, readMany)
import Control.Monad (ap, foldM_, guard)
import Data.Either (fromLeft)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T (lines)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = mapM (readEntire $ readMany T.decimal) . T.lines

-- Behavior of guard and $>
-- guard:
--
-- It checks the condition x /= 0 && abs x <= 3 && k /= compare 0 x.
-- If the condition fails, the computation short-circuits (stops early) because guard returns Nothing.
-- If the condition passes, the computation proceeds, and the monadic result is () (essentially "nothing meaningful" other than continuing).

-- $>:
--
-- This operator effectively says: "Replace the result of the left-hand side with the value on the right-hand side."
-- If the guard condition passes, $> applies, and the result of compare x 0 becomes the next accumulator value.
-- If the guard condition fails, $> is never reached because the fold stops.

-- First iteration:
--
-- k = EQ (neutral, starting point).
-- compare 0 1 = LT (since 0 < 1).
-- The guard condition checks EQ /= LT → Passes, so it continues.
-- The accumulator (k) is updated to compare 1 0 = GT (since 1 > 0).
-- Second iteration:
--
-- k = GT (from the first iteration).
-- compare 0 1 = LT (since 0 < 1).
-- The guard condition checks GT /= LT → Passes, so it continues.
-- The accumulator (k) stays GT.
-- Third iteration:
--
-- k = GT (still from previous iterations).
-- compare 0 1 = LT (same reasoning as before).
-- The guard condition checks GT /= LT → Passes, so it continues.
-- The accumulator remains GT.

isSafe1 :: [Int] -> Bool
isSafe1 = isJust . foldM_ go EQ . (zipWith (-) `ap` drop 1)
  where
    go k x = guard (x /= 0 && abs x <= 3 && k /= compare 0 x) $> compare x 0

part1 :: Text -> Either String Int
part1 input = length . filter isSafe1 <$> parse input
