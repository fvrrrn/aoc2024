-- |
-- Module:         Day2
-- Description:    <https://adventofcode.com/2024/day/1 Day 2: Red-Nosed Reports>
module Day2 (part1, part2) where

import Common (readEntire, readMany)
import Control.Monad (ap, foldM_, guard)
import Data.Functor (($>))
import Data.List (inits, tails)
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

isSafe1, isSafe2 :: [Int] -> Bool
isSafe1 = isJust . foldM_ go EQ . (zipWith (-) `ap` drop 1)
  where
    go k x = guard (x /= 0 && abs x <= 3 && k /= compare 0 x) $> compare x 0
isSafe2 report = any isSafe1 [a ++ b | (a, _ : b) <- zip (inits report) (tails report)]

part1 :: Text -> Either String Int
part1 input = length . filter isSafe1 <$> parse input

-- 1. zip (inits report) (tails report)
-- inits report gives all initial segments of the list (including the empty list at the beginning).
--
-- For example, for report = [1, 2, 3], inits report will be:
-- haskell
-- Copy
-- Edit
-- [[], [1], [1, 2], [1, 2, 3]]
-- tails report gives all the tails of the list (including the entire list and the empty list at the end).
--
-- For report = [1, 2, 3], tails report will be:
-- haskell
-- Copy
-- Edit
-- [[1, 2, 3], [2, 3], [3], []]
-- zip combines these two lists element by element. So, for the list [1, 2, 3]:
--
-- haskell
-- Copy
-- Edit
-- zip (inits report) (tails report)
-- = [([], [1, 2, 3]), ([1], [2, 3]), ([1, 2], [3]), ([1, 2, 3], [])]
-- 2. The List Comprehension: [a ++ b | (a, _ : b) <- zip (inits report) (tails report)]
-- This is a list comprehension that iterates over the zipped list.
--
-- The expression (a, _ : b) ensures that in each pair (a, b) (from the zip), we discard the first element of the tail (_ : b) and combine the head (a) with the remainder (b).
--
-- What does this do?
--
-- It takes the prefix a from the inits and appends the tail b from the tails, effectively removing the first element of the list.
-- This gives us every possible sub-sequence that results from removing a single level from the original report.
-- Example for report = [1, 2, 3]:
--
-- zip (inits [1, 2, 3]) (tails [1, 2, 3])
-- = [([], [1, 2, 3]), ([1], [2, 3]), ([1, 2], [3]), ([1, 2, 3], [])]
--
-- -- List comprehension: [a ++ b | (a, _ : b) <- zip (inits [1, 2, 3]) (tails [1, 2, 3])]
-- = [[], [2, 3], [1, 3], [1, 2]]
-- These are the subsequences where we have removed exactly one element from the original list [1, 2, 3].
--
-- 3. any isSafe1 [...]
-- isSafe1 checks whether a sequence is "safe" by verifying the conditions (monotonically increasing or decreasing and the differences between adjacent levels are between 1 and 3).
--
-- The any function checks if any of the resulting sub-sequences (after removing one level) satisfy the isSafe1 condition.
--
-- How does this work?
-- For each sub-sequence (e.g., [2, 3], [1, 3]), isSafe1 checks if it's a "safe" sequence.
-- If any of these sub-sequences is safe, isSafe2 returns True, indicating that the report is safe.
part2 :: Text -> Either String Int
part2 input = length . filter isSafe2 <$> parse input
