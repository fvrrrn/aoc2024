{-# LANGUAGE NondecreasingIndentation #-}

module Main (main) where

import Control.Monad (ap, when)
-- import Data.Bifunctor (bimap)
-- import Data.Foldable (find)
-- import Data.List (intercalate)
-- import Data.Maybe (fromMaybe)
import Data.Text (Text)
-- import Data.Text qualified as T (unpack)
import Data.Text.IO qualified as TIO (readFile)
import Day1 qualified (part1)
import System.Environment (getArgs)

getDayInput :: Int -> IO Text
getDayInput i = do
  -- dataDir <- fromMaybe "." . find (not . null) <$> lookupEnv "AOC2024_DATADIR"
  -- TIO.readFile . combine dataDir $ "day" ++ show i ++ ".txt"
  TIO.readFile $ show i <> ".txt"

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run = run' `ap` show

run' :: Int -> String -> (a -> IO ()) -> [Text -> a] -> IO ()
run' day name showIO funcs = do
  args <- getArgs
  when (null args || name `elem` args) $ do
    putStrLn $ "Day " ++ name
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
  run 1 (either fail print) [Day1.part1]
