{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task13 where

import Commons (readInput)
import Data.List
import Data.List.Split (splitWhen)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 13 ==="
  input <- readInput "./inputs/task-13.txt"
  print $ summariseAllPatterns input

-- reads horizontal lines and vertical lines from input
readLines :: [String] -> ([String], [String])
readLines input = (horizontalLines, verticalLines)
  where
    horizontalLines = input
    verticalLines = transpose input

findReflection :: [String] -> [Int]
findReflection lines = filter isReflection [1..(length lines-1)]
  where
    isReflection :: Int -> Bool
    isReflection idx = let
      (beforeFull, afterFull) = splitAt idx lines
      len = min (length beforeFull) (length afterFull)
      before = if length beforeFull > len
               then drop (length beforeFull - len) beforeFull
               else beforeFull
      after = take len afterFull
      pairs = zip (reverse before) after
      in all (uncurry (==)) pairs

summarisePattern :: [String] -> Int
summarisePattern lines = 100 * sum horizontal + sum vertical
  where
    horizontal = findReflection lines
    vertical = findReflection (transpose lines)

readPatterns :: [String] -> [[String]]
readPatterns = splitWhen (== "")

summariseAllPatterns :: [String] -> Int
summariseAllPatterns input = sum $ map summarisePattern patterns
  where
    patterns = readPatterns input
