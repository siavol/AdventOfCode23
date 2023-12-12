{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task12 where

import Commons (splitBy, readInput)
import Data.List

runTask :: IO ()
runTask = do
  print "=== Advent of Code 12 ==="
  input <- readInput "./inputs/task-12.txt"
  print $ sumOfArrangementCounts input


lineSatisfiesArrangement :: String -> [Int] -> Bool
lineSatisfiesArrangement line arrangement = lineSchema == arrangement
  where
    lineSchema = map length $ filter (\s -> head s == '#') $ group line

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

allPossibleArrangements :: String -> [Int] -> [String]
allPossibleArrangements line arrangement = filter (`lineSatisfiesArrangement` arrangement) $ go possibleBrokenPositions []
  where
    knownBrokens = length $ filter (== '#') line
    unknowns = sum arrangement - knownBrokens
    questionPositions = map fst $ filter (\t -> snd t == '?') $ zip [1..] line
    possibleBrokenPositions = combinations unknowns questionPositions
    go :: [[Int]] -> [String] -> [String]
    go [] acc = acc
    go (x:xs) acc = go xs (acc ++ [replaceWithBrokenAt x line])

replaceWithBrokenAt :: [Int] -> String -> String
replaceWithBrokenAt [] line = line
replaceWithBrokenAt arrangement line = go line 1 []
  where
    getReplacement :: Char -> Int -> Char
    getReplacement '?' idx = if idx `elem` arrangement then '#' else '.'
    getReplacement c _ = c

    go :: [Char] -> Int -> [Char] -> [Char]
    go [] _ acc = acc
    go (x:xs) idx acc = go xs (idx+1) (acc ++ [getReplacement x idx])

readInputLine :: String -> (String, [Int])
readInputLine line = (row, arrangement)
  where
    [row, arrStr] = splitBy ' ' line
    arrangement = map read $ splitBy ',' arrStr

sumOfArrangementCounts :: [String] -> Int
sumOfArrangementCounts input = sum counts
  where
    xs = map readInputLine input
    counts = map (length . uncurry allPossibleArrangements) xs

unfoldInputLine :: (String, [Int]) -> (String, [Int])
unfoldInputLine (line, arrangement) = (line', arrangement')
  where
    line' = intercalate "?" $ replicate 5 line
    arrangement' = concat $ replicate 5 arrangement


sumOfUnfoldedArrangementCounts :: [String] -> Int
sumOfUnfoldedArrangementCounts input = sum counts
  where
    xs = map (unfoldInputLine . readInputLine) input
    counts = map (length . uncurry allPossibleArrangements) xs
