{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task9 where

import Commons (readInput)
import Task3 (Coord(xEnd))

runTask :: IO ()
runTask = do
  print "=== Advent of Code 9 ==="
  input <- readInput "./inputs/task-9.txt"
  print $ sumOfExtrapolatedLasts input
  print $ sumOfExtrapolatedHeads input


difference :: [Int] -> [Int]
difference [] = []
difference [x] = []
difference (x:y:xs) = y - x : difference (y:xs)

diffSequnce :: [Int] -> [[Int]]
diffSequnce [] = []
diffSequnce [x] = [[x]]
diffSequnce xs = go (allZero xs) xs [xs]
  where
    allZero :: [Int] -> Bool
    allZero = all (== 0)

    go :: Bool -> [Int] -> [[Int]] -> [[Int]]
    go True _ acc = acc
    go False xs acc = go (allZero xs') xs' (xs' : acc)
      where
        xs' = difference xs

propagateDiff :: [[Int]] -> [Int]
propagateDiff [] = []
propagateDiff [x] = x
propagateDiff (x:y:xs) = propagateDiff (y':xs)
  where
    lastDiff = last x
    headDiff = head x
    y' = [head y - headDiff] ++ y ++ [last y + lastDiff]

valuesHistory :: [String] -> [[Int]]
valuesHistory = map (map read . words)

sumOfExtrapolatedLasts :: [String] -> Int
sumOfExtrapolatedLasts = sum . map (last . propagateDiff . diffSequnce) . valuesHistory

sumOfExtrapolatedHeads :: [String] -> Int
sumOfExtrapolatedHeads = sum . map (head . propagateDiff . diffSequnce) . valuesHistory
