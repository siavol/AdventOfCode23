{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task14 where

import Commons (readInput)
import Data.List

runTask :: IO ()
runTask = do
  print "=== Advent of Code 14 ==="
  input <- readInput "./inputs/task-14.txt"
  print $ levelTotalLoad input

rollRoundRocksRight :: String -> String
rollRoundRocksRight line = go line 0 0 []
  where
    go :: String -> Int -> Int -> String -> String
    go [] rocks spaces acc = acc ++ replicate spaces '.' ++ replicate rocks 'O'
    go (x:xs) rocks spaces acc
      | x == '.' = go xs rocks (spaces + 1) acc
      | x == 'O' = go xs (rocks + 1) spaces acc
      | x == '#' = go xs 0 0 (acc ++ replicate spaces '.' ++ replicate rocks 'O' ++ [x])
      | otherwise = error "Should not happen"

tiltLevelNorth :: [String] -> [String]
tiltLevelNorth lines = transformBack (map rollRoundRocksRight (transform lines))
    where
      transform :: [String] -> [String]
      transform lines = map reverse $ transpose lines
      transformBack :: [String] -> [String]
      transformBack lines = transpose $ map reverse lines

tiltLevelWest :: [String] -> [String]
tiltLevelWest lines = transform (map rollRoundRocksRight (transform lines))
    where
      transform = map reverse


tiltLevelSouth :: [String] -> [String]
tiltLevelSouth lines = transform (map rollRoundRocksRight (transform lines))
    where
      transform = transpose

tiltLevelEast :: [String] -> [String]
tiltLevelEast = map rollRoundRocksRight

tiltCycle :: [String] -> [String]
tiltCycle lines = tiltLevelEast $ tiltLevelSouth $ tiltLevelWest $ tiltLevelNorth lines

tiltNCycles :: Int -> [String] -> [String]
tiltNCycles 0 lines = lines
tiltNCycles n lines = tiltNCycles (n-1) (tiltCycle lines)

levelTotalLoad :: [String] -> Int
levelTotalLoad lines = sum $ zipWith (curry (\p -> rocksCount (fst p) * snd p )) (reverse tiltedLines) [1..]
  where
    tiltedLines = tiltLevelNorth lines
    rocksCount :: String -> Int
    rocksCount line = length $ filter (== 'O') line

levelTotalLoadAfterNCycles :: Int -> [String] -> Int
levelTotalLoadAfterNCycles n lines = levelTotalLoad $ tiltNCycles n lines
