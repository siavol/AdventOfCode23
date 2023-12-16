{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task14 where

import Commons (readInput)
import Data.List
import qualified Data.Map as Map

runTask :: IO ()
runTask = do
  print "=== Advent of Code 14 ==="
  input <- readInput "./inputs/task-14.txt"
  print $ levelTotalLoadAfterTiltNorh input
  print $ levelTotalLoadAfterNCycles 1000000000 input

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
tiltNCycles cycles lines = go cycles lines Map.empty
  where
    go :: Int -> [String] -> Map.Map [String] (Int, [String]) -> [String]
    go 0 lines _ = lines
    go n lines cache = case Map.lookup lines cache of
      Just cached -> let
        (prevCycle, tilted) = cached
        patternLen = prevCycle - n
        shortcut = (n `mod` patternLen) - 1
        in go shortcut tilted Map.empty
      Nothing -> let
        lines' = tiltCycle lines
        in go (n-1) lines' (Map.insert lines (n, lines') cache)

levelTotalLoad :: [String] -> Int
levelTotalLoad lines = sum $ zipWith (curry (\p -> rocksCount (fst p) * snd p )) (reverse lines) [1..]
  where
    rocksCount :: String -> Int
    rocksCount line = length $ filter (== 'O') line

levelTotalLoadAfterTiltNorh :: [String] -> Int
levelTotalLoadAfterTiltNorh lines = levelTotalLoad $ tiltLevelNorth lines

levelTotalLoadAfterNCycles :: Int -> [String] -> Int
levelTotalLoadAfterNCycles n lines = levelTotalLoad $ tiltNCycles n lines
