module Task2 where

import Data.List (sort, isPrefixOf)
import Data.Char (digitToInt)
import Control.Monad (foldM)
import Text.Read (readMaybe)

runTask :: IO ()
runTask = do
  print "=== Advent Code 2 ==="
  input <- readInput "./inputs/task-2.txt"
  let maxCubes = SetOfCubes { red=12, green=13, blue=14 }
  print $ sumOfValidIds maxCubes input
  print $ sumOfPowers input


-- reads input from file
readInput :: FilePath -> IO [String]
readInput filename = do
  contents <- readFile filename
  return (lines contents)

data SetOfCubes = SetOfCubes { red :: Int
                             , green :: Int
                             , blue :: Int
                             } deriving (Show, Eq)

data Color = Red | Green | Blue deriving(Show, Read, Eq, Enum)

readColor :: String -> Color
readColor str = case str of
  "red" -> Red
  "green" -> Green
  "blue" -> Blue
  _ -> error "Unknown color"

-- set color to value
setCube :: SetOfCubes -> Int -> Color -> SetOfCubes
setCube set value color = case color of
  Red -> set { red = value }
  Green -> set { green = value }
  Blue -> set { blue = value }

-- returns pairs from array like [1, 2, 3, 4] -> [(1, 2), (3, 4)]
getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [x] = error "Cannot create pair with a single element"
getPairs (x:y:xs) = (x, y) : getPairs xs

parsePairs :: [(String, String)] -> [(Int, Color)]
parsePairs = map (\(a, b) -> (read a, readColor b))

readPairs :: String -> [(Int, Color)]
readPairs str = parsePairs pairs
  where
    w = words $ map (\c -> if c == ',' then ' ' else c) str
    pairs = getPairs w

-- reads SetOfCubes from string like 8 green, 6 blue, 20 red
readSetOfCubes :: String -> SetOfCubes
readSetOfCubes str = foldl (\acc (value, color) -> setCube acc value color) (SetOfCubes 0 0 0) pairs
  where
    pairs = readPairs str

trim :: String -> String
trim = dropWhile (== ' ')

-- returns string after Game N: where N is a number
-- like "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
readGame :: String -> String
readGame str = trim $ go str []
  where
    go :: String -> String -> String
    go [] acc = acc
    go (x:xs) acc
      | x == ':' = xs
      | otherwise = go xs (acc ++ [x])

-- reads record with SetOfCubes like
-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> ["3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green"]
readRecord :: String -> [String]
readRecord str = map trim $ go (readGame str) [] []
  where
    go :: String -> String -> [String] -> [String]
    go [] acc res = res ++ [acc]
    go (x:xs) acc res
      | x == ';' = go xs [] (res ++ [acc])
      | otherwise = go xs (acc ++ [x]) res

-- checks is SetOfCubes is valid
isValid :: SetOfCubes -> SetOfCubes -> Bool
isValid (SetOfCubes rMax gMax bMax) (SetOfCubes r g b) = r <= rMax && g <= gMax && b <= bMax

readRecordSetOfCubes :: String -> [SetOfCubes]
readRecordSetOfCubes = map readSetOfCubes . readRecord . readGame

isRecordValid :: SetOfCubes -> String -> Bool
isRecordValid max str = all (isValid max) sets
  where
    sets = readRecordSetOfCubes str

-- returns sum of valid record ids
sumOfValidIds :: SetOfCubes -> [String] -> Int
sumOfValidIds max records = sum validIds
  where
    recordsWithIds = zip [1..] records
    validIds = map fst $ filter (\t -> isRecordValid max (snd t)) recordsWithIds

-- returns a maximum SetOfCubes from two SetOfCubes.
maxSet :: SetOfCubes -> SetOfCubes -> SetOfCubes
maxSet (SetOfCubes r1 g1 b1) (SetOfCubes r2 g2 b2) = SetOfCubes (max r1 r2) (max g1 g2) (max b1 b2)

-- returns a minimum SetOfCubes from array of SetOfCubes.
-- minimum is defined as a SetOfCubes with maximum red, then maximum green, then maximum blue
minimumSetOfCubes :: [SetOfCubes] -> SetOfCubes
minimumSetOfCubes [] = SetOfCubes 0 0 0
minimumSetOfCubes cubes = foldl1 maxSet cubes

powerOfSetOfCubes :: SetOfCubes -> Int
powerOfSetOfCubes (SetOfCubes r g b) = r * g * b

-- returns sum of power of minimum SetOfCubes from each record
sumOfPowers :: [String] -> Int
sumOfPowers records = sum powers
  where
    sets = map readRecordSetOfCubes records
    minimumSets = map minimumSetOfCubes sets
    powers = map powerOfSetOfCubes minimumSets
