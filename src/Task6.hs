module Task6 where

import Commons (readInput, readTitledRecord)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 6 ==="
  input <- readInput "./inputs/task-6.txt"
  print $ numberOfWinWays input
  print $ raceWinStrategiesCount $ readRaceWithoutSpaces input

data Race = Race { time :: Int
                 , distance :: Int } deriving (Show, Eq)

-- returns win strategies for the race.
-- Tuple with bounds of the time to hold the button to win the race.
-- To get it, method solves the equation:
-- -x^2 + time*x - distance > 0
raceWinStrategies :: Race -> (Int, Int)
raceWinStrategies r = (x1, x2)
  where
    sd = sqrt $ fromIntegral $ time r^2 - 4 * distance r
    x1 = floor $ (fromIntegral (time r) - sd) / 2 + 1
    x2 = ceiling $ (fromIntegral (time r) + sd) / 2 - 1

raceWinStrategiesCount :: Race -> Int
raceWinStrategiesCount r = x2 - x1 + 1
  where
    (x1, x2) = raceWinStrategies r

readRaces :: [String] -> [Race]
readRaces input = zipWith Race time distance
  where
    (_, timeStr) = readTitledRecord (head input)
    time = map read $ words timeStr
    (_, distanceStr) = readTitledRecord (input !! 1)
    distance = map read $ words distanceStr

numberOfWinWays :: [String] -> Int
numberOfWinWays input = product winStrategies
  where
    races = readRaces input
    winStrategies = map raceWinStrategiesCount races

readRaceWithoutSpaces :: [String] -> Race
readRaceWithoutSpaces input = Race time distance
  where
    (_, timeStr) = readTitledRecord (head input)
    time = read $ concat $ words timeStr
    (_, distanceStr) = readTitledRecord (input !! 1)
    distance = read $ concat $ words distanceStr
