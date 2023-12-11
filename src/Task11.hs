{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task11 where

import Commons (readInput)
import Data.List (sort, (\\))

runTask :: IO ()
runTask = do
  print "=== Advent of Code 11 ==="
  input <- readInput "./inputs/task-11.txt"
  print $ starsDistanceForSpace input (2-1)
  print $ starsDistanceForSpace input (1000000-1)


data Coord = Coord { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

readSpace :: [String] -> [Coord]
readSpace lines = map fst $ stars allItems
  where
    allItems = concat $ zipWith (\y line -> zipWith (\x field -> (Coord x y, field)) [1..] line) [1..] lines
    stars :: [(Coord, Char)] -> [(Coord, Char)]
    stars = filter (\(_, field) -> field == '#')

expandX :: Int -> Int -> [Coord] -> [Coord]
expandX extValue extX = map (\(Coord x y) -> if x > extX then Coord (x + extValue) y else Coord x y)

expandY :: Int -> Int -> [Coord] -> [Coord]
expandY extValue extY = map (\(Coord x y) -> if y > extY then Coord x (y + extValue) else Coord x y)

expandSpace :: Int -> [Coord] -> [Coord]
expandSpace extValue stars = foldr (expandX extValue) (foldr (expandY extValue) stars extYs) extXs
  where
    maxY = maximum $ map y stars
    extYs = sort $ [1..maxY] \\ map y stars
    maxX = maximum $ map x stars
    extXs = sort $ [1..maxX] \\ map x stars

distance :: Coord -> Coord -> Int
distance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

starsDistanceSum :: [Coord] -> Int
starsDistanceSum stars = sum $ map (\p -> distance (fst p) (snd p)) starPairs
  where
    starPairs = [(s1, s2) | s1 <- stars, s2 <- stars, s1 /= s2]

starsDistanceForSpace :: [String] -> Int -> Int
starsDistanceForSpace lines extValue = starsDistanceSum (expandSpace extValue $ readSpace lines) `div` 2
