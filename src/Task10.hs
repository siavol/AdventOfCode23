{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task10 where

import Commons (readInput)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 10 ==="
  input <- readInput "./inputs/task-10.txt"
  print $ stepsToFarthestPoint input

data Field = Vertical | Horizontal | NorthEast | NorthWest | SouthEast | SouthWest | Ground | Start
  deriving (Show, Eq)
instance Read Field where
  readsPrec :: Int -> ReadS Field
  readsPrec _ value = case value of
        "|" -> [(Vertical, "")]
        "-" -> [(Horizontal, "")]
        "L" -> [(NorthEast, "")]
        "J" -> [(NorthWest, "")]
        "7" -> [(SouthWest, "")]
        "F" -> [(SouthEast, "")]
        "." -> [(Ground, "")]
        "S" -> [(Start, "")]
        _ -> error "unexpected field"

data Coord = Coord { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

readPipesMap :: [String] -> Map.Map Coord Field
readPipesMap lines = Map.fromList fields
  where
    fields = concat $ zipWith (\y line -> zipWith (\x field -> (Coord x y, read [field])) [0..] line) [0..] lines

findStart :: Map.Map Coord Field -> Coord
findStart = fst . head . filter (\(_, field) -> field == Start) . Map.toList

move :: Coord -> Direction -> Coord
move (Coord x y) direction = case direction of
  North -> Coord x (y - 1)
  South -> Coord x (y + 1)
  East -> Coord (x + 1) y
  West -> Coord (x - 1) y


connectedToStart :: Map.Map Coord Field -> Coord -> [Direction]
connectedToStart pipesMap start = filter availableDirection [North, South, East, West]
  where
    availableDirection :: Direction -> Bool
    availableDirection direction = getConnection direction field
      where
        nextCoord = move start direction
        field = fromMaybe Ground (Map.lookup nextCoord pipesMap)
    getConnection :: Direction -> Field -> Bool
    getConnection North Vertical = True
    getConnection North SouthWest = True
    getConnection North SouthEast = True
    getConnection South Vertical = True
    getConnection South NorthWest = True
    getConnection South NorthEast = True
    getConnection East Horizontal = True
    getConnection East NorthWest = True
    getConnection East SouthWest = True
    getConnection West Horizontal = True
    getConnection West NorthEast = True
    getConnection West SouthEast = True
    getConnection _ _ = False

goPipeFromStart :: Map.Map Coord Field -> Coord -> Direction -> Int
goPipeFromStart pipesMap start direction = go start direction 1
  where
    go :: Coord -> Direction -> Int -> Int
    go coord direction steps =
      let nextCoord = move coord direction
      in case Map.lookup nextCoord pipesMap of
      Nothing -> error "unexpected end of pipe map"
      Just field -> case field of
        Start -> steps
        _ -> go nextCoord (pipeDirection direction field) (steps + 1)
    pipeDirection :: Direction -> Field -> Direction
    pipeDirection North Vertical = North
    pipeDirection North SouthWest = West
    pipeDirection North SouthEast = East
    pipeDirection South Vertical = South
    pipeDirection South NorthWest = West
    pipeDirection South NorthEast = East
    pipeDirection East Horizontal = East
    pipeDirection East NorthWest = North
    pipeDirection East SouthWest = South
    pipeDirection West Horizontal = West
    pipeDirection West NorthEast = North
    pipeDirection West SouthEast = South
    pipeDirection _ _ = error "unexpected pipe direction"

stepsToFarthestPoint :: [String] -> Int
stepsToFarthestPoint input = floor (fromIntegral fullCycle / 2)
  where
    pipesMap = readPipesMap input
    start = findStart pipesMap
    direction = head $ connectedToStart pipesMap start
    fullCycle = goPipeFromStart pipesMap start direction
