{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task16 where

import Commons (readInput)
import Data.List (nub)
import qualified Data.Map as Map
import Prelude hiding (Right, Left)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 16 ==="
  input <- readInput "./inputs/task-16.txt"
  let fieldMap = readField input
  print $ calcEnergizedFields fieldMap (Coord 0 0) Right
  print $ mostEnergized fieldMap


data Direction = Left | Right | Up | Down
  deriving (Show, Eq, Ord)

data Field = Space | MirrorForward | MirrodBackward | SplitHor | SplitVer
  deriving (Show, Eq)

data Coord = Coord { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

readField :: [String] -> Map.Map Coord Char
readField lines = Map.fromList fields
  where
    fields = concat $ zipWith (\y line -> zipWith (\x field -> (Coord x y, field)) [0..] line) [0..] lines

traceBeamStep :: Map.Map Coord Char -> Map.Map (Coord, Direction) Bool -> (Coord, Direction)
  -> (Map.Map (Coord, Direction) Bool, [(Coord, Direction)])
traceBeamStep fieldMap stepsMap (coord, direction) = case Map.lookup (coord, direction) stepsMap of
  Just True -> (stepsMap, [])
  _ -> go field coord
    where
      field = Map.lookup coord fieldMap
      newStepsMap = Map.insert (coord, direction) True stepsMap

      nextCoord :: Coord -> Direction -> Coord
      nextCoord (Coord x y) direction = case direction of
        Left -> Coord (x - 1) y
        Right -> Coord (x + 1) y
        Up -> Coord x (y - 1)
        Down -> Coord x (y + 1)

      go :: Maybe Char -> Coord -> (Map.Map (Coord, Direction) Bool, [(Coord, Direction)])
      go Nothing _ = (stepsMap, [])
      go (Just '.') coord = (newStepsMap, [(nextCoord coord direction, direction)])
      go (Just '/') coord = case direction of
        Left -> (newStepsMap, [(nextCoord coord Down, Down)])
        Right -> (newStepsMap, [(nextCoord coord Up, Up)])
        Up -> (newStepsMap, [(nextCoord coord Right, Right)])
        Down -> (newStepsMap, [(nextCoord coord Left, Left)])
      go (Just '\\') coord = case direction of
        Left -> (newStepsMap, [(nextCoord coord Up, Up)])
        Right -> (newStepsMap, [(nextCoord coord Down, Down)])
        Up -> (newStepsMap, [(nextCoord coord Left, Left)])
        Down -> (newStepsMap, [(nextCoord coord Right, Right)])
      go (Just '|') coord = case direction of
        Up -> (newStepsMap, [(nextCoord coord Up, Up)])
        Down -> (newStepsMap, [(nextCoord coord Down, Down)])
        _ -> (newStepsMap, [(nextCoord coord Up, Up), (nextCoord coord Down, Down)])
      go (Just '-') coord = case direction of
        Left -> (newStepsMap, [(nextCoord coord Left, Left)])
        Right -> (newStepsMap, [(nextCoord coord Right, Right)])
        _ -> (newStepsMap, [(nextCoord coord Left, Left), (nextCoord coord Right, Right)])

traceBeams :: Map.Map Coord Char -> Coord -> Direction -> Map.Map (Coord, Direction) Bool
traceBeams fieldMap coord direction = go fieldMap Map.empty [(coord, direction)]
  where
    go :: Map.Map Coord Char -> Map.Map (Coord, Direction) Bool -> [(Coord, Direction)]
      -> Map.Map (Coord, Direction) Bool
    go fieldMap stepsMap [] = stepsMap
    go fieldMap stepsMap ((coord, direction):steps) =
      let (newStepsMap, newSteps) = traceBeamStep fieldMap stepsMap (coord, direction)
      in go fieldMap newStepsMap (steps ++ newSteps)

calcEnergizedFields :: Map.Map Coord Char -> Coord -> Direction -> Int
calcEnergizedFields fieldMap coord direction = length energizedCoords
  where
    stepsMap = traceBeams fieldMap coord direction
    energizedCoords = nub $ map fst $ Map.keys stepsMap

startingPositions :: Int -> Int -> [(Coord, Direction)]
startingPositions width height = concat [left, right, top, bottom, leftTop, rightTop, leftBottom, rightBottom]
  where
    left = map (\y -> (Coord 0 y, Right)) [0..height]
    right = map (\y -> (Coord width y, Left)) [0..height]
    top = map (\x -> (Coord x 0, Down)) [0..width]
    bottom = map (\x -> (Coord x height, Up)) [0..width]
    leftTop = [(Coord 0 0, Right), (Coord 0 0, Down)]
    rightTop = [(Coord width 0, Left), (Coord width 0, Down)]
    leftBottom = [(Coord 0 height, Right), (Coord 0 height, Up)]
    rightBottom = [(Coord width height, Left), (Coord width height, Up)]

mostEnergized :: Map.Map Coord Char -> Int
mostEnergized fieldMap = maximum $ map (uncurry (calcEnergizedFields fieldMap)) (startingPositions width height)
  where
    width = maximum $ map x $ Map.keys fieldMap
    height = maximum $ map y $ Map.keys fieldMap
