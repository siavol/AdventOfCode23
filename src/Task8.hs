{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task8 where

import Data.List (maximumBy, group, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Commons (trimStartBy, splitBy, trimStart, trimEndBy, trimEnd, readInput)
import Prelude hiding (Left, Right)
import Data.Maybe (fromJust)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 8 ==="
  input <- readInput "./inputs/task-8.txt"
  let (netWork, instructions) = readTaskInput input
  print $ getPathLength netWork instructions
  print $ getGhostPathLength2 netWork instructions

data Node = Node { left :: String, right :: String }
  deriving (Show, Eq)
instance Read Node where
  readsPrec :: Int -> ReadS Node
  readsPrec _ value = [(Node left right, "")]
    where
      [leftPart, rightPart] = splitBy ',' value
      left = trimStartBy '(' leftPart
      right = trimStart $ trimEndBy ')' rightPart

data Direction = Left | Right
  deriving (Show, Eq)
instance Read Direction where
  readsPrec :: Int -> ReadS Direction
  readsPrec _ value = case value of
        "L" -> [(Left, "")]
        "R" -> [(Right, "")]
        _ -> error "unexpected direction"

readNetLine :: String -> (String, Node)
readNetLine str = (label, node)
  where
    [part1, part2] = splitBy '=' str
    label = trimEnd part1
    nodeStr = trimStart part2
    node = read nodeStr

readTaskInput :: [String] -> (Map.Map String Node, [Direction])
readTaskInput input = (netWork, instructions)
  where
    readDirecton :: Char -> Direction
    readDirecton c = read [c]
    instrunctionStr = head input
    instructions = map readDirecton instrunctionStr
    netWork = Map.fromList $ map readNetLine (drop 2 input)

getPath' :: Map.Map String Node -> [Direction] -> String -> [String] -> [(String, Direction)]
getPath' netWork instructions start ends = go False start instructions []
  where
    go :: Bool -> String -> [Direction] -> [(String, Direction)] -> [(String, Direction)]
    go True _ _ res = res
    go False label [] res = go False label instructions res
    go False label (x:xs) res =
      case x of
        Left -> go (left node `elem` ends) (left node) xs (res ++ [(label, x)])
        Right -> go  (right node `elem` ends) (right node) xs (res ++ [(label, x)])
      where
        node = fromJust $ Map.lookup label netWork

getPath :: Map.Map String Node -> [Direction] -> [(String, Direction)]
getPath netWork instructions = getPath' netWork instructions "AAA" ["ZZZ"]

getPathLength :: Map.Map String Node -> [Direction] -> Int
getPathLength netWork instructions = length $ getPath netWork instructions

getGhostPathLength :: Map.Map String Node -> [Direction] -> Int
getGhostPathLength netWork instructions = go False (getALabels $ Map.keys netWork) instructions 0
  where
    getALabels :: [String] -> [String]
    getALabels = filter (\label -> last label == 'A')

    go :: Bool -> [String] -> [Direction] -> Int -> Int
    go True _ _ res = res
    go False labels [] res = go False labels instructions res
    go False labels (x:xs) res =
      case x of
        Left -> go leftNodesStop leftNodes xs (res + 1)
        Right -> go rightNodesStop rightNodes xs (res + 1)
      where
        nodes = map (\label -> fromJust $ Map.lookup label netWork) labels
        leftNodes = map left nodes
        rightNodes = map right nodes
        leftNodesStop = all (\label -> last label == 'Z') leftNodes
        rightNodesStop = all (\label -> last label == 'Z') rightNodes

getGhostPathLength2 :: Map.Map String Node -> [Direction] -> Int
getGhostPathLength2 netWork instructions = getLCM pathLens
  where
    startLabels = filter (\label -> last label == 'A') $ Map.keys netWork
    endLabels = filter (\label -> last label == 'Z') $ Map.keys netWork
    pathLens = map (\start -> length $ getPath' netWork instructions start endLabels) startLabels

getLCM :: [Int] -> Int
getLCM = foldl1 lcm
