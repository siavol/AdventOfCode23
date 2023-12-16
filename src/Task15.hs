{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Task15 where

import Commons (readInput, splitBy)
import Data.List
import Data.Array ((//))
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 15 ==="
  input <- readInput "./inputs/task-15.txt"
  print $ sumOfHashes $ concat input
  print $ sumOfFocusingPower $ concat input


hashChar :: Int -> Char -> Int
hashChar current char = (current + ord char) * 17 `mod` 256

hashStr :: String -> Int
hashStr = foldl hashChar 0

sumOfHashes :: String -> Int
sumOfHashes input = sum $ map hashStr parts
  where
    parts = splitBy ',' input

data Lens = Lens { label :: String, focalLength :: Int } deriving (Eq)
instance Show Lens where
  show :: Lens -> String
  show (Lens label focalLength) = "[" ++ label ++ " " ++ show focalLength ++ "]"

data Box = Box { boxId :: Int, lenses :: [Lens] } deriving (Eq)
instance Show Box where
  show :: Box -> String
  show (Box id lenses) = "Box " ++ show id ++ ": " ++ show lenses

-- returns [delimeter, part, rest]
splitByOneOf :: [Char] -> String -> [String]
splitByOneOf delimiters input = splitByOneOf' delimiters input []
  where
    splitByOneOf' :: [Char] -> String -> String -> [String]
    splitByOneOf' delimiters input part
      | null input = [part]
      | head input `elem` delimiters = [part, [head input], tail input]
      | otherwise = splitByOneOf' delimiters (tail input) (part ++ [head input])

-- "rn=1" - AddLens (Lens "rn" 1)
-- "cm-"  - RemoveLens "cm"
data Command = AddLens Lens | RemoveLens String deriving (Show, Eq)
instance Read Command where
  readsPrec :: Int -> ReadS Command
  readsPrec _ value = case cmd of
    "=" -> [(AddLens (Lens label focalLength), "")]
    "-" -> [(RemoveLens label, "")]
    _ -> error "unexpected command"
    where
      [label, cmd, focalStr] = splitByOneOf "=-" value
      focalLength = read focalStr

createBoxes :: Int -> Map.Map Int Box
createBoxes n = Map.fromList $ zip [0..] boxes
  where
    boxes = map (`Box` []) [0..n-1]

command :: Map.Map Int Box -> Command -> Map.Map Int Box
command boxes (AddLens lens) =
  let
    boxId = hashStr (label lens)
    box = fromJust (Map.lookup boxId boxes)
    oldLenses = lenses box
    newLenses = case find ((== label lens) . label) oldLenses of
      Just _  -> map (\l -> if label l == label lens then lens else l) oldLenses
      Nothing -> oldLenses ++ [lens]
    newBox = box { lenses =  newLenses }
  in Map.insert boxId newBox boxes
command boxes (RemoveLens delLabel) =
  let boxId = hashStr delLabel
      box = fromJust (Map.lookup boxId boxes)
      newBox = box { lenses = filter (\l -> label l /= delLabel) (lenses box) }
  in Map.insert boxId newBox boxes

boxFocusingPower :: Box -> Int
boxFocusingPower box = sum $ zipWith (curry (\l -> boxNo * fst l * focalLength (snd l))) [1..] (lenses box)
  where
    boxNo = boxId box + 1

sumOfFocusingPower :: String -> Int
sumOfFocusingPower input = sum $ map boxFocusingPower boxes
  where
    commands = map (\p -> read p :: Command) (splitBy ',' input)
    boxes = Map.elems $ foldl command (createBoxes 256) commands
