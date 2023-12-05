module Task5 where

import Data.List (intersect, find)
import Data.Char (isDigit)
import qualified Data.Map as Map
import qualified Data.Maybe

import Commons (readTitledRecord, readInput)

runTask :: IO ()
runTask = do
  print "=== Advent Code 5 ==="
  input <- readInput "./inputs/task-5.txt"
  print $ lowestLocationSeedBySeed input
  print $ lowestLocationBySeedRange input

data Range = Range { destStart :: Int
                   , srcStart :: Int
                   , len :: Int }
                   deriving (Show, Eq)
instance Read Range where
  readsPrec _ s = case words s of
    [destStart, srcStart, len] -> [(Range (read destStart) (read srcStart) (read len), "")]
    _ -> []

listFromRange :: Range -> [(Int, Int)]
listFromRange (Range destStart srcStart len) = zip [srcStart..srcStart+len-1] [destStart..destStart+len-1]

type Transform = [Range]

readMap :: [String] -> Transform
readMap xs = map read xs :: [Range]

fromSource :: Transform -> Int -> Int
fromSource map key = case r of
  Nothing -> key
  Just range -> (destStart range) + (key - (srcStart range))
  where
    r = find (\r -> key >= (srcStart r) && key <= (srcStart r) + (len r)) map

readPipeline :: [String] -> [Transform]
readPipeline xs = go xs [] []
  where
    go [] [] res = res
    go [] acc res = res ++ [readMap acc]
    go (x:xs) acc res
      | null x = go xs [] (res ++ [readMap acc])
      | isDigit (head x) = go xs (acc ++ [x]) res
      | otherwise = go xs acc res

pipeline :: [Transform] -> Int -> Int
pipeline [] x = x
pipeline transforms y = foldl (\acc t -> fromSource t acc) y transforms

lowestLocation :: ([Int] -> [Int]) -> [String] -> Int
lowestLocation f xs = minimum locations
  where
    (_, seedsStr) = readTitledRecord $ head xs
    seeds = f $ map read $ words seedsStr
    transforms = drop 2 xs
    ppln = readPipeline transforms
    locations = map (pipeline ppln) seeds

lowestLocationSeedBySeed :: [String] -> Int
lowestLocationSeedBySeed = lowestLocation id

lowestLocationBySeedRange :: [String] -> Int
lowestLocationBySeedRange = lowestLocation seedRange

seedRange :: [Int] -> [Int]
seedRange = listFromPairs . pairs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "odd number of elements"
pairs (x:y:xs) = (x, y) : pairs xs

listFromPairs :: [(Int, Int)] -> [Int]
listFromPairs = concatMap (\(x, y) -> [x..x+y-1])
