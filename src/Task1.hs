module Task1 where

import Data.List (sort)

runTask :: IO ()
runTask = do
  print "=== Advent Code 1 ==="
  input <- readInput "./inputs/task-1.txt"
  -- print input
  print "hello world"


-- reads input from file
readInput :: FilePath -> IO [String]
readInput filename = do
  contents <- readFile filename
  return (lines contents)
