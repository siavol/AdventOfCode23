module Main where

import System.Environment
import qualified Task1
import qualified Task2

main :: IO ()

main = do
  args <- getArgs
  case args of
    [] -> print "Please provide task number using 'task <number>' arguments"
    ["task", "1"] -> Task1.runTask
    ["task", "2"] -> Task2.runTask
    otherwise -> print "Invalid task number"
