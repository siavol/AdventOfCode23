module Main where

import System.Environment
import qualified Task1
import qualified Task2
import qualified Task3
import qualified Task4
import qualified Task5

main :: IO ()

main = do
  args <- getArgs
  case args of
    [] -> print "Please provide task number using 'task <number>' arguments"
    ["task", "1"] -> Task1.runTask
    ["task", "2"] -> Task2.runTask
    ["task", "3"] -> Task3.runTask
    ["task", "4"] -> Task4.runTask
    ["task", "5"] -> Task5.runTask
    otherwise -> print "Invalid task number"
