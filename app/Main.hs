module Main where

import System.Environment
import qualified Task1
import qualified Task2
import qualified Task3
import qualified Task4
import qualified Task5
import qualified Task6
import qualified Task7
import qualified Task8
import qualified Task9
import qualified Task10
import qualified Task11
import qualified Task12
import qualified Task13
import qualified Task14
import qualified Task15
import qualified Task16

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
    ["task", "6"] -> Task6.runTask
    ["task", "7"] -> Task7.runTask
    ["task", "8"] -> Task8.runTask
    ["task", "9"] -> Task9.runTask
    ["task", "10"] -> Task10.runTask
    ["task", "11"] -> Task11.runTask
    ["task", "12"] -> Task12.runTask
    ["task", "13"] -> Task13.runTask
    ["task", "14"] -> Task14.runTask
    ["task", "15"] -> Task15.runTask
    ["task", "16"] -> Task16.runTask
    otherwise -> print "Invalid task number"
