module Task10Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Task10 ( Coord(..)
              , Field(..)
              , Direction(..)
              , readPipesMap
              , findStart
              , connectedToStart
              , goPipeFromStart
              , stepsToFarthestPoint)

spec :: Spec
spec = do
  describe "readPipesMap" $ do
    it "returns pipe map for simple test input" $ do
      let input = [".S-7.", ".|.|."]
      readPipesMap input `shouldBe` Map.fromList [ (Coord 0 0, Ground)
                                  , (Coord 1 0, Start)
                                  , (Coord 2 0, Horizontal)
                                  , (Coord 3 0, SouthWest)
                                  , (Coord 4 0, Ground)
                                  , (Coord 0 1, Ground)
                                  , (Coord 1 1, Vertical)
                                  , (Coord 2 1, Ground)
                                  , (Coord 3 1, Vertical)
                                  , (Coord 4 1, Ground)
                                  ]

  describe "findStart" $ do
    it "returns start coord for simple test input" $ do
      let input = [".S-7.", ".|.|."]
      findStart (readPipesMap input) `shouldBe` Coord 1 0

  describe "connectedToStart" $ do
    it "returns two directions for simple test input" $ do
      let input = [ "-L|"
                  , "7S-"
                  , "L|7"]
      let pipeMap = readPipesMap input
      connectedToStart pipeMap Coord {x=1, y=1} `shouldBe` [South, East]

  describe "goPipeFromStart" $ do
    it "returns 8 steps simple test input" $ do
      let input = [ "....."
                  , ".S-7."
                  , ".|.|."
                  , ".L-J."
                  , "....."]
      let pipeMap = readPipesMap input
      let start = findStart pipeMap
      goPipeFromStart pipeMap start South `shouldBe` 8

    it "returns 8 steps more complex test input" $ do
      let input = [ "7-F7-"
                  , ".FJ|7"
                  , "SJLL7"
                  , "|F--J"
                  , "LJ.LJ"]
      let pipeMap = readPipesMap input
      let start = findStart pipeMap
      goPipeFromStart pipeMap start South `shouldBe` 16

  describe "stepsToFarthestPoint" $ do
    it "returns 8 steps more complex test input" $ do
      let input = [ "7-F7-"
                  , ".FJ|7"
                  , "SJLL7"
                  , "|F--J"
                  , "LJ.LJ"]
      stepsToFarthestPoint input `shouldBe` 8
