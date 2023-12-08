module Task8Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

import Task8 ( Node(..)
             , Direction(..)
             , readNetLine
             , getPath
             , readTaskInput
             , getPathLength
             , getGhostPathLength
             , getGhostPathLength2)

spec :: Spec
spec = do
  describe "Node" $ do
    it "reads from '(BBB, CCC)'" $ do
      let node = read "(BBB, CCC)" :: Node
      node `shouldBe` Node { left = "BBB", right = "CCC" }

  describe "Direction" $ do
    it "reads from 'L'" $ do
      let direction = read "L" :: Direction
      direction `shouldBe` Left
    it "reads from 'R'" $ do
      let direction = read "R" :: Direction
      direction `shouldBe` Right

  describe "readNetLine" $ do
    it "reads from AAA = (BBB, CCC)" $ do
      let (label, node) = readNetLine "AAA = (BBB, CCC)"
      label `shouldBe` "AAA"
      node `shouldBe` Node { left = "BBB", right = "CCC" }

  describe "getPath" $ do
    it "returns path for test input" $ do
      let netWork = Map.fromList[ ("AAA", Node "BBB" "CCC")
                                , ("BBB", Node "DDD" "EEE")
                                , ("CCC", Node "ZZZ" "GGG")
                                , ("DDD", Node "DDD" "DDD")
                                , ("EEE", Node "EEE" "EEE")
                                , ("GGG", Node "GGG" "GGG")
                                , ("ZZZ", Node "ZZZ" "ZZZ") ]
      let instructions = [Right, Left]
      getPath netWork instructions `shouldBe` [ ("AAA", Right), ("CCC", Left) ]

  describe "readInput" $ do
    it "reads test input" $ do
      let input = [ "LLR"
                  , ""
                  , "AAA = (BBB, BBB)"
                  , "BBB = (AAA, ZZZ)"
                  , "ZZZ = (ZZZ, ZZZ)"]
      let (netWork, instructions) = readTaskInput input
      instructions `shouldBe` [Left, Left, Right]
      netWork `shouldBe` Map.fromList[ ("AAA", Node "BBB" "BBB")
                                     , ("BBB", Node "AAA" "ZZZ")
                                     , ("ZZZ", Node "ZZZ" "ZZZ") ]

  describe "getPathLength" $ do
    it "returns 2 for the first test input" $ do
      let input = ["RL"
                  , ""
                  , "AAA = (BBB, CCC)"
                  , "BBB = (DDD, EEE)"
                  , "CCC = (ZZZ, GGG)"
                  , "DDD = (DDD, DDD)"
                  , "EEE = (EEE, EEE)"
                  , "GGG = (GGG, GGG)"
                  , "ZZZ = (ZZZ, ZZZ)"]
      let (netWork, instructions) = readTaskInput input
      getPathLength netWork instructions `shouldBe` 2

    it "returns 6 for the first test input" $ do
      let input = ["LLR"
                  , ""
                  , "AAA = (BBB, BBB)"
                  , "BBB = (AAA, ZZZ)"
                  , "ZZZ = (ZZZ, ZZZ)"]
      let (netWork, instructions) = readTaskInput input
      getPathLength netWork instructions `shouldBe` 6

  describe "getGhostPathLength" $ do
    it "returns path for test input" $ do
      let netWork = Map.fromList[ ("11A", Node "11B" "XXX")
                                , ("11B", Node "XXX" "11Z")
                                , ("11Z", Node "11B" "XXX")
                                , ("22A", Node "22B" "XXX")
                                , ("22B", Node "22C" "22C")
                                , ("22C", Node "22Z" "22Z")
                                , ("22Z", Node "22B" "22B")
                                , ("XXX", Node "XXX" "XXX") ]
      let instructions = [Left, Right]
      getGhostPathLength netWork instructions `shouldBe` 6

  describe "getGhostPathLength2" $ do
    it "returns path for test input" $ do
      let netWork = Map.fromList[ ("11A", Node "11B" "XXX")
                                , ("11B", Node "XXX" "11Z")
                                , ("11Z", Node "11B" "XXX")
                                , ("22A", Node "22B" "XXX")
                                , ("22B", Node "22C" "22C")
                                , ("22C", Node "22Z" "22Z")
                                , ("22Z", Node "22B" "22B")
                                , ("XXX", Node "XXX" "XXX") ]
      let instructions = [Left, Right]
      getGhostPathLength2 netWork instructions `shouldBe` 6
