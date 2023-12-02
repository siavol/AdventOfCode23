module Task2Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task2

spec :: Spec
spec = do
  describe "getPairs" $ do
    it "returns pairs from array" $ do
      getPairs [1, 2, 3, 4] `shouldBe` [(1, 2), (3, 4)]
    it "returns empty array for empty array" $ do
      getPairs [] `shouldBe` ([] :: [(Int, Int)])
    it "throws an error for array with one element" $ do
      evaluate (getPairs [1]) `shouldThrow` anyException

  describe "parsePairs" $ do
    it "returns pairs from array of pairs" $ do
      parsePairs [("1", "red"), ("2", "green")] `shouldBe` [(1, "red"), (2, "green")]

  describe "readPairs" $ do
    it "returns pairs from string" $ do
      readPairs "1 red, 2 green" `shouldBe` [(1, "red"), (2, "green")]

  describe "setCube" $ do
    it "sets red cube" $ do
      setCube (SetOfCubes 1 2 3) 2 "red" `shouldBe` SetOfCubes 2 2 3
    it "sets green cube" $ do
      setCube (SetOfCubes 1 2 3) 1 "green" `shouldBe` SetOfCubes 1 1 3
    it "sets blue cube" $ do
      setCube (SetOfCubes 1 2 3) 1 "blue" `shouldBe` SetOfCubes 1 2 1
    it "does not set unknown cube" $ do
      setCube (SetOfCubes 1 2 3) 1 "unknown" `shouldBe` SetOfCubes 1 2 3

  describe "readSetOfCubes" $ do
    it "returns set of cubes" $ do
      readSetOfCubes "8 green, 6 blue, 20 red" `shouldBe` SetOfCubes { red = 20, green = 8, blue = 6 }
    it "returns set of cubes with 0 when no cubes are provided" $ do
      readSetOfCubes "1 blue, 2 green" `shouldBe` SetOfCubes { red = 0, green = 2, blue = 1 }

  describe "readGame" $ do
    it "returns string after Game N: where N is a number" $ do
      readGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

  describe "readRecord" $ do
    it "returns record with set of cubes" $ do
      readRecord "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` ["3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green"]
    it "returns record with set of cubes when no cubes are provided" $ do
      readRecord "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` ["3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green"]

  describe "isValid" $ do
    it "returns true for valid set of cubes" $ do
      isValid (SetOfCubes 1 2 3) (SetOfCubes 1 2 3) `shouldBe` True
    it "returns false for invalid set of cubes" $ do
      isValid (SetOfCubes 1 2 2) (SetOfCubes 1 2 3) `shouldBe` False

  describe "isRecordValid" $ do
    let maxCubes = SetOfCubes { red=12, green=13, blue=14 }
    it "returns true for valid record" $ do
      isRecordValid maxCubes "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` True
    it "returns false for invalid record" $ do
      isRecordValid maxCubes "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" `shouldBe` False

  describe "sumOfValidIds" $ do
    let maxCubes = SetOfCubes { red=12, green=13, blue=14 }
    let testInput = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                     "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                     "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                     "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
                    ]
    it "returns sum of valid ids" $ do
      sumOfValidIds maxCubes testInput `shouldBe` 8

  describe "minimumSetOfCubes" $ do
    let cubes = [SetOfCubes { red=4, green=0, blue=3 },
                 SetOfCubes { red=1, green=2, blue=6 },
                 SetOfCubes { red=0, green=2, blue=0 }
                ]
    it "returns minimum set of cubes" $ do
      minimumSetOfCubes cubes `shouldBe` SetOfCubes { red=4, green=2, blue=6 }

  describe "powerOfSetOfCubes" $ do
    it "returns power of set of cubes" $ do
      powerOfSetOfCubes (SetOfCubes { red=4, green=2, blue=6 }) `shouldBe` 48

  describe "sumOfPowers" $ do
    let testInput = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                     "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                     "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                     "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
                    ]
    it "returns sum of powers" $ do
      sumOfPowers testInput `shouldBe` 2286
