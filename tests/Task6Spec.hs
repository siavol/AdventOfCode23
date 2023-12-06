module Task6Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Task6 (Race(..), raceWinStrategies, readRaces, numberOfWinWays, readRaceWithoutSpaces)

spec :: Spec
spec = do
  describe "raceWinStrategies" $ do
    it "returns (2, 5) for 7ms race with 9 dist" $ do
      raceWinStrategies Race { time = 7, distance = 9 } `shouldBe` (2, 5)
    it "returns (4, 11) for 15ms race with 40 dist" $ do
      raceWinStrategies Race { time = 15, distance = 40 } `shouldBe` (4, 11)
    it "returns (11, 19) for 30ms race with 200 dist" $ do
      raceWinStrategies Race { time = 30, distance = 200 } `shouldBe` (11, 19)

  describe "readRaces" $ do
    it "returns races from test input" $ do
      let input = [ "Time:      7  15   30"
                  , "Distance:  9  40  200"]
      readRaces input `shouldBe` [Race 7 9, Race 15 40, Race 30 200]

  describe "numberOfWinWays" $ do
    it "returns 288 for test input" $ do
      let input = [ "Time:      7  15   30"
                  , "Distance:  9  40  200"]
      numberOfWinWays input `shouldBe` 288

  describe "readRaceWithoutSpaces" $ do
    it "returns correct race for the test input" $ do
      let input = [ "Time:      7  15   30"
                  , "Distance:  9  40  200"]
      readRaceWithoutSpaces input `shouldBe` Race 71530 940200
