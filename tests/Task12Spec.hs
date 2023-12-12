module Task12Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task12 ( lineSatisfiesArrangement
              , replaceWithBrokenAt
              , allPossibleArrangements
              , readInputLine
              , sumOfArrangementCounts
              , unfoldInputLine
              , sumOfUnfoldedArrangementCounts)
import Data.List.NonEmpty (unfold)

spec :: Spec
spec = do
  describe "lineSatisfiesArrangement" $ do
    it "returns true for '.###.##.#...' with [3,2,1]" $ do
      let result = lineSatisfiesArrangement ".###.##.#..." [3,2,1]
      result `shouldBe` True
    it "returns false for '.#.###.#.######' with [1,3,1,5]" $ do
      let result = lineSatisfiesArrangement ".#.###.#.######" [1,3,1,5]
      result `shouldBe` False

  describe "replaceWithBrokenAt" $ do
    it "replaces with # at indexes" $ do
      let result = replaceWithBrokenAt [1,5] "?.?#?"
      result `shouldBe` "#..##"

  describe "allPossibleArrangements" $ do
    it "returns one possible arrangement for ???.###" $ do
      let result = allPossibleArrangements "???.###" [1,1,3]
      result `shouldBe` ["#.#.###"]
    it "returns 4 possible arrangements for .??..??...?##." $ do
      let result = allPossibleArrangements ".??..??...?##." [1, 1, 3]
      result `shouldBe` [ ".#...#....###.", ".#....#...###."
                        , "..#..#....###.", "..#...#...###."]
    it "returns 1 for ?#?#?#?#?#?#?#? 1,3,1,6" $ do
      let result = allPossibleArrangements "?#?#?#?#?#?#?#?" [1,3,1,6]
      result `shouldBe` [".#.###.#.######"]
    it "returns 1 for ????.#...#... 4,1,1" $ do
      let result = allPossibleArrangements "????.#...#..." [4,1,1]
      result `shouldBe` ["####.#...#..."]
    it "returns 4 for ????.######..#####. 1,6,5" $ do
      let result = allPossibleArrangements "????.######..#####." [1,6,5]
      result `shouldBe` [ "#....######..#####."
                        , ".#...######..#####."
                        , "..#..######..#####."
                        , "...#.######..#####."]
    it "returns 10 for ?###???????? 3,2,1" $ do
      let result = allPossibleArrangements "?###????????" [3,2,1]
      length result `shouldBe` 10

  describe "readInputLine" $ do
    it "returns ('.###.##.#...', [3,2,1]) for '.###.##.#... 3,2,1'" $ do
      let result = readInputLine ".###.##.#... 3,2,1"
      result `shouldBe` (".###.##.#...", [3,2,1])

  describe "sumOfArrangementCounts" $ do
    it "returns 21 for test input" $ do
      let input = [ "???.### 1,1,3"
                  , ".??..??...?##. 1,1,3"
                  , "?#?#?#?#?#?#?#? 1,3,1,6"
                  , "????.#...#... 4,1,1"
                  , "????.######..#####. 1,6,5"
                  , "?###???????? 3,2,1"]
      sumOfArrangementCounts input `shouldBe` 21

  describe "unfoldInputLine" $ do
    it "returns expected result for test input" $ do
      let input = ("???.###", [1,1,3])
      unfoldInputLine input `shouldBe` ("???.###????.###????.###????.###????.###", [1,1,3,1,1,3,1,1,3,1,1,3,1,1,3])

  describe "sumOfUnfoldedArrangementCounts" $ do
    xit "returns 525152 for test input" $ do
      let input = [ "???.### 1,1,3"
                  , ".??..??...?##. 1,1,3"
                  , "?#?#?#?#?#?#?#? 1,3,1,6"
                  , "????.#...#... 4,1,1"
                  , "????.######..#####. 1,6,5"
                  , "?###???????? 3,2,1"]
      sumOfUnfoldedArrangementCounts input `shouldBe` 525152

