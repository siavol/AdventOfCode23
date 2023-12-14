module Task13Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task13
import Data.List.NonEmpty (unfold)
import Data.Type.Coercion (trans)
import Data.List (transpose)

spec :: Spec
spec = do
  describe "findReflection" $ do
    it "returns [4] for horizontal second test input" $ do
      let input = [ "#...##..#"
                  , "#....#..#"
                  , "..##..###"
                  , "#####.##."
                  , "#####.##."
                  , "..##..###"
                  , "#....#..#"]
      findReflection input `shouldBe` [4]
    it "returns [5] for vertical first test input" $ do
      let input  = [ "#.##..##."
                   , "..#.##.#."
                   , "##......#"
                   , "##......#"
                   , "..#.##.#."
                   , "..##..##."
                   , "#.#.##.#."]
      findReflection (transpose input) `shouldBe` [5]
    it "returns [] for vertical first test input" $ do
      let input  = [ "#.##..##."
                   , "..#.##.#."
                   , "##......#"
                   , "##......#"
                   , "..#.##.#."
                   , "..##..##."
                   , "#.#.##.#."]
      findReflection input `shouldBe` []

  describe "summarisePattern" $ do
    it "returns 5 for first test input" $ do
      let input  = [ "#.##..##."
                   , "..#.##.#."
                   , "##......#"
                   , "##......#"
                   , "..#.##.#."
                   , "..##..##."
                   , "#.#.##.#."]
      summarisePattern input `shouldBe` 5
    it "returns 400 for second test input" $ do
      let input = [ "#...##..#"
                  , "#....#..#"
                  , "..##..###"
                  , "#####.##."
                  , "#####.##."
                  , "..##..###"
                  , "#....#..#"]
      summarisePattern input `shouldBe` 400

  describe "summariseAllPatterns" $ do
    it "returns 405 for test input" $ do
      let input = [ "#.##..##."
                  , "..#.##.#."
                  , "##......#"
                  , "##......#"
                  , "..#.##.#."
                  , "..##..##."
                  , "#.#.##.#."
                  , ""
                  , "#...##..#"
                  , "#....#..#"
                  , "..##..###"
                  , "#####.##."
                  , "#####.##."
                  , "..##..###"
                  , "#....#..#"]
      summariseAllPatterns input `shouldBe` 405
