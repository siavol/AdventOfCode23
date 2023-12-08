module CommonsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Commons

spec :: Spec
spec = do
  describe "readTitledRecord" $ do
    it "returns a number and record" $ do
      let (number, record) = readTitledRecord "Card 23: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      number `shouldBe` 23
      record `shouldBe` "41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    it "returns a record when there is no number" $ do
      let (_, record) = readTitledRecord "seeds: 79 14 55"
      record `shouldBe` "79 14 55"

  describe "splitBy" $ do
    it "splits string by comma char" $ do
      let splitted = splitBy ',' "1, 2, 3, 4"
      splitted `shouldBe` ["1", " 2", " 3", " 4"]
    it "splits string by | char" $ do
      let splitted = splitBy '|' "1, 2, 3, 4 | 5, 6, 7, 8"
      splitted `shouldBe` ["1, 2, 3, 4 ", " 5, 6, 7, 8"]

  describe "trimStart" $ do
    it "trims spaces from the start of the string" $ do
      let trimmed = trimStart "   1, 2, 3, 4"
      trimmed `shouldBe` "1, 2, 3, 4"
    it "does not trim spaces from the middle of the string" $ do
      let trimmed = trimStart "1, 2, 3, 4"
      trimmed `shouldBe` "1, 2, 3, 4"
    it "does not trim spaces from the end of the string" $ do
      let trimmed = trimStart "1, 2, 3, 4   "
      trimmed `shouldBe` "1, 2, 3, 4   "

  describe "trimStartBy" $ do
    it "trims ( from the start of the string" $ do
      let trimmed = trimStartBy '(' "(BBB"
      trimmed `shouldBe` "BBB"

  describe "trimEndBy" $ do
    it "trims ) from the end of the string" $ do
      let trimmed = trimEndBy ')' "BBB)"
      trimmed `shouldBe` "BBB"
