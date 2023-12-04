module Task4Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task4

spec :: Spec
spec = do
  describe "readCard" $ do
    it "returns a Card" $ do
      let card = readCard "Card 12: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      card `shouldBe` Card { cardId = 12
                           , winningNumbers = [41, 48, 83, 86, 17]
                           , cardNumbers = [83, 86, 6, 31, 17, 9, 48, 53] }

  describe "cardPoints" $ do
    it "returns 8 when card has 3 matched numbers" $ do
      let card = Card { cardId = 12
                      , winningNumbers = [41, 48, 83, 86, 17]
                      , cardNumbers = [83, 86, 6, 31, 17, 9, 48, 53] }
      cardPoints card `shouldBe` 8
    it "returns 0 when card has no matched numbers" $ do
      let card = Card { cardId = 12
                      , winningNumbers = [41, 48, 83, 86, 17]
                      , cardNumbers = [1, 2, 3, 4, 5, 6, 7, 8] }
      cardPoints card `shouldBe` 0

  describe "sumPointsFromInput" $ do
    let input = [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
    it "returns 13 for test input" $ do
      sumPointsFromInput input `shouldBe` 13

  describe "addFromIndex" $ do
    it "adds numbers from index" $ do
      let xs = [1, 1, 1, 1, 1, 1, 1, 1]
      let ys =       [1, 2, 3, 4]
      addFromIndex 2 xs ys `shouldBe` [1, 1, 2, 3, 4, 5, 1, 1]

  describe "addCardCopies" $ do
    it "adds 2, 3, 4, 5 copies for test card 1" $ do
      let card = Card { cardId = 1
                      , winningNumbers = [41, 48, 83, 86, 17]
                      , cardNumbers = [83, 86, 6, 31, 17, 9, 48, 53] }
      let xs = [1, 1, 1, 1, 1, 1]
      addCardCopies xs card `shouldBe` [1, 2, 2, 2, 2, 1]
    it "adds 3, 4 copies for test card 2" $ do
      let card = Card { cardId = 2
                      , winningNumbers = [13, 32, 20, 16, 61]
                      , cardNumbers = [61, 30, 68, 82, 17, 32, 24, 19] }
      let xs = [1, 2, 2, 2, 2, 1]
      addCardCopies xs card `shouldBe` [1, 2, 4, 4, 2, 1]

  describe "countScratchcards" $ do
    let input = [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
    it "returns 13 for test input" $ do
      countScratchcards input `shouldBe` 30

