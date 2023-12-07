module Task7Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Task7 ( Card(..)
             , Hand(..)
             , HandType(..)
             , handType
             , readHandsAndBets
             , totalWinnings
             , HandWithJokery (HandWithJokery)
             , replaceJokers
             , compareCardsWithJoker
             , totalWinningsWithJoker)
import Data.List (sortBy)

spec :: Spec
spec = do
  describe "Card" $ do
    it "Ace is greater than King" $ do
      Ace `compare` King `shouldBe` GT
    it "Ace is greater Than Two" $ do
      Ace `compare` Two `shouldBe` GT
    it "King is greater than Queen" $ do
      King `compare` Queen `shouldBe` GT
    it "Five is lower than Six" $ do
      Five `compare` Six `shouldBe` LT

  describe "Hand" $ do
    it "reads hand from '32T3K'" $ do
      (read "32T3K" :: Hand) `shouldBe` Hand [Three, Two, Ten, Three, King]
    it "hand AAAAA is greater than hand AA8AA" $ do
      (read "AAAAA" :: Hand) `compare` (read "AA8AA" :: Hand) `shouldBe` GT
    it "hand QQQJA is greater than hand KTJJT" $ do
      (read "QQQJA" :: Hand) `compare` (read "KTJJT" :: Hand) `shouldBe` GT
    it "hand KK677 is greater than hand KTJJT" $ do
      (read "KK677" :: Hand) `compare` (read "KTJJT" :: Hand) `shouldBe` GT
    it "hand QQQJA is greater that hand T55J5" $ do
      (read "QQQJA" :: Hand) `compare` (read "T55J5" :: Hand) `shouldBe` GT

  describe "handType" $ do
    it "returns Five of a king for 'AAAAA'" $ do
      handType (read "AAAAA" :: Hand) `shouldBe` FiveOfAKind
    it "returns Four of a king for 'AA8AA'" $ do
      handType (read "AA8AA" :: Hand) `shouldBe` FourOfAKind
    it "returns Full house for '23332'" $ do
      handType (read "23332" :: Hand) `shouldBe` FullHouse
    it "returns Three of a kind for 'TTT98'" $ do
      handType (read "TTT98" :: Hand) `shouldBe` ThreeOfAKind
    it "returns Two pairs for '23432'" $ do
      handType (read "23432" :: Hand) `shouldBe` TwoPairs
    it "returns One pair for 'A23A4'" $ do
      handType (read "A23A4" :: Hand) `shouldBe` OnePair
    it "returns High card for 'A2345'" $ do
      handType (read "A2345" :: Hand) `shouldBe` HighCard

  describe "readHandsAndBets" $ do
    it "returns hands and bets for test input" $ do
      let input = [ "32T3K 765"
                  , "T55J5 684"
                  , "KK677 28"]
      readHandsAndBets input `shouldBe` [ (Hand [Three, Two, Ten, Three, King], 765)
                                        , (Hand [Ten, Five, Five, Jack, Five], 684)
                                        , (Hand [King, King, Six, Seven, Seven], 28)]

  describe "totalWinnings" $ do
    it "returns 6440 for test input" $ do
      let input = [ "32T3K 765"
                  , "T55J5 684"
                  , "KK677 28"
                  , "KTJJT 220"
                  , "QQQJA 483"]
      totalWinnings input `shouldBe` 6440

  describe "replaceJokers" $ do
    it "returns the same hand for 32T3K" $ do
      let hand = HandWithJokery [Three, Two, Ten, Three, King]
      replaceJokers hand `shouldBe` HandWithJokery [Three, Two, Ten, Three, King]
    it "returns the same hand for JJJJJ" $ do
      let hand = HandWithJokery [Jack, Jack, Jack, Jack, Jack]
      replaceJokers hand `shouldBe` HandWithJokery [Jack, Jack, Jack, Jack, Jack]
    it "replaces Jokers with 5 in T55J5" $ do
      let hand = HandWithJokery [Ten, Five, Five, Jack, Five]
      replaceJokers hand `shouldBe` HandWithJokery [Ten, Five, Five, Five, Five]
    it "replaces Jokers with Ten in KTJJT" $ do
      let hand = HandWithJokery [King, Ten, Jack, Jack, Ten]
      replaceJokers hand `shouldBe` HandWithJokery [King, Ten, Ten, Ten, Ten]
    it "replaces Jokers with Ace in QQJAA" $ do
      let hand = HandWithJokery [Queen, Queen, Jack, Ace, Ace]
      replaceJokers hand `shouldBe` HandWithJokery [Queen, Queen, Ace, Ace, Ace]

  describe "compareCardsWithJoker" $ do
    it "orders cards with joker" $ do
      sortBy compareCardsWithJoker [Ten, Ace, King, Jack, Two] `shouldBe` [Jack, Two, Ten, King, Ace]

  describe "HandWithJokery" $ do
    it "hand T55J5 is lower than QQQJA" $ do
      let hand1 = HandWithJokery [Ten, Five, Five, Jack, Five]
          hand2 = HandWithJokery [Queen, Queen, Queen, Jack, Ace]
      hand1 `compare` hand2 `shouldBe` LT
    it "hand J2345 is lower than 2J345" $ do
      let hand1 = HandWithJokery [Jack, Two, Three, Four, Five]
          hand2 = HandWithJokery [Two, Jack, Three, Four, Five]
      hand1 `compare` hand2 `shouldBe` LT

  describe "totalWinningsWithJoker" $ do
    it "returns 5905 for test input" $ do
      let input = [ "32T3K 765"
                  , "T55J5 684"
                  , "KK677 28"
                  , "KTJJT 220"
                  , "QQQJA 483"]
      totalWinningsWithJoker input `shouldBe` 5905
