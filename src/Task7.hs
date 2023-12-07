{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Task7 where

import Data.List (maximumBy, group, sort, sortBy)
import Data.Ord (comparing)
import Commons (splitBy, readInput)

runTask :: IO ()
runTask = do
  print "=== Advent of Code 7 ==="
  input <- readInput "./inputs/task-7.txt"
  print $ totalWinnings input
  print $ totalWinningsWithJoker input

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show)
instance Read Card where
  readsPrec :: Int -> ReadS Card
  readsPrec _ value =
    case value of
      "A" -> [(Ace, "")]
      "K" -> [(King, "")]
      "Q" -> [(Queen, "")]
      "J" -> [(Jack, "")]
      "T" -> [(Ten, "")]
      "9" -> [(Nine, "")]
      "8" -> [(Eight, "")]
      "7" -> [(Seven, "")]
      "6" -> [(Six, "")]
      "5" -> [(Five, "")]
      "4" -> [(Four, "")]
      "3" -> [(Three, "")]
      "2" -> [(Two, "")]
      _   -> []

newtype Hand = Hand [Card]
  deriving (Eq, Show)
instance Read Hand where
  readsPrec :: Int -> ReadS Hand
  readsPrec _ value = [(Hand $ map readCard value, "")]
    where
      readCard :: Char -> Card
      readCard c = read [c]
instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare hand1 hand2 =
    let
      handType1 = handType hand1
      handType2 = handType hand2
    in
      case compare handType1 handType2 of
        LT -> LT
        GT -> GT
        EQ -> let (Hand cards1) = hand1
                  (Hand cards2) = hand2
              in cards1 `compare` cards2

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Show)

-- returns a hand type for a given hand
-- - Five of a kind, where all five cards have the same label
-- - Four of a kind, where four cards have the same label
-- - Full house, where three cards have the same label and the remaining two cards have the same label
-- - Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand
-- - Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label
-- - One pair, where two cards share one label, and the other three cards have a different label from the pair and each other
-- - High card, where all cards' labels are distinct
handType :: Hand -> HandType
handType (Hand cards) =
  let
    sortedCards = reverse . sort $ cards
    cardCounts = reverse . sort . map length . group $ sortedCards
  in
    case cardCounts of
      [5] -> FiveOfAKind
      [4, 1] -> FourOfAKind
      [3, 2] -> FullHouse
      [3, 1, 1] -> ThreeOfAKind
      [2, 2, 1] -> TwoPairs
      [2, 1, 1, 1] -> OnePair
      [1, 1, 1, 1, 1] -> HighCard
      _ -> error "Unknown hand type"

readHandsAndBets :: [String] -> [(Hand, Int)]
readHandsAndBets = map readHandAndBet
  where
    readHandAndBet :: String -> (Hand, Int)
    readHandAndBet line =
      let
        [hand, bet] = splitBy ' ' line
      in
        (read hand, read bet)

totalWinnings :: [String] -> Int
totalWinnings input = sum $ map (uncurry (*)) orderedBets
  where
    handsAndBets = readHandsAndBets input
    orderedHandsAndBets = sortBy (flip (\(hand1, _) (hand2, _) -> hand2 `compare` hand1)) handsAndBets
    orderedBets = zip (map snd orderedHandsAndBets) [1..]

newtype HandWithJokery = HandWithJokery [Card]
  deriving (Eq, Show)
instance Read HandWithJokery where
  readsPrec :: Int -> ReadS HandWithJokery
  readsPrec _ value = [(HandWithJokery $ map readCard value, "")]
    where
      readCard :: Char -> Card
      readCard c = read [c]
instance Ord HandWithJokery where
  compare :: HandWithJokery -> HandWithJokery -> Ordering
  compare hand1 hand2 =
    let
      handType1 = handWithJokerType $ replaceJokers hand1
      handType2 = handWithJokerType $ replaceJokers hand2
    in
      case compare handType1 handType2 of
        LT -> LT
        GT -> GT
        EQ -> let (HandWithJokery cards1) = hand1
                  (HandWithJokery cards2) = hand2
              in cards1 `compareCardListsWithJoker` cards2

replaceJokers :: HandWithJokery -> HandWithJokery
replaceJokers xs = HandWithJokery $ map replaceIfJoker cards
  where
    (HandWithJokery cards) = xs
    noJokers = filter (/= Jack) cards
    mostFrequentElement = if not (null noJokers)
                          then maximumBy (comparing length) (group (sort noJokers))
                          else [Jack]
    replaceIfJoker x
      | x == Jack = head mostFrequentElement
      | otherwise = x

handWithJokerType :: HandWithJokery -> HandType
handWithJokerType (HandWithJokery cards) =
  let
    sortedCards = reverse . sort $ cards
    cardCounts = reverse . sort . map length . group $ sortedCards
  in
    case cardCounts of
      [5] -> FiveOfAKind
      [4, 1] -> FourOfAKind
      [3, 2] -> FullHouse
      [3, 1, 1] -> ThreeOfAKind
      [2, 2, 1] -> TwoPairs
      [2, 1, 1, 1] -> OnePair
      [1, 1, 1, 1, 1] -> HighCard
      _ -> error "Unknown hand type"

compareCardsWithJoker :: Card -> Card -> Ordering
compareCardsWithJoker Jack Jack = EQ
compareCardsWithJoker Jack _    = LT
compareCardsWithJoker _    Jack = GT
compareCardsWithJoker c1   c2   = c1 `compare` c2

compareCardListsWithJoker :: [Card] -> [Card] -> Ordering
compareCardListsWithJoker [] [] = EQ
compareCardListsWithJoker (x:xs) (y:ys) = case x `compareCardsWithJoker` y of
  EQ -> compareCardListsWithJoker xs ys
  LT -> LT
  GT -> GT

readHandsWithJokerAndBets :: [String] -> [(HandWithJokery, Int)]
readHandsWithJokerAndBets = map readHandAndBet
  where
    readHandAndBet :: String -> (HandWithJokery, Int)
    readHandAndBet line =
      let
        [hand, bet] = splitBy ' ' line
      in
        (read hand, read bet)

totalWinningsWithJoker :: [String] -> Int
totalWinningsWithJoker input = sum $ map (uncurry (*)) orderedBets
  where
    handsAndBets = readHandsWithJokerAndBets input
    orderedHandsAndBets = sortBy (flip (\(hand1, _) (hand2, _) -> hand2 `compare` hand1)) handsAndBets
    orderedBets = zip (map snd orderedHandsAndBets) [1..]
