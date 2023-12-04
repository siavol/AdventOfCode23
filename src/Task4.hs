module Task4 where

import Data.List (intersect)
import Commons

runTask :: IO ()
runTask = do
  print "=== Advent Code 4 ==="
  input <- readInput "./inputs/task-4.txt"
  print $ sumPointsFromInput input
  print $ countScratchcards input

data Card = Card { cardId :: Int
                 , winningNumbers :: [Int]
                 , cardNumbers :: [Int]
                 } deriving (Show, Eq)

readCard :: String -> Card
readCard s = Card { cardId = id, winningNumbers = winningNumbers, cardNumbers = cardNumbers }
  where
    (id, record) = readTitledRecord s
    parts = splitBy '|' record
    winningNumbers = map read $ words $ head parts
    cardNumbers = map read $ words $ last parts

readCardsInput :: [String] -> [Card]
readCardsInput = map readCard

cardMatches :: Card -> Int
cardMatches card = length $ intersect (winningNumbers card) (cardNumbers card)

cardPoints :: Card -> Int
cardPoints card = if match == 0 then 0 else 2^(match-1)
  where
    match = cardMatches card

sumPointsFromInput :: [String] -> Int
sumPointsFromInput = sum . map (cardPoints . readCard)

addFromIndex :: Int -> [Int] -> [Int] -> [Int]
addFromIndex index xs ys = before ++ added ++ after
  where
    before = take index xs
    after = drop (index + length ys) xs
    added = zipWith (+) (drop index xs) ys

addCardCopies :: [Int] -> Card -> [Int]
addCardCopies xs card = addFromIndex (cardId card) xs copies
  where
    countOfCards = xs !! (cardId card - 1)
    matches = cardMatches card
    copies = replicate matches countOfCards

countScratchcards :: [String] -> Int
countScratchcards input = sum numbers
  where
    cards = readCardsInput input
    numbers = foldl (\acc card -> addCardCopies acc card) (replicate (length cards) 1) cards
