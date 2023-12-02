module Task1 where

import Data.List (sort, isPrefixOf)
import Data.Char (digitToInt)

runTask :: IO ()
runTask = do
  print "=== Advent Code 1 ==="
  input <- readInput "./inputs/task-1.txt"
  print $ calibrationSum input
  print $ calibrationSumWithReplace input
  -- print "====================="
  -- print $ debugOutput input


-- reads input from file
readInput :: FilePath -> IO [String]
readInput filename = do
  contents <- readFile filename
  return (lines contents)

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

-- returns digits from string like "1abc2" -> [1, 2]
getDigits :: String -> [Int]
getDigits = map digitToInt . filter isDigit

-- returns calibration value
calibrationValue :: String -> Int
calibrationValue s = a * 10 + b
  where
    digits = getDigits s
    a = head digits
    b = last digits

-- returns a maybe tuple of (digit, length) when string starts with digit or letter
-- like "1abc" -> (1, 1)
-- like "oneabc" -> (1, 3)
startsWithDigitOrLetter :: String -> Maybe (Int, Int)
startsWithDigitOrLetter s
  | isDigit (head s) = Just (digitToInt (head s), 1)
  | isPrefixOf "one" s = Just (1, 3)
  | isPrefixOf "two" s = Just (2, 3)
  | isPrefixOf "three" s = Just (3, 5)
  | isPrefixOf "four" s = Just (4, 4)
  | isPrefixOf "five" s = Just (5, 4)
  | isPrefixOf "six" s = Just (6, 3)
  | isPrefixOf "seven" s = Just (7, 5)
  | isPrefixOf "eight" s = Just (8, 5)
  | isPrefixOf "nine" s = Just (9, 4)
  | otherwise = Nothing

-- returns digits from string
getDigitsWithLetters :: String -> [Int]
getDigitsWithLetters str = go str []
  where
    go :: String -> [Int] -> [Int]
    go s acc
      | null s = acc
      | Just (i, len) <- startsWithDigitOrLetter s = go (tail s) (acc ++ [i])
      | otherwise = go (tail s) acc

calibrationValueWithLetters :: String -> Int
calibrationValueWithLetters s = a * 10 + b
  where
    digits = getDigitsWithLetters s
    a = head digits
    b = last digits

-- returns sum of calibration values
calibrationSum :: [String] -> Int
calibrationSum = sum . map calibrationValue

calibrationSumWithReplace :: [String] -> Int
calibrationSumWithReplace = sum . map calibrationValueWithLetters

debugOutput :: [String] -> [(Int, String)]
debugOutput = map (\s -> (calibrationValueWithLetters s, s))
