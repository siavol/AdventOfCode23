import Test.HUnit
import qualified System.Exit as Exit
import Task1 (
    getDigits,
    isDigit,
    calibrationValue,
    calibrationSum,
    calibrationSumWithReplace,
    getDigitsWithLetters)

testIsDigit_1 :: Test
testIsDigit_1 = TestCase
    $ assertEqual "for (isDigit '1')" True (isDigit '1')

testIsDigit_a :: Test
testIsDigit_a = TestCase
    $ assertEqual "for (isDigit 'a')" False (isDigit 'a')

testGetDigits :: Test
testGetDigits = TestCase
    $ assertEqual "for (getDigits '1abc2')"
    [1, 2] (getDigits "1abc2")

testCalibrationValue :: Test
testCalibrationValue = TestCase
    $ assertEqual "for (calibrationValue '1abc2')"
    12 (calibrationValue "1abc2")

testCalibrationSum :: Test
testCalibrationSum = TestCase
    $ assertEqual "for (calibrationSum <sample input>)"
    142 (calibrationSum ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"])

testGetDigitsWithLetters :: Test
testGetDigitsWithLetters = TestCase
    $ assertEqual "for (digitIndexes 'zoneight234')"
    [1, 8, 2, 3, 4] (getDigitsWithLetters "zoneight234")

testGetDigitsWithLetters2 :: Test
testGetDigitsWithLetters2 = TestCase
    $ assertEqual "for (digitIndexes 'ltgeightwothree5ccxbhssxrsbj')"
    [8, 2, 3, 5] (getDigitsWithLetters "ltgeightwothree5ccxbhssxrsbj")

testCalibrationSumWithReplace :: Test
testCalibrationSumWithReplace = TestCase
    $ assertEqual "for (calibrationSumWithReplace <sample input>)"
    281 (calibrationSumWithReplace ["two1nine",
                                    "eightwothree",
                                    "abcone2threexyz",
                                    "xtwone3four",
                                    "4nineeightseven2",
                                    "zoneight234",
                                    "7pqrstsixteen"])

tests = TestList [
      TestLabel "Test getDigits" testGetDigits
    , TestLabel "Test isDigit" testIsDigit_1
    , TestLabel "Test isDigit" testIsDigit_a
    , TestLabel "Test calibrationValue" testCalibrationValue
    , TestLabel "Test calibrationSum" testCalibrationSum
    , TestLabel "Test testGetDigitsWithLetters" testGetDigitsWithLetters
    , TestLabel "Test testGetDigitsWithLetters2" testGetDigitsWithLetters2
    , TestLabel "Test calibrationSumWithReplace" testCalibrationSumWithReplace
    ]

main :: IO Counts
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
