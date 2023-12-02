import Test.HUnit
import qualified System.Exit as Exit
import Task1 (sumAll, mapToInt)

testSumAll :: Test
testSumAll = TestCase
    $ assertEqual "for (sumAll [[1,2,3],[4,5,6],[7,8,9]])"
    [6, 15, 24] (sumAll [[1,2,3],[4,5,6],[7,8,9]])

testMapToInt :: Test
testMapToInt = TestCase
    $ assertEqual "for (mapToInt [[\"1\",\"2\",\"3\"],[\"4\",\"5\",\"6\"]])"
    [[1,2,3], [4,5,6]] (mapToInt [["1","2","3"],["4","5","6"]])

tests = TestList [
    TestLabel "Test sumAll" testSumAll,
    TestLabel "Test mapToInt" testMapToInt
    ]

main :: IO Counts
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
