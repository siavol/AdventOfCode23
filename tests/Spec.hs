-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Task2Spec
import qualified Task3Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Task 2"     Task2Spec.spec
  describe "Task 3"     Task3Spec.spec
