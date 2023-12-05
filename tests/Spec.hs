-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified CommonsSpec
import qualified Task2Spec
import qualified Task3Spec
import qualified Task4Spec
import qualified Task5Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Commons"    CommonsSpec.spec
  describe "Task 2"     Task2Spec.spec
  describe "Task 3"     Task3Spec.spec
  describe "Task 4"     Task4Spec.spec
  describe "Task 5"     Task5Spec.spec
