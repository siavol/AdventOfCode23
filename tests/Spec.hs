-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Task2Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Task 2"     Task2Spec.spec
