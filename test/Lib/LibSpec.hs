module Lib.LibSpec (spec) where

-- Hspec
import           Test.Hspec (Spec, describe, it)

-- HUnit
import           Test.HUnit ((@?=))

-- QuickCheck
import           Test.QuickCheck (property)

-- novelist
import qualified Lib as N


spec :: Spec
spec =
  describe "Lib" $
    describe "Lib.bar" $ do
      it "works with positive ints" $
        5 @?= N.bar 4
      it "works with negative ints" $ 0 @?= N.bar (-1)
      it "prop: is equal to lambda" . property $
        \x -> (x + 1) @?= N.bar x
