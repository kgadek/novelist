module System.NovelistSpec (spec) where

-- base
import           Data.List (isSuffixOf)
import           Data.Function (on)

-- fclabels
import           Data.Label

-- Hspec
import           Test.Hspec (Spec, describe, it)

-- HUnit
import           Test.HUnit ((@?=), Assertion)

-- QuickCheck
import           Test.QuickCheck (property, (==>))

-- novelist
import qualified System.Novelist as N


{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec =
    describe "System.Novelist" $ do
      it "exists" $
        True @?= True
      describe "isNameEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          N.isNameEnabled ""                        @?= True
          N.isNameEnabled "abc"                     @?= True
          N.isNameEnabled "abc.disabled_not_really" @?= True
        it "rejects paths ending with .disabled" $ do
          N.isNameEnabled ".disabled"                        @?= False
          N.isNameEnabled "abc.disabled"                     @?= False
          N.isNameEnabled "abc.disabled_not_really.disabled" @?= False
      describe "isEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          N.isEnabled (N.File ""                       ) @?= True
          N.isEnabled (N.File "abc"                    ) @?= True
          N.isEnabled (N.File "abc.disabled_not_really") @?= True
          N.isEnabled (N.Dir ""                        []) @?= True
          N.isEnabled (N.Dir "abc"                     []) @?= True
          N.isEnabled (N.Dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          N.isEnabled (N.File ".disabled"                       ) @?= False
          N.isEnabled (N.File "abc.disabled"                    ) @?= False
          N.isEnabled (N.File "abc.disabled_not_really.disabled") @?= False
          N.isEnabled (N.Dir ".disabled"                        []) @?= False
          N.isEnabled (N.Dir "abc.disabled"                     []) @?= False
          N.isEnabled (N.Dir "abc.disabled_not_really.disabled" []) @?= False
      describe "prune" $ do
        it "works with one-level" $ do
          N.prune [ file1
                  , dir1 [file1, file2]
                  , dir2 [file2, file3]
                  , file2
                  ] `toplevelPathsShouldBe` [ 
                    file1
                  , dir1 [file1, file2]
                  ]
          N.prune [ dir3 [file2, dir2 [], dir1 [], file1]
                  ] `toplevelPathsShouldBe` [
                    dir3 [file2, dir2 [], dir1 [], file1]
                  ]
        it "works with multiple-levels" $ do
          N.prune [ file1
                  , dir1 [file1, file2]
                  , dir2 [file1, file2, file3]
                  ] @?= [
                    file1
                  , dir1 [file1]
                  ]
  where
    file1 = N.File "abc"
    file2 = N.File "def.disabled"
    file3 = N.File "ghi"
    dir1 = N.Dir "mno"
    dir2 = N.Dir "pqr.disabled"
    dir3 = N.Dir "stu"



isNotSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isNotSuffixOf needle haystack = not (needle `isSuffixOf` haystack)

toplevelPathsShouldBe :: [N.Novella] -> [N.Novella] -> Assertion
toplevelPathsShouldBe = (@?=) `on` (get N.name <$>)

