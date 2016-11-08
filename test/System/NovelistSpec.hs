{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.NovelistSpec (spec) where

-- base
import           Prelude hiding ((.))
import           Control.Category ((.))
import           Control.Monad (liftM)
import           Data.Coerce (coerce)
import           Data.Function (on)
import           Data.List (isSuffixOf)
import           Data.Proxy

-- fclabels
import           Data.Label

-- Hspec
import           Test.Hspec (Spec, describe, it)

-- HUnit
import           Test.HUnit ((@?=), Assertion)

-- QuickCheck
import           Test.QuickCheck (property, (==>))
import qualified Test.QuickCheck as QC

-- novelist
import qualified System.Novelist as N
import           System.Novelist.QuickCheck


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
          N.isEnabled (N.Fix $ N.File ""                       ) @?= True
          N.isEnabled (N.Fix $ N.File "abc"                    ) @?= True
          N.isEnabled (N.Fix $ N.File "abc.disabled_not_really") @?= True
          N.isEnabled (N.Fix $ N.Dir ""                        []) @?= True
          N.isEnabled (N.Fix $ N.Dir "abc"                     []) @?= True
          N.isEnabled (N.Fix $ N.Dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          N.isEnabled (N.Fix $ N.File ".disabled"                       ) @?= False
          N.isEnabled (N.Fix $ N.File "abc.disabled"                    ) @?= False
          N.isEnabled (N.Fix $ N.File "abc.disabled_not_really.disabled") @?= False
          N.isEnabled (N.Fix $ N.Dir ".disabled"                        []) @?= False
          N.isEnabled (N.Fix $ N.Dir "abc.disabled"                     []) @?= False
          N.isEnabled (N.Fix $ N.Dir "abc.disabled_not_really.disabled" []) @?= False
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
        it "is an identify for all-good trees" $ property $
          \(xs :: [ANovella AllEnabled]) ->
            let xxs = coerce xs
             in N.prune xxs == xxs
        it "is a cleaner for all-bad trees" $ property $
          \(xs :: [ANovella AllDisabled]) ->
            let xxs = coerce xs
             in null . N.prune $ xxs
  where
    file1 = N.Fix $ N.File "abc"
    file2 = N.Fix $ N.File "def.disabled"
    file3 = N.Fix $ N.File "ghi"
    dir1 = N.Fix . N.Dir "mno"
    dir2 = N.Fix . N.Dir "pqr.disabled"
    dir3 = N.Fix . N.Dir "stu"


toplevelPathsShouldBe :: [N.Novella] -> [N.Novella] -> Assertion
toplevelPathsShouldBe = (@?=) `on` (get (N.name . N.unFix) <$>)

