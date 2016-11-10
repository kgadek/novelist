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
import           System.Novelist (convertFtoNormal)
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.QuickCheckF as NovelistFQC
import qualified System.Novelist.Novelist   as Novelist
import qualified System.Novelist.QuickCheck as NovelistQC


{-# ANN module ("HLint: ignore Redundant do"::String) #-}


spec :: Spec
spec = do
    describe "System.Novelist.Novelist" $ do
      describe "isNameEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          Novelist.isNameEnabled ""                        @?= True
          Novelist.isNameEnabled "abc"                     @?= True
          Novelist.isNameEnabled "abc.disabled_not_really" @?= True
        it "rejects paths ending with .disabled" $ do
          Novelist.isNameEnabled ".disabled"                        @?= False
          Novelist.isNameEnabled "abc.disabled"                     @?= False
          Novelist.isNameEnabled "abc.disabled_not_really.disabled" @?= False
      describe "isEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          Novelist.isEnabled (Novelist.File ""                       ) @?= True
          Novelist.isEnabled (Novelist.File "abc"                    ) @?= True
          Novelist.isEnabled (Novelist.File "abc.disabled_not_really") @?= True
          Novelist.isEnabled (Novelist.Dir ""                        []) @?= True
          Novelist.isEnabled (Novelist.Dir "abc"                     []) @?= True
          Novelist.isEnabled (Novelist.Dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          Novelist.isEnabled (Novelist.File ".disabled"                       ) @?= False
          Novelist.isEnabled (Novelist.File "abc.disabled"                    ) @?= False
          Novelist.isEnabled (Novelist.File "abc.disabled_not_really.disabled") @?= False
          Novelist.isEnabled (Novelist.Dir ".disabled"                        []) @?= False
          Novelist.isEnabled (Novelist.Dir "abc.disabled"                     []) @?= False
          Novelist.isEnabled (Novelist.Dir "abc.disabled_not_really.disabled" []) @?= False
      describe "prune" $ do
        it "works with one-level" $ do
          Novelist.prune [ file1
                  , dir1 [file1, file2]
                  , dir2 [file2, file3]
                  , file2
                  ] `toplevelPathsShouldBe` [
                    file1
                  , dir1 [file1, file2]
                  ]
          Novelist.prune [ dir3 [file2, dir2 [], dir1 [], file1]
                  ] `toplevelPathsShouldBe` [
                    dir3 [file2, dir2 [], dir1 [], file1]
                  ]
        it "works with multiple-levels" $ do
          Novelist.prune [ file1
                  , dir1 [file1, file2]
                  , dir2 [file1, file2, file3]
                  ] @?= [
                    file1
                  , dir1 [file1]
                  ]
        it "is an identify for all-good trees" $ property $
          \(xs :: [NovelistQC.ANovella NovelistQC.AllEnabled]) ->
            let xxs = coerce xs
             in Novelist.prune xxs == xxs
        it "is a cleaner for all-bad trees" $ property $
          \(xs :: [NovelistQC.ANovella NovelistQC.AllDisabled]) ->
            let xxs = coerce xs
             in null . Novelist.prune $ xxs

    describe "System.Novelist.NovelistF" $ do
      describe "isNameEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          NovelistF.isNameEnabled ""                        @?= True
          NovelistF.isNameEnabled "abc"                     @?= True
          NovelistF.isNameEnabled "abc.disabled_not_really" @?= True
        it "rejects paths ending with .disabled" $ do
          NovelistF.isNameEnabled ".disabled"                        @?= False
          NovelistF.isNameEnabled "abc.disabled"                     @?= False
          NovelistF.isNameEnabled "abc.disabled_not_really.disabled" @?= False
      describe "isEnabled" $ do
        it "accepts paths not ending with .disabled" $ do
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File ""                       ) @?= True
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File "abc"                    ) @?= True
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File "abc.disabled_not_really") @?= True
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir ""                        []) @?= True
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir "abc"                     []) @?= True
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File ".disabled"                       ) @?= False
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File "abc.disabled"                    ) @?= False
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.File "abc.disabled_not_really.disabled") @?= False
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir ".disabled"                        []) @?= False
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir "abc.disabled"                     []) @?= False
          NovelistF.isEnabled (NovelistF.Fix $ NovelistF.Dir "abc.disabled_not_really.disabled" []) @?= False
      describe "prune" $ do
        it "works with one-level" $ do
          NovelistF.prune [ fileF1
                  , dirF1 [fileF1, fileF2]
                  , dirF2 [fileF2, fileF3]
                  , fileF2
                  ] `toplevelPathsFShouldBe` [ 
                    fileF1
                  , dirF1 [fileF1, fileF2]
                  ]
          NovelistF.prune [ dirF3 [fileF2, dirF2 [], dirF1 [], fileF1]
                  ] `toplevelPathsFShouldBe` [
                    dirF3 [fileF2, dirF2 [], dirF1 [], fileF1]
                  ]
        it "works with multiple-levels" $ do
          NovelistF.prune [ fileF1
                  , dirF1 [fileF1, fileF2]
                  , dirF2 [fileF1, fileF2, fileF3]
                  ] @?= [
                    fileF1
                  , dirF1 [fileF1]
                  ]
        it "is an identify for all-good trees" $ property $
          \(xs :: [NovelistFQC.ANovella NovelistFQC.AllEnabled]) ->
            let xxs = coerce xs
             in NovelistF.prune xxs == xxs
        it "is a cleaner for all-bad trees" $ property $
          \(xs :: [NovelistFQC.ANovella NovelistFQC.AllDisabled]) ->
            let xxs = coerce xs
             in null . NovelistF.prune $ xxs
    describe "System.Novelist._" $ do
      it "implementations are equal" $ property $
        \(xs :: [NovelistFQC.ANovella NovelistFQC.Mixed]) ->
          let ys = (NovelistF.cata convertFtoNormal . NovelistFQC.unANovella) <$> xs
              xxs = topLevelNames . NovelistF.prune . coerce $ xs
              yys = topLevelNames . Novelist.prune . coerce $ ys
           in xxs == yys
  where
    fileF1 = NovelistF.Fix $ NovelistF.File "abc"
    fileF2 = NovelistF.Fix $ NovelistF.File "def.disabled"
    fileF3 = NovelistF.Fix $ NovelistF.File "ghi"
    dirF1 = NovelistF.Fix . NovelistF.Dir "mno"
    dirF2 = NovelistF.Fix . NovelistF.Dir "pqr.disabled"
    dirF3 = NovelistF.Fix . NovelistF.Dir "stu"
    file1 = Novelist.File "abc"
    file2 = Novelist.File "def.disabled"
    file3 = Novelist.File "ghi"
    dir1 = Novelist.Dir "mno"
    dir2 = Novelist.Dir "pqr.disabled"
    dir3 = Novelist.Dir "stu"

class TopLevelNames a where
    topLevelNames :: [a] -> [String]

instance TopLevelNames NovelistF.Novella where
    topLevelNames = (get (NovelistF.name . NovelistF.unFix) <$>)

instance TopLevelNames Novelist.Novella where
    topLevelNames = (get Novelist.name <$>)

toplevelPathsFShouldBe :: [NovelistF.Novella] -> [NovelistF.Novella] -> Assertion
toplevelPathsFShouldBe = (@?=) `on` (get (NovelistF.name . NovelistF.unFix) <$>)


toplevelPathsShouldBe :: [Novelist.Novella] -> [Novelist.Novella] -> Assertion
toplevelPathsShouldBe = (@?=) `on` (get Novelist.name <$>)
