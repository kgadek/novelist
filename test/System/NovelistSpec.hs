{-# LANGUAGE DataKinds #-}
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
{-import           System.Novelist (convertFtoNormal)-}
import qualified System.Novelist.Novelist   as Novelist
import qualified System.Novelist.NovelistF   as NovelistF

--
import qualified System.Novelist.Arbitrary.QuickCheckF as NovelistFQC
import qualified System.Novelist.Arbitrary.QuickCheck as NovelistQC
import           System.Novelist (novellasF)


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
          Novelist.isEnabled (Novelist.file ""                       ) @?= True
          Novelist.isEnabled (Novelist.file "abc"                    ) @?= True
          Novelist.isEnabled (Novelist.file "abc.disabled_not_really") @?= True
          Novelist.isEnabled (Novelist.dir ""                        []) @?= True
          Novelist.isEnabled (Novelist.dir "abc"                     []) @?= True
          Novelist.isEnabled (Novelist.dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          Novelist.isEnabled (Novelist.file ".disabled"                       ) @?= False
          Novelist.isEnabled (Novelist.file "abc.disabled"                    ) @?= False
          Novelist.isEnabled (Novelist.file "abc.disabled_not_really.disabled") @?= False
          Novelist.isEnabled (Novelist.dir ".disabled"                        []) @?= False
          Novelist.isEnabled (Novelist.dir "abc.disabled"                     []) @?= False
          Novelist.isEnabled (Novelist.dir "abc.disabled_not_really.disabled" []) @?= False
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
          NovelistF.isEnabled (NovelistF.file ""                       ) @?= True
          NovelistF.isEnabled (NovelistF.file "abc"                    ) @?= True
          NovelistF.isEnabled (NovelistF.file "abc.disabled_not_really") @?= True
          NovelistF.isEnabled (NovelistF.dir ""                        []) @?= True
          NovelistF.isEnabled (NovelistF.dir "abc"                     []) @?= True
          NovelistF.isEnabled (NovelistF.dir "abc.disabled_not_really" []) @?= True
        it "rejects paths ending with .disabled" $ do
          NovelistF.isEnabled (NovelistF.file ".disabled"                       ) @?= False
          NovelistF.isEnabled (NovelistF.file "abc.disabled"                    ) @?= False
          NovelistF.isEnabled (NovelistF.file "abc.disabled_not_really.disabled") @?= False
          NovelistF.isEnabled (NovelistF.dir ".disabled"                        []) @?= False
          NovelistF.isEnabled (NovelistF.dir "abc.disabled"                     []) @?= False
          NovelistF.isEnabled (NovelistF.dir "abc.disabled_not_really.disabled" []) @?= False
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
          NovelistF.prune [
                    fileF1
                  , dirF1 [fileF1, fileF2, dirF1 [fileF1, fileF2, dirF1 [], dirF2 []]]
                  , dirF2 [fileF1, fileF2, fileF3, dirF1 [fileF1, fileF2]]
                  , fileF2
                  , fileF1
                  , fileF2
                  ] @?= [
                    fileF1
                  , dirF1 [fileF1, dirF1 [fileF1, dirF1 []]]
                  , fileF1
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
          let ys = (get (iso novellasF) . NovelistFQC.unANovella) <$> xs
              xxs = topLevelNames . NovelistF.prune . coerce $ xs
              yys = topLevelNames . Novelist.prune . coerce $ ys
           in xxs == yys
  where
    fileF1 = NovelistF.file "abc"
    fileF2 = NovelistF.file "def.disabled"
    fileF3 = NovelistF.file "ghi"
    dirF1 = NovelistF.Fix2 . NovelistF.Dir "mno"
    dirF2 = NovelistF.Fix2 . NovelistF.Dir "pqr.disabled"
    dirF3 = NovelistF.Fix2 . NovelistF.Dir "stu"
    file1 = Novelist.file "abc"
    file2 = Novelist.file "def.disabled"
    file3 = Novelist.file "ghi"
    dir1 = Novelist.dir "mno"
    dir2 = Novelist.dir "pqr.disabled"
    dir3 = Novelist.dir "stu"

class TopLevelNames a where
    topLevelNames :: [a] -> [String]

instance TopLevelNames NovelistF.Novella where
    topLevelNames = (get (NovelistF.name . NovelistF.unFix2) <$>)

instance TopLevelNames Novelist.Novella where
    topLevelNames = (get Novelist.name <$>)

toplevelPathsFShouldBe :: [NovelistF.Novella] -> [NovelistF.Novella] -> Assertion
toplevelPathsFShouldBe = (@?=) `on` (get (NovelistF.name . NovelistF.unFix2) <$>)


toplevelPathsShouldBe :: [Novelist.Novella] -> [Novelist.Novella] -> Assertion
toplevelPathsShouldBe = (@?=) `on` (get Novelist.name <$>)
