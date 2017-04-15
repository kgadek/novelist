{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Novelist.NovelistSpec (spec) where

-- base
import           Prelude hiding ((.))
import           Control.Category ((.))
import           Data.Coerce (coerce)
import           Data.Function (on)

-- microlens
import           Lens.Micro.Extras

-- Hspec
import           Test.Hspec (Spec, describe, it)

-- HUnit
import           Test.HUnit ((@?=), Assertion)

-- QuickCheck
import qualified Test.QuickCheck as QC

-- novelist
import qualified Novelist.NovelistF as NovelistF
import qualified Novelist.Types     as NovelistF

--
import qualified Novelist.Arbitrary.QuickCheckF as NovelistFQC


{-# ANN module ("HLint: ignore Redundant do"::String) #-}


spec :: Spec
spec = do
    describe "Novelist.NovelistF" $ do
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
        it "is an identify for all-good trees" $ QC.property $
          \(xs :: [NovelistFQC.ANovella 'NovelistFQC.AllEnabled]) ->
            let xxs = coerce xs
             in NovelistF.prune xxs == xxs
        it "is a cleaner for all-bad trees" $ QC.property $
          \(xs :: [NovelistFQC.ANovella 'NovelistFQC.AllDisabled]) ->
            let xxs = coerce xs
             in null . NovelistF.prune $ xxs
  where
    fileF1 = NovelistF.file "abc"
    fileF2 = NovelistF.file "def.disabled"
    fileF3 = NovelistF.file "ghi"
    dirF1 = NovelistF.Fix2 . NovelistF.Dir "mno"
    dirF2 = NovelistF.Fix2 . NovelistF.Dir "pqr.disabled"
    dirF3 = NovelistF.Fix2 . NovelistF.Dir "stu"

class TopLevelNames a where
    topLevelNames :: [a] -> [String]

instance TopLevelNames NovelistF.Novella where
    topLevelNames = (view (NovelistF.unFix2 . NovelistF.name) <$>)

toplevelPathsFShouldBe :: [NovelistF.Novella] -> [NovelistF.Novella] -> Assertion
toplevelPathsFShouldBe = (@?=) `on` (view (NovelistF.unFix2 . NovelistF.name) <$>)
