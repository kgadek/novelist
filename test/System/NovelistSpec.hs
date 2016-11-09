{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.NovelistSpec (spec) where

-- base
import           Data.List (isSuffixOf)
import           Data.Function (on)
import           Control.Monad (liftM)
import           Data.Coerce (coerce)

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


{-# ANN module ("HLint: ignore Redundant do"::String) #-}


-- |Use a biased coin, with ratio N:1.
biasedCoin :: Int -> QC.Gen Bool
biasedCoin k = (<k) <$> QC.choose (0,k)

data Validity
  = AllEnabled
  | AllDisabled
  | Mixed


arbitraryNovellaName :: Validity -> QC.Gen String
arbitraryNovellaName AllEnabled  = QC.arbitrary `QC.suchThat` (".disabled" `isNotSuffixOf`)
arbitraryNovellaName AllDisabled = (++".disabled") <$> QC.arbitrary
arbitraryNovellaName Mixed       = ifM (biasedCoin 4)
                                     (arbitraryNovellaName AllEnabled)
                                     (arbitraryNovellaName AllDisabled)

arbitraryNovella :: Validity -> QC.Gen N.Novella
arbitraryNovella v = QC.sized arb
  where arb n
          | n <= 0    = N.File <$> arbitraryNovellaName v
          | otherwise = QC.scale (`div` 2) $
              ifM (biasedCoin 5)
                 (N.Dir <$> arbitraryNovellaName v <*> QC.listOf (arbitraryNovella v))
                 (arb 0)

newtype ANovella (v :: Validity)
  = ANovella N.Novella 
  deriving (Show)

class ArbitraryNovellaValidity (a :: Validity) where
    arbitraryNovellaValidity :: QC.Gen (ANovella a)

instance (ArbitraryNovellaValidity a) => QC.Arbitrary (ANovella a) where
    arbitrary = sqrtSize arbitraryNovellaValidity


instance ArbitraryNovellaValidity AllEnabled where
    arbitraryNovellaValidity = ANovella <$> arbitraryNovella AllEnabled

instance ArbitraryNovellaValidity AllDisabled where
    arbitraryNovellaValidity = ANovella <$> arbitraryNovella AllDisabled

instance ArbitraryNovellaValidity Mixed where
    arbitraryNovellaValidity = ANovella <$> arbitraryNovella Mixed

-- |Resize QuickCheck's internal size to sqrt of itself.
sqrtSize :: QC.Gen a -> QC.Gen a
sqrtSize f = QC.sized $ \(n::Int) -> QC.resize (round . sqrt . fromIntegral $ n) f
                          
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb p q = mb >>= (\b -> if b then p else q)

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
        it "is an identify for all-good trees" $ property $
          \(xs :: [ANovella AllEnabled]) ->
            let xxs = coerce xs
             in N.prune xxs == xxs
        it "is a cleaner for all-bad trees" $ property $
          \(xs :: [ANovella AllDisabled]) ->
            let xxs = coerce xs
             in null . N.prune $ xxs
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

