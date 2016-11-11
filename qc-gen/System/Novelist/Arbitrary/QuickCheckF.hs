{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Novelist.Arbitrary.QuickCheckF (
    ANovella(..)
  , Validity(..)
) where


-- base
import           Data.List (isSuffixOf)
import           Data.Function (on)
import           Control.Monad (liftM)
import           Data.Coerce (coerce)

-- fclabels
import           Data.Label

-- QuickCheck
import           Test.QuickCheck (property, (==>))
import qualified Test.QuickCheck as QC

-- novelist
import qualified System.Novelist.NovelistF as N


data Validity
  = AllEnabled
  | AllDisabled
  | Mixed


-- |Use a biased coin, with ratio N:1.
biasedCoin :: Int -> QC.Gen Bool
biasedCoin k = (<k) <$> QC.choose (0,k)

arbitraryNovellaName :: Validity -> QC.Gen String
arbitraryNovellaName AllEnabled  = QC.arbitrary `QC.suchThat` (".disabled" `isNotSuffixOf`)
arbitraryNovellaName AllDisabled = (++".disabled") <$> QC.arbitrary
arbitraryNovellaName Mixed       = ifM (biasedCoin 4)
                                     (arbitraryNovellaName AllEnabled)
                                     (arbitraryNovellaName AllDisabled)

arbitraryNovella :: Validity -> QC.Gen N.Novella
arbitraryNovella v = N.Fix2 <$> QC.sized arb
  where arb n
          | n <= 0    = N.File <$> arbitraryNovellaName v
          | otherwise = QC.scale (`div` 2) $
              ifM (biasedCoin 5)
                 (N.Dir <$> arbitraryNovellaName v <*> QC.listOf (arbitraryNovella v))
                 (arb 0)

newtype ANovella (v :: Validity)
  = ANovella { unANovella :: N.Novella }
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


isNotSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isNotSuffixOf needle haystack = not (needle `isSuffixOf` haystack)

