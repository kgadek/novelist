{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Novelist.Internal.Fix2 (
    Fix2(Fix2)
  , unFix2
) where

-- base
import           GHC.Generics (Generic)
import           Prelude hiding ((.))
import           Control.Category ((.))
import           Data.Functor.Classes

-- deepseq
import           Control.DeepSeq (NFData)

-- recursion-schemes
import           Data.Functor.Foldable

-- fclabels
import           Data.Label


newtype Fix2 (f :: (* -> *) -> * -> *) (g :: (* -> *)) 
  = Fix2 { _unFix2 :: f g (Fix2 f g) }
  deriving (Generic)
mkLabel ''Fix2

instance Show1 (f g) => Show (Fix2 f g) where
  showsPrec d (Fix2 a) = showParen (d >= 11) $ showString "Fix2 " . showsPrec1 11 a

instance Eq1 (f g) => Eq (Fix2 f g) where
  (Fix2 a) == (Fix2 b) = liftEq (==) a b

deriving instance (NFData (f g (Fix2 f g))) => NFData (Fix2 f g)


type instance Base (Fix2 f g) = f g
instance (Functor (f g)) => Recursive (Fix2 f g) where
  project = _unFix2
instance (Functor (f g)) => Corecursive (Fix2 f g) where
  embed = Fix2
