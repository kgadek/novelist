{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Novelist.Types.NovellaF where

-- base
import           GHC.Generics (Generic)
import           Data.Functor.Classes

-- deepseq
import           Control.DeepSeq (NFData)

-- microlens
import           Lens.Micro.TH

--
import           Novelist.Types.Fix2 (Fix2(Fix2))


data NovellaF g a 
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: g a
        }
  deriving (Show, Eq, Functor, Generic, NFData)
makeLenses ''NovellaF


file :: String -> Novella
file = Fix2 . File

dir :: String -> [Novella] -> Novella
dir n c = Fix2 $ Dir n c


instance Show1 f => Show1 (NovellaF f) where
  liftShowsPrec _  _  d (File n)  = showsUnaryWith showsPrec "File" d n
  liftShowsPrec sp sl d (Dir n c) = showsBinaryWith showsPrec (liftShowsPrec sp sl) "Dir" d n c

instance Eq1 f => Eq1 (NovellaF f) where
  liftEq _ (File x) (File y) = x == y
  liftEq eq (Dir x m) (Dir y n) = x == y && liftEq eq m n
  liftEq _ _ _ = False

type Novella = Fix2 NovellaF []
