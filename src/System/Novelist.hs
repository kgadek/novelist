{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module System.Novelist where

-- base
import           Data.List (isSuffixOf)
import           GHC.Generics (Generic)
import           Prelude hiding ((.))
import           Control.Category ((.))

-- deepseq
import           Control.DeepSeq (NFData)

-- fclabels
import           Data.Label
import qualified Data.Label.Partial as Partial


data NovellaF a 
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: [a]
        }
  deriving (Show, Eq, Functor, Generic, NFData)
mkLabel ''NovellaF

newtype Fix f
  = Fix { _unFix :: f (Fix f) }
  deriving (Generic)
mkLabel ''Fix

deriving instance Eq (Fix NovellaF)
deriving instance Show (Fix NovellaF)
deriving instance NFData (Fix NovellaF)

type Novella = Fix NovellaF

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Novella -> Bool
isEnabled = isNameEnabled . get (name . unFix)

prune :: [Novella] -> [Novella]
prune = fmap (Partial.modify' (contents . unFix) prune) . filter isEnabled

