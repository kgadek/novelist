{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module System.Novelist.Novelist (
    Novella(..), file, dir, name, contents
  , isNameEnabled, isEnabled
  , prune
) where

-- base
import           Data.List (isSuffixOf)
import           GHC.Generics (Generic)

-- deepseq
import           Control.DeepSeq (NFData)

-- microlens
import           Lens.Micro
import           Lens.Micro.Extras

-- microlens-th
import           Lens.Micro.TH


data Novella
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: [Novella]
        }
  deriving (Show, Eq, Generic, NFData)
makeLenses ''Novella


file :: String -> Novella
file = File

dir :: String -> [Novella] -> Novella
dir = Dir 

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Novella -> Bool
isEnabled = isNameEnabled . view name

prune :: [Novella] -> [Novella]
prune = fmap (over contents prune) . filter isEnabled

