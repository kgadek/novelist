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

-- fclabels
import           Data.Label
import qualified Data.Label.Partial as Partial


data Novella
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: [Novella]
        }
  deriving (Show, Eq, Generic, NFData)
mkLabel ''Novella


file :: String -> Novella
file = File

dir :: String -> [Novella] -> Novella
dir = Dir 

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Novella -> Bool
isEnabled = isNameEnabled . get name

prune :: [Novella] -> [Novella]
prune = fmap (Partial.modify' contents prune) . filter isEnabled

