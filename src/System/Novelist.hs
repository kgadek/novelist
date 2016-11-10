{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module System.Novelist where

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

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Novella -> Bool
isEnabled = isNameEnabled . get name

prune :: [Novella] -> [Novella]
prune = fmap (Partial.modify' contents prune) . filter isEnabled

