{-# LANGUAGE TemplateHaskell #-}

module Novelist.Types.DirectoryListing where


-- microlens
import           Lens.Micro.TH


data DirectoryListing
  = DirectoryListing {
      _dirs :: [String]
    , _files :: [String]
    }
  deriving (Show)
makeLenses ''DirectoryListing
