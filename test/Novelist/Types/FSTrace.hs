{-# LANGUAGE TemplateHaskell #-}

module Novelist.Types.FSTrace where

-- microlens
import           Lens.Micro.TH

-- (lib)
import           Novelist.Types (
    DirectoryListing
  )


data FSTrace
  = GetDirLst { _dirLstQuery :: String
              , _dirLstAns :: Maybe DirectoryListing
              }
  deriving (Show, Eq)
makeLenses ''FSTrace

