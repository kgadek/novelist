{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Novelist.Types.FSopsF where


-- microlens
import           Lens.Micro.TH

--
import           Novelist.Types.DirectoryListing (DirectoryListing)


data FSopsF next
  = ListDir {
      _dirName :: String
    , _next :: DirectoryListing -> next
    }
  deriving (Functor)
makeLenses ''FSopsF

