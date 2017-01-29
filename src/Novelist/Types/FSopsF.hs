{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Novelist.Types.FSopsF where


-- microlens
import           Lens.Micro.TH

-- free
import           Control.Monad.Free (Free, liftF)

--
import           Novelist.Types.DirectoryListing (DirectoryListing)


data FSopsF next
  = ListDir {
      _dirName :: String
    , _continueWithDirectoryListing :: DirectoryListing -> next
    }
  deriving (Functor)
makeLenses ''FSopsF

type FSops a = Free FSopsF a


listDir :: String -> FSops DirectoryListing
listDir n = liftF $ ListDir n id

