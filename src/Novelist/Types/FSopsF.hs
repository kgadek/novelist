{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Novelist.Types.FSopsF where


-- microlens
import           Lens.Micro (Lens')

-- freer
import           Control.Monad.Freer (Member, Eff, send)

-- filepath
import           System.FilePath (normalise)

--
import           Novelist.Types.DirectoryListing (DirectoryListing)


data FSopsF s where
    ListDir :: { _dirPath :: String } -> FSopsF (Maybe DirectoryListing)

dirPath :: Lens' (FSopsF (Maybe DirectoryListing)) String
dirPath f ListDir{ _dirPath = x } = ListDir <$> f x


listDir :: Member FSopsF r => FilePath -> Eff r (Maybe DirectoryListing)
listDir n = send (ListDir (normalise n))

