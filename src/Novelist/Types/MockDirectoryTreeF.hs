{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Novelist.Types.MockDirectoryTreeF where


-- microlens
import           Lens.Micro.TH

--
import           Novelist.Types.Fix2 (Fix2)



data MockDirectoryTreeF f r
  = MockDirectoryTreeF {
      _mName :: String -- drop, shall be external
    , _mDirs :: f r
    , _mFiles :: [String]
    }
  deriving (Show, Functor)

makeLenses ''MockDirectoryTreeF

type MockDirectoryTree = Fix2 MockDirectoryTreeF []

