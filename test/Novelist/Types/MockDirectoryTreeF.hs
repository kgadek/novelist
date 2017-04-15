{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Novelist.Types.MockDirectoryTreeF where


-- microlens
import           Lens.Micro.TH

-- microlens-ghc
import           Lens.Micro.GHC

-- mtl
import           Control.Monad.State

-- containers
import qualified Data.Map.Lazy as Map

-- :lib:
import           Novelist.Types.Fix2 (Fix2(Fix2), unFix2)



data MockDirectoryTreeF f r
  = MockDirectoryTreeF {
      _mName :: String -- drop, shall be external
    , _mDirs :: f r
    , _mFiles :: [String]
    }
  deriving (Show, Functor)

makeLenses ''MockDirectoryTreeF


type MockDirectoryTree = Fix2 MockDirectoryTreeF (Map.Map String)


rootDirName :: Lens' MockDirectoryTree String
rootDirName = unFix2 . mName


flattenMockDir :: MockDirectoryTree -> MockDirectoryTreeF (Map.Map String) String
flattenMockDir (Fix2 m) = over mDirs flatten m
  where flatten :: Map.Map String MockDirectoryTree -> Map.Map String String
        flatten mp = Map.fromList $ mp ^.. each . rootDirName . to dup
        dup :: a -> (a, a)
        dup x = (x, x)

type MockDirectoryTreeBuilder = State ([String], [MockDirectoryTree]) ()

mkDir :: String -> MockDirectoryTreeBuilder -> MockDirectoryTreeBuilder
mkDir dirName dirContentMaker = modify addDir
  where addDir (files, dirs) = (files, newDir : dirs)
        newDir = buildMockDir dirName dirContentMaker


mkFile :: String -> MockDirectoryTreeBuilder
mkFile fileName = modify addFile
  where addFile (files, dirs) = (fileName : files, dirs)


buildMockDir :: String -> MockDirectoryTreeBuilder -> MockDirectoryTree
buildMockDir dirName dirContentMaker = Fix2 MockDirectoryTreeF { _mName=dirName
                                                               , _mDirs=innerDirs'
                                                               , _mFiles=reverse innerFiles
                                                               }
  where innerDirs :: [MockDirectoryTree]
        (innerFiles, innerDirs) = execState dirContentMaker ([], [])
        innerDirs' :: Map.Map String MockDirectoryTree
        innerDirs' = Map.fromList . map (\x -> (x ^. rootDirName, x)) $ innerDirs

