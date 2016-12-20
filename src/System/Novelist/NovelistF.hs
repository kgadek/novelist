{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module System.Novelist.NovelistF where
{-
 -module System.Novelist.NovelistF (
 -  -- data
 -    Novella, NovellaF(..)
 -  , Fix2(..)
 -    -- smart constructors
 -  , file, dir
 -    -- lens
 -  , name, contents
 -  , unFix2
 -  -- functions
 -  , isNameEnabled, isEnabled
 -  , prune
 -) where
 -}

-- base
import           Data.List (isSuffixOf)
import           GHC.Generics (Generic)
import           Prelude hiding ((.))
import           Control.Category ((.))
import           Data.Functor.Classes
import           Control.Arrow (Kleisli(Kleisli, runKleisli))
import           Data.Maybe (listToMaybe, mapMaybe)

-- deepseq
import           Control.DeepSeq (NFData)

-- fclabels
{-import           Data.Label-}
{-import qualified Data.Label.Base as FCBase-}
{-import qualified Data.Label.Poly as Poly-}
{-import qualified Data.Label.Partial as Partial-}
{-import           Data.Label.Partial ((:~>))-}

-- microlens
import           Lens.Micro
import           Lens.Micro.Extras

-- microlens-th
import           Lens.Micro.TH

-- recursion-schemes
import           Data.Functor.Foldable

-- free
import           Control.Monad.Free (liftF, Free(Pure,Free))
import qualified Control.Monad.Trans.Free as TF

--
import           System.Novelist.Internal.Fix2 (Fix2(Fix2), unFix2)



data NovellaF g a 
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: g a
        }
  deriving (Show, Eq, Functor, Generic, NFData)
makeLenses ''NovellaF

instance Show1 f => Show1 (NovellaF f) where
  liftShowsPrec _  _  d (File n)  = showsUnaryWith showsPrec "File" d n
  liftShowsPrec sp sl d (Dir n c) = showsBinaryWith showsPrec (liftShowsPrec sp sl) "Dir" d n c

instance Eq1 f => Eq1 (NovellaF f) where
  liftEq _ (File x) (File y) = x == y
  liftEq eq (Dir x m) (Dir y n) = x == y && liftEq eq m n
  liftEq _ _ _ = False


type Novella = Fix2 NovellaF []

file :: String -> Novella
file = Fix2 . File

dir :: String -> [Novella] -> Novella
dir n c = Fix2 $ Dir n c

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Fix2 NovellaF f -> Bool
isEnabled = isNameEnabled . view (unFix2 . name)

prune :: [Novella] -> [Novella]
prune = fmap (cata alg) . filter isEnabled
  where
    alg :: NovellaF [] Novella -> Novella
    alg (File n)  = Fix2 . File $ n
    alg (Dir n c) = Fix2 . Dir n $ filter isEnabled c


data DirectoryListing
  = DirectoryListing {
      _dirs :: [String]
    , _files :: [String]
    }
  deriving (Show)
makeLenses ''DirectoryListing

data MockDirectoryTreeF f r
  = MockDirectoryTreeF {
      _mName :: String -- drop, shall be external
    , _mDirs :: f r
    , _mFiles :: [String]
    }
  deriving (Show, Functor)
  --deriving (Show, Eq, Functor, Generic, NFData)
makeLenses ''MockDirectoryTreeF

type MockDirectoryTree = Fix2 MockDirectoryTreeF []

mTree1 :: MockDirectoryTree
mTree1 = rootDir
  where
    rootDir = Fix2 $ MockDirectoryTreeF "dir"  [subDir1] ["file1", "file2"]
    subDir1 = Fix2 $ MockDirectoryTreeF "dirA" []        ["fileA1", "fileA2"]

data FSopsF next
  = ListDir {
      _dirName :: String
    , _next :: DirectoryListing -> next
    }
  deriving (Functor)
makeLenses ''FSopsF


listDir :: String -> Free FSopsF DirectoryListing
listDir n = liftF $ ListDir n id

test :: Free FSopsF DirectoryListing
test = do
  x <- listDir "d1"
  y <- listDir "d2"
  z <- listDir "d3"
  return x

test2 :: Free FSopsF ()
test2 = return ()

rootDirName :: Lens' MockDirectoryTree String
rootDirName = unFix2 . mName

flattenMockDir :: MockDirectoryTree -> MockDirectoryTreeF [] String
flattenMockDir (Fix2 m) = over mDirs (fmap (view rootDirName)) m

pp2 :: MockDirectoryTree -> Free FSopsF r -> String
pp2 mockTree = cata eval
  where
    eval :: TF.FreeF FSopsF r String -> String
    eval (TF.Free x@ListDir{}) = msg ++ cont
      where
        msg = "LISTDIR: " ++ view dirName x ++ "\n  RESULT: " ++ show theDir ++ "\n"
        theDir = DirectoryListing (view (unFix2 . mName) <$> view (unFix2 . mDirs) mockTree)
                                  (view (unFix2 . mFiles) mockTree)
        cont = (view next x) theDir

    eval (TF.Pure _) = ""
