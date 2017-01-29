module Novelist.FSops where


-- base
import           Text.Printf (printf)

-- free
import qualified Control.Monad.Trans.Free as TF

-- recursion-schemes
import           Data.Functor.Foldable

-- microlens
import           Lens.Micro

--
import           Novelist.Types (
    Fix2(Fix2), unFix2
  , MockDirectoryTree, MockDirectoryTreeF(MockDirectoryTreeF), mName, mDirs, mFiles
  , FSopsF(ListDir), FSops, dirName, continueWithDirectoryListing, listDir
  , DirectoryListing(DirectoryListing)
  )


mTree1 :: MockDirectoryTree
mTree1 = rootDir
  where
    rootDir = Fix2 $ MockDirectoryTreeF "dir"  [subDir1] ["file1", "file2"]
    subDir1 = Fix2 $ MockDirectoryTreeF "dirA" []        ["fileA1", "fileA2"]

test :: FSops DirectoryListing
test = do
  x <- listDir "d1"
  _ <- listDir "d2"
  _ <- listDir "d3"
  return x

test2 :: FSops ()
test2 = return ()

rootDirName :: Lens' MockDirectoryTree String
rootDirName = unFix2 . mName

flattenMockDir :: MockDirectoryTree -> MockDirectoryTreeF [] String
flattenMockDir (Fix2 m) = over mDirs (^.. each . rootDirName) m

pp2 :: MockDirectoryTree -> FSops r -> [String]
pp2 mockTree = cata eval
  where
    eval :: TF.FreeF FSopsF r [String] -> [String]
    eval (TF.Free x@ListDir{}) = msg : cont
      where
        msg            = printf "LISTDIR: %s\n RESULT: %s\n" (x ^. dirName) (show dirListing)
        dirListing     = DirectoryListing immediateDirs immediateFiles
        immediateDirs  = mockTree ^.. unFix2 . mDirs . each . rootDirName
        immediateFiles = mockTree ^. unFix2 . mFiles
        cont           = (x ^. continueWithDirectoryListing) dirListing

    eval (TF.Pure _) = []


