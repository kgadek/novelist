module Novelist.FSops where


-- free
import           Control.Monad.Free (liftF, Free)
import qualified Control.Monad.Trans.Free as TF

-- recursion-schemes
import           Data.Functor.Foldable

-- microlens
import           Lens.Micro
import           Lens.Micro.Extras

--
import           Novelist.Types (
    Fix2(Fix2), unFix2
  , MockDirectoryTree, MockDirectoryTreeF(MockDirectoryTreeF), mName, mDirs, mFiles
  , FSopsF(ListDir), dirName, next
  , DirectoryListing(DirectoryListing)
  )


mTree1 :: MockDirectoryTree
mTree1 = rootDir
  where
    rootDir = Fix2 $ MockDirectoryTreeF "dir"  [subDir1] ["file1", "file2"]
    subDir1 = Fix2 $ MockDirectoryTreeF "dirA" []        ["fileA1", "fileA2"]


listDir :: String -> Free FSopsF DirectoryListing
listDir n = liftF $ ListDir n id

test :: Free FSopsF DirectoryListing
test = do
  x <- listDir "d1"
  _ <- listDir "d2"
  _ <- listDir "d3"
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


