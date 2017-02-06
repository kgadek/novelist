module Novelist.FSopsSpec where


-- base
import           Text.Printf (printf)

-- containers
import qualified Data.Map.Lazy as Map

-- Hspec
import           Test.Hspec (Spec, describe, it, shouldBe, Expectation)

-- microlens-ghc
import           Lens.Micro.GHC

-- filepath
import           System.FilePath (splitPath, pathSeparator)

-- free
import qualified Control.Monad.Trans.Free as TF

-- recursion-schemes
import           Data.Functor.Foldable

-- (lib)
import           Novelist.Types (
    Fix2(Fix2), unFix2
  , FSopsF(ListDir), FSops, dirPath, continueWithDirectoryListing, listDir
  , DirectoryListing(DirectoryListing), _dirs, _files
  )

-- (test)
import           Novelist.Types.MockDirectoryTreeF (
    MockDirectoryTree, MockDirectoryTreeF(MockDirectoryTreeF), mName, mDirs, mFiles, rootDirName
  , mkDir, mkFile, buildMockDir
  , flattenMockDir
  )
import           Novelist.Types.FSTrace (
    FSTrace(GetDirLst), dirLstQuery, dirLstAns, _dirLstQuery, _dirLstAns
  )


infix 1 `shouldMatchTrace`
shouldMatchTrace :: (Show a, Eq a) => a -> a -> Expectation
shouldMatchTrace = flip shouldBe

spec :: Spec
spec = do
    describe "FSops computation" $ do
      it "non-effectful computations are non-effectful" $ do
        shouldMatchTrace []
            . getTracesWithMock mockTree1 $ do
          return ()
      describe "listDir" $ do
        describe "queries" $ do
          it "allows asking for root path: ." $ do
            shouldMatchTrace [Just "."]
                . map (^? dirLstQuery)
                . getTracesWithMock mockTree1 $ do
              listDir "."
          it "allows asking for subpath: d1" $ do
            shouldMatchTrace [Just "d1"]
                . map (^? dirLstQuery)
                . getTracesWithMock mockTree1 $ do
              listDir "d1"
          it "allows asking for subpath: d1/d2" $ do
            shouldMatchTrace [Just "d1/d2"]
                . map (^? dirLstQuery)
                . getTracesWithMock mockTree1 $ do
              listDir "d1/d2"
          it "allows asking for subpath with leading no-ops: ././d1" $ do
            shouldMatchTrace [Just "d1"]
                . map (^? dirLstQuery)
                . getTracesWithMock mockTree1 $ do
              listDir "././d1"
          it "allows asking for non-existent subpaths: e1, e1/e2" $ do
            shouldMatchTrace [Just "e1", Just "e1/e2"]
                . map (^? dirLstQuery)
                . getTracesWithMock mockTree1 $ do
              listDir "e1"
              listDir "e1/e2"
        describe "responses" $ do
          it "responds with valid ans for root path: ." $ do
            shouldMatchTrace [Just $ Just DirectoryListing { _dirs = ["d1", "d2"]
                                                   , _files = ["f1", "f2"]
                                                   }
                     ]
                . map (^? dirLstAns)
                . getTracesWithMock mockTree1 $ do
              listDir "."
          it "responds with valid ans for subpaths: d1, d2/d3" $ do
            shouldMatchTrace [ Just $ Just DirectoryListing { _dirs = []
                                                    , _files = ["f1", "f2"]
                                                    }
                     , Just $ Just DirectoryListing { _dirs = []
                                                    , _files = ["f1"]
                                                    }
                     ]
                . map (^? dirLstAns)
                . getTracesWithMock mockTree1 $ do
              listDir "d1"
              listDir "d2/d3"


mockTree1 :: MockDirectoryTree
mockTree1 =
  buildMockDir "." $ do
    mkFile "f1"
    mkFile "f2"
    mkDir "d1" $ do
      mkFile "f1"
      mkFile "f2"
    mkDir "d2" $ do
      mkFile "f1"
      mkFile "f3"
      mkDir "d3" $ do
        mkFile "f1"


directoryListing :: MockDirectoryTree -> DirectoryListing
directoryListing theMock = DirectoryListing { _dirs = flattened ^. mDirs . to Map.keys
                                            , _files = flattened ^. mFiles
                                            }
    where flattened = flattenMockDir theMock

queryMock :: String -> MockDirectoryTree -> Maybe DirectoryListing
queryMock query mock | query == "." = Just . directoryListing $ mock
                     | otherwise    = directoryListing <$> deepQueryTree
  where pthComponents :: [FilePath]
        pthComponents = dropTailSeparator <$> splitPath query

        dropTailSeparator = takeWhile (not . (== pathSeparator))
  
        deepQueryTree :: Maybe MockDirectoryTree
        deepQueryTree = foldl shallowQuery (Just mock) pthComponents

        shallowQuery :: Maybe MockDirectoryTree -> FilePath -> Maybe MockDirectoryTree
        shallowQuery Nothing _ = Nothing
        shallowQuery (Just m) pth = m ^? unFix2 . mDirs . ix pth

getTracesWithMock :: MockDirectoryTree -> FSops r -> [FSTrace]
getTracesWithMock mockTree = cata eval
  where
    eval :: TF.FreeF FSopsF r [FSTrace] -> [FSTrace]
    eval (TF.Free x@ListDir{}) = lstTrace : rest
      where
        rest = (x ^. continueWithDirectoryListing) queryResult
        lstTrace = GetDirLst { _dirLstQuery = x ^. dirPath
                             , _dirLstAns = queryResult
                             }
        queryResult = queryMock (x ^. dirPath) mockTree

    eval (TF.Pure _) = []


