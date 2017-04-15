{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Novelist.FSopsSpec where


-- base
import           Control.Monad (void)

-- containers
import qualified Data.Map.Lazy as Map

-- Hspec
import           Test.Hspec (Spec, describe, it, shouldBe, Expectation)

-- microlens-ghc
import           Lens.Micro.GHC

-- filepath
import           System.FilePath (splitPath, pathSeparator)

-- freer
import           Control.Monad.Freer (Eff, Arr, run, handleRelayS)

-- (lib)
import           Novelist.Types (
    unFix2
  , FSopsF(ListDir), dirPath, listDir
  , DirectoryListing(DirectoryListing), _dirs, _files
  )

-- (test)
import           Novelist.Types.MockDirectoryTreeF (
    MockDirectoryTree, mDirs, mFiles
  , mkDir, mkFile, buildMockDir
  , flattenMockDir
  )
import           Novelist.Types.FSTrace (
    FSTrace(GetDirLst), dirLstQuery, dirLstAns, _dirLstQuery, _dirLstAns
  )


{-# ANN module ("HLint: ignore Redundant do"::String) #-}


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
              void $ listDir "e1"
              void $ listDir "e1/e2"
        describe "responses" $ do
          it "responds with valid ans for root path: ." $ do
            shouldMatchTrace [ Just $ Just DirectoryListing { _dirs = ["d1", "d2"]
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
              void $ listDir "d1"
              void $ listDir "d2/d3"
          it "responds with empty ans for subpaths: d999, d1/d999, d2/d3/d999/d999" $ do
            shouldMatchTrace [ Just Nothing
                             , Just Nothing
                             , Just Nothing
                             ]
                . map (^? dirLstAns)
                . getTracesWithMock mockTree1 $ do
              void $ listDir "d99"
              void $ listDir "d1/d999"
              void $ listDir "d2/d3/d999/d999"


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

        dropTailSeparator = takeWhile (/= pathSeparator)

        deepQueryTree :: Maybe MockDirectoryTree
        deepQueryTree = foldl shallowQuery (Just mock) pthComponents

        shallowQuery :: Maybe MockDirectoryTree -> FilePath -> Maybe MockDirectoryTree
        shallowQuery Nothing _ = Nothing
        shallowQuery (Just m) pth = m ^? unFix2 . mDirs . ix pth

getTracesWithMock :: MockDirectoryTree
                  -> Eff '[FSopsF] a
                  -> [FSTrace]
getTracesWithMock mockTree =
    reverse . run . handleRelayS [] (\s _ -> pure s) go
  where
    go :: [FSTrace]
       -> FSopsF v
       -> ([FSTrace] -> Arr r v [FSTrace])
       -> Eff r [FSTrace]
    go traces x@ListDir{} q = q (lstTrace : traces) queryResult
      where
        lstTrace = GetDirLst { _dirLstQuery = x ^. dirPath
                             , _dirLstAns = queryResult
                             }
        queryResult = queryMock (x ^. dirPath) mockTree


