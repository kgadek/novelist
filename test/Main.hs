module Main where

-- base
import           System.IO (withFile, IOMode(WriteMode))

-- unix
import           System.Posix.Env (getEnvDefault)

-- filepath
import           System.FilePath.Posix ((</>))

-- directory
import           System.Directory (createDirectoryIfMissing)

-- hspec
import           Test.Hspec.Runner

-- hspec-jenkins
import           Test.Hspec.Formatters.Jenkins (xmlFormatter)

--
import qualified Spec


main :: IO ()
main = do
    resultDirRoot <- getEnvDefault "CIRCLE_TEST_REPORTS" "."
    let resultDir = resultDirRoot </> "junit"
        resultPath = resultDir </> "results.xml"
    createDirectoryIfMissing True resultDir

    withFile resultPath WriteMode $ \h -> do
        let config = defaultConfig { configFormatter = Just xmlFormatter
                                   , configOutputFile = Left h
                                   }
        hspecWith config Spec.spec
