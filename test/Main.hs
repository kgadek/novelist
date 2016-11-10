module Main where

-- hspec
import           Test.Hspec.Runner
import           Test.Hspec.Formatters
import qualified Spec

main :: IO ()
main = hspecWith config Spec.spec
  where config = defaultConfig
