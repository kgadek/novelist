module Main (
    main
) where


-- criterion
import           Criterion      (Benchmark, bench, nf)
import           Criterion.Main (bgroup, defaultMain)

-- novelist
import qualified Lib as N


benchBar :: [Benchmark]
benchBar = [
      bench "bar 1" $ nf N.bar 1 
    , bench "bar 2" $ nf N.bar 2 
    ]

main :: IO ()
main = 
    defaultMain [
      bgroup "group A" [
          bench "2" $ nf   const (2::Int)
      ]
    , bgroup "group B" benchBar
    ]
