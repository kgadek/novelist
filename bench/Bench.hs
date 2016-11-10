{-# LANGUAGE DataKinds #-}
module Main (
    main
) where


-- deepseq
import qualified Control.DeepSeq as NF

-- criterion
import           Criterion      (Benchmark, bench, nf)
import           Criterion.Main (bgroup, defaultMain)

-- QuickCheck
import qualified Test.QuickCheck        as QC
import qualified Test.QuickCheck.Gen    as QC
import qualified Test.QuickCheck.Random as QC

-- novelist
import qualified System.Novelist            as N
import qualified System.Novelist.QuickCheck as N


genPruneBench :: Int -> Int -> [N.Novella]
genPruneBench seed =
    (:[])
  . N.unANovella
  . QC.unGen (QC.arbitrary :: QC.Gen (N.ANovella N.Mixed))
             (QC.mkQCGen seed)

main :: IO ()
main = defaultMain [
    bgroup "System.Novelist  size = 10" [
      bench "N.prune" . nf N.prune $ data10
    ]
  , bgroup "System.Novelist  size =     100" [
      bench "N.prune" . nf N.prune $ data100
    ]
  , bgroup "System.Novelist  size =   1,000" [
      bench "N.prune" . nf N.prune $ data1000
    ]
  , bgroup "System.Novelist  size =  10,000" [
      bench "N.prune" . nf N.prune $ data10000
    ]
  {-
   -, bgroup "System.Novelist  size = 100,000" [
   -    bench "N.prune" . nf N.prune $ data100000
   -  ]
   -}
  ]
  where
    seed = 42  -- Totally random, Dilbert.
    data10 :: [N.Novella]
    data10     = NF.force $ genPruneBench seed 10
    data100    = NF.force $ genPruneBench seed 100
    data1000   = NF.force $ genPruneBench seed 1000
    data10000  = NF.force $ genPruneBench seed 10000
    data100000 = NF.force $ genPruneBench seed 100000
