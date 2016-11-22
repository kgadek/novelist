{-# LANGUAGE DataKinds #-}
module Main (
    main
) where


-- deepseq
import qualified Control.DeepSeq as NF

-- recursion-schemes
import           Data.Functor.Foldable

-- criterion
import           Criterion      (Benchmark, bench, nf)
import           Criterion.Main (bgroup, defaultMain)

-- QuickCheck
import qualified Test.QuickCheck        as QC
import qualified Test.QuickCheck.Gen    as QC
import qualified Test.QuickCheck.Random as QC

-- novelist
import qualified System.Novelist.NovelistF   as NovelistF

--
import qualified System.Novelist.Arbitrary.QuickCheckF as NovelistFQC


genPruneBench :: Int -> Int -> [NovelistF.Novella]
genPruneBench seed =
    (:[])
  . NovelistFQC.unANovella
  . QC.unGen (QC.arbitrary :: QC.Gen (NovelistFQC.ANovella NovelistFQC.Mixed))
             (QC.mkQCGen seed)

main :: IO ()
main = defaultMain [
    bgroup "System.Novelist  size = 10" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF10
    ]
  , bgroup "System.Novelist  size =     100" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF100
    ]
  , bgroup "System.Novelist  size =   1,000" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF1000
    ]
  , bgroup "System.Novelist  size =  10,000" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF10000
    ]
  {-
   -, bgroup "System.Novelist  size = 100,000" [
   -    bench "NovelistF.prune" . nf NovelistF.prune $ dataF100000
   -  ]
   -}
  ]
  where
    seed = 42  -- Totally random, Dilbert.
    dataF10, dataF100, dataF1000, dataF10000, dataF100000 :: [NovelistF.Novella]
    dataF10     = NF.force $ genPruneBench seed 10
    dataF100    = NF.force $ genPruneBench seed 100
    dataF1000   = NF.force $ genPruneBench seed 1000
    dataF10000  = NF.force $ genPruneBench seed 10000
    dataF100000 = NF.force $ genPruneBench seed 100000
