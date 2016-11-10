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
import           System.Novelist (convertFtoNormal)
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.QuickCheckF as NovelistFQC
import qualified System.Novelist.Novelist   as Novelist
import qualified System.Novelist.QuickCheck as NovelistQC


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
    , bench "Novelist.prune" . nf Novelist.prune $ data10
    ]
  , bgroup "System.Novelist  size =     100" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF100
    , bench "Novelist.prune" . nf Novelist.prune $ data100
    ]
  , bgroup "System.Novelist  size =   1,000" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF1000
    , bench "Novelist.prune" . nf Novelist.prune $ data1000
    ]
  , bgroup "System.Novelist  size =  10,000" [
      bench "NovelistF.prune" . nf NovelistF.prune $ dataF10000
    , bench "Novelist.prune" . nf Novelist.prune $ data10000
    ]
  {-
   -, bgroup "System.Novelist  size = 100,000" [
   -    bench "NovelistF.prune" . nf NovelistF.prune $ dataF100000
   -  , bench "Novelist.prune" . nf Novelist.prune $ data100000
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
    data10, data100, data1000, data10000, data100000 :: [Novelist.Novella]
    data10     = NF.force $ (NovelistF.cata convertFtoNormal <$>) dataF10
    data100    = NF.force $ (NovelistF.cata convertFtoNormal <$>) dataF100
    data1000   = NF.force $ (NovelistF.cata convertFtoNormal <$>) dataF1000
    data10000  = NF.force $ (NovelistF.cata convertFtoNormal <$>) dataF10000
    data100000 = NF.force $ (NovelistF.cata convertFtoNormal <$>) dataF100000
