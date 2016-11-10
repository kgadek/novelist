module System.Novelist (
    prune
  , convertFtoNormal
) where

-- novelist
import           System.Novelist.NovelistF (prune)
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.QuickCheckF as NovelistFQC
import qualified System.Novelist.Novelist   as Novelist
import qualified System.Novelist.QuickCheck as NovelistQC


convertFtoNormal :: NovelistF.NovellaF Novelist.Novella -> Novelist.Novella
convertFtoNormal (NovelistF.File n) = Novelist.File n
convertFtoNormal (NovelistF.Dir n c) = Novelist.Dir n c

