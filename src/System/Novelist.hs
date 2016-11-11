module System.Novelist (
    module Novelist
  , convertFtoNormal
) where

-- novelist
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.Novelist    as Novelist


convertFtoNormal :: NovelistF.NovellaF [] Novelist.Novella -> Novelist.Novella
convertFtoNormal (NovelistF.File n) = Novelist.File n
convertFtoNormal (NovelistF.Dir n c) = Novelist.Dir n c

