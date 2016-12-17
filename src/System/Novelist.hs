module System.Novelist (
    novellasF
) where

-- recursion-schemes
import           Data.Functor.Foldable

-- fclabels
import           Data.Label
import           Data.Label.Total

-- novelist
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.Novelist    as Novelist


novellasF :: Iso Total NovelistF.Novella Novelist.Novella
novellasF = Iso (cata fromF) (ana toF)
  where
    fromF :: NovelistF.NovellaF [] Novelist.Novella -> Novelist.Novella
    fromF (NovelistF.File n)  = Novelist.File n
    fromF (NovelistF.Dir n c) = Novelist.Dir n c
    toF :: Novelist.Novella -> NovelistF.NovellaF [] Novelist.Novella
    toF (Novelist.File n)  = NovelistF.File n
    toF (Novelist.Dir n c) = NovelistF.Dir n c
