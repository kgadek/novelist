module System.Novelist where

-- recursion-schemes
import           Data.Functor.Foldable

-- microlens
import           Lens.Micro

-- novelist
import qualified System.Novelist.NovelistF   as NovelistF
import qualified System.Novelist.Novelist    as Novelist


novellasFasN :: Lens' NovelistF.Novella Novelist.Novella
novellasFasN f s = novellasNtoF <$> f (novellasFtoN s)

novellasNasF :: Lens' Novelist.Novella NovelistF.Novella
novellasNasF f s = novellasFtoN <$> f (novellasNtoF s)


novellasFtoN :: NovelistF.Novella -> Novelist.Novella
novellasFtoN = cata fromF
  where
    fromF :: NovelistF.NovellaF [] Novelist.Novella -> Novelist.Novella
    fromF (NovelistF.File n)  = Novelist.File n
    fromF (NovelistF.Dir n c) = Novelist.Dir n c

novellasNtoF :: Novelist.Novella -> NovelistF.Novella
novellasNtoF = ana toF
  where
    toF :: Novelist.Novella -> NovelistF.NovellaF [] Novelist.Novella
    toF (Novelist.File n)  = NovelistF.File n
    toF (Novelist.Dir n c) = NovelistF.Dir n c

