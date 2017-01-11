module Novelist.NovelistF where

-- base
import           Data.List (isSuffixOf)

-- microlens
import           Lens.Micro
import           Lens.Micro.Extras

-- recursion-schemes
import           Data.Functor.Foldable

--
import           Novelist.Types (
    Fix2(Fix2), unFix2
  , Novella, NovellaF(), name, contents
  )


isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Fix2 NovellaF f -> Bool
isEnabled = view (unFix2 . name . to isNameEnabled)

prune :: [Novella] -> [Novella]
prune = fmap (cata alg) . filter isEnabled
  where
    alg :: NovellaF [] Novella -> Novella
    alg = Fix2 . over contents (filter isEnabled)

