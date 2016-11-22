{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module System.Novelist.NovelistF where
{-
 -module System.Novelist.NovelistF (
 -  -- data
 -    Novella, NovellaF(..)
 -  , Fix2(..)
 -    -- smart constructors
 -  , file, dir
 -    -- lens
 -  , name, contents
 -  , unFix2
 -  -- functions
 -  , isNameEnabled, isEnabled
 -  , prune
 -) where
 -}

-- base
import           Data.List (isSuffixOf)
import           GHC.Generics (Generic)
import           Prelude hiding ((.))
import           Control.Category ((.))
import           Data.Functor.Classes

-- deepseq
import           Control.DeepSeq (NFData)

-- fclabels
import           Data.Label

-- recursion-schemes
import           Data.Functor.Foldable

-- free
import           Control.Monad.Free (liftF, Free(Pure,Free))
import qualified Control.Monad.Trans.Free as TF



data NovellaF g a 
  = File { _name :: String
         }
  | Dir { _name :: String
        , _contents :: g a
        }
  deriving (Show, Eq, Functor, Generic, NFData)
mkLabel ''NovellaF

instance Show1 f => Show1 (NovellaF f) where
  liftShowsPrec _  _  d (File n)  = showsUnaryWith showsPrec "File" d n
  liftShowsPrec sp sl d (Dir n c) = showsBinaryWith showsPrec (liftShowsPrec sp sl) "Dir" d n c

instance Eq1 f => Eq1 (NovellaF f) where
  liftEq _ (File x) (File y) = x == y
  liftEq eq (Dir x m) (Dir y n) = x == y && liftEq eq m n
  liftEq _ _ _ = False



newtype Fix2 (f :: (* -> *) -> * -> *) (g :: (* -> *)) 
  = Fix2 { _unFix2 :: f g (Fix2 f g) }
  deriving (Generic)
mkLabel ''Fix2

instance Show1 (f g) => Show (Fix2 f g) where
  showsPrec d (Fix2 a) = showParen (d >= 11) $ showString "Fix2 " . showsPrec1 11 a

-- TODO: change to
--   instance (Eq1 (f g), Sortable (f g), Eq g) => Eq (Fix2 f g)
-- to make this work regardless of the exact ordering of the elements
instance Eq1 (f g) => Eq (Fix2 f g) where
  (Fix2 a) == (Fix2 b) = liftEq (==) a b

deriving instance (NFData (f g (Fix2 f g))) => NFData (Fix2 f g)


type instance Base (Fix2 f g) = f g
instance (Functor (f g)) => Recursive (Fix2 f g) where
  project (Fix2 a) = a




type Novella = Fix2 NovellaF []

file :: String -> Novella
file = Fix2 . File

dir :: String -> [Novella] -> Novella
dir n c = Fix2 $ Dir n c

isNameEnabled :: String -> Bool
isNameEnabled = not . (".disabled" `isSuffixOf`)

isEnabled :: Fix2 NovellaF f -> Bool
isEnabled = isNameEnabled . get (name . unFix2)

cataWithSentry :: (NovellaF [] Novella -> Novella) -> [Novella] -> [Novella]
cataWithSentry = cataWith (Fix2 . Dir "sentry") (_contents . _unFix2)

cataWith :: Recursive t => (pt -> t) -> (pa -> a) -> (Base t pa -> pa) -> pt -> a
cataWith pre post alg = post . cata alg . pre

prune :: [Novella] -> [Novella]
prune = cataWithSentry alg
  where
    alg :: NovellaF [] Novella -> Novella
    alg (File n)  = Fix2 . File $ n
    alg (Dir n c) = Fix2 . Dir n $ filter isEnabled c


data FSopsF next
  = ListDir { _dirName :: String
            , _next :: next
            }
  deriving (Functor)
mkLabel ''FSopsF


type instance Base (Free f r) = TF.FreeF f r

instance Functor f => Recursive (Free f r) where
  project (Free a) = TF.Free a
  project (Pure b) = TF.Pure b


listDir :: String -> Free FSopsF ()
listDir n = liftF $ ListDir n ()

test :: Free FSopsF ()
test = do
  listDir "d1"
  listDir "d2"
  listDir "d3"

test2 :: Free FSopsF ()
test2 = return ()

pp :: Free FSopsF r -> IO r
pp = cata eval
  where
    eval :: TF.FreeF FSopsF r (IO r) -> IO r
    eval (TF.Free x@ListDir{}) = do
      putStr "LISTDIR: "
      putStrLn $ get dirName x
      get next x
    eval (TF.Pure r) = return r

pp2 :: Free FSopsF r -> String
pp2 = cata eval
  where
    eval :: TF.FreeF FSopsF r String -> String
    eval (TF.Free x@ListDir{}) = "LISTDIR: " ++ get dirName x ++ "\n" ++ get next x
    eval (TF.Pure _) = ""
