{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Categorical.Algebra where

import           Control.Category
import           Data.Categorical.Functor
import           Prelude                  hiding ((.))


type Algebra c f a = c (f a) a

type Coalgebra c f a = c a (f a)


class Category c => FixOp c t | t -> c where
  outMor :: Coalgebra c f (t f)

  cata :: EndoFunctor c f => Algebra c f a -> c (t f) a
  cata f = go
    where
      go = f . cfmap go . outMor


class Category c => CofixOp c t | t -> c where
  inMor :: Algebra c f (t f)

  ana :: EndoFunctor c f => Coalgebra c f a -> c a (t f)
  ana f = go
    where
      go = inMor . cfmap go . f


newtype Fix f = In
  { out :: f (Fix f)
  }

instance FixOp (->) Fix where
  outMor = out

instance CofixOp (->) Fix where
  inMor = In

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)
