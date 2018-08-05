{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Categorical.Functor where

import           Control.Category
import           Data.Categorical.Category.Dual
import           Data.Coerce
import           Data.Functor
import           Data.Functor.Compose
import           Prelude                        hiding ((.))


class (Category c, Category d) => CFunctor c d f | f d -> c, f c -> d where
  cfmap :: c a b -> d (f a) (f b)

instance CFunctor c d f => CFunctor (Dual c) (Dual d) f where
  cfmap (Dual f) = Dual $ cfmap f


type EndoFunctor c = CFunctor c c


newtype HaskEndoFunctor f a = HaskEndoFunctor
  { unHaskEndoFunctor :: f a
  }

instance Functor f => CFunctor (->) (->) (HaskEndoFunctor f) where
  cfmap f = HaskEndoFunctor . fmap f . coerce


instance (EndoFunctor (->) f, EndoFunctor (->) g) => CFunctor (->) (->) (Compose f g) where
  cfmap f = Compose . cfmap (cfmap f) . coerce
