{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.HigherOrder.Trans.Kleene where

import           Control.Category
import           Data.Categorical.Algebra
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           GHC.Generics
import           Prelude                      hiding (id, (.))


data HKleeneT f r i where
  HNomoreT :: r i -> HKleeneT f r i
  HMoreT   :: f (HKleeneT f r) i -> HKleeneT f r i
  deriving
    ( Generic
    )

deriving instance (Eq (f (HKleeneT f r) i), Eq (r i)) => Eq (HKleeneT f r i)
deriving instance (Ord (f (HKleeneT f r) i), Ord (r i)) => Ord (HKleeneT f r i)
deriving instance (Show (f (HKleeneT f r) i), Show (r i)) => Show (HKleeneT f r i)

instance HFunctor f => CFunctor (:~>) (:~>) (HKleeneT f) where
  cfmap f = HCat $ \case
    HNomoreT x -> HNomoreT $ unHCat f x
    HMoreT   m -> HMoreT $ unHCat (cfmap (cfmap f)) m


inMorKleene :: (CofixOp (:~>) t, HFunctor f) => Algebra (:~>) (HKleeneT f) (t f)
inMorKleene = inMorKleeneF id

inMorKleeneF :: (CofixOp (:~>) t, HFunctor f) => a :~> t f -> HKleeneT f a :~> t f
inMorKleeneF f = go
  where
    go = HCat $ \case
      HNomoreT x -> unHCat f x
      HMoreT   m -> unHCat (inMor . cfmap go) m

anaKleene :: (CofixOp (:~>) t, HFunctor f) => Coalgebra (:~>) (HKleeneT f) a -> a :~> t f
anaKleene f = go
  where
    go = inMorKleeneF go . f
