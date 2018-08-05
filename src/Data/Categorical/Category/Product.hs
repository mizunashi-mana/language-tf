{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Categorical.Category.Product where

import           Control.Category
import           Data.Bifunctor
import           Data.Categorical.Functor
import           Data.Kind
import           Data.Type.Equality
import           Prelude                  hiding (id, (.))
import           Unsafe.Coerce


type family UncurryF (f :: t1 -> t2 -> r) (k :: (t1, t2)) = (fk :: r) | fk -> f k where
  UncurryF p '(a, b) = p a b

type family Fst (k :: (a, b)) :: a where
  Fst '(a, b) = a

type family Snd (k :: (a, b)) :: b where
  Snd '(a, b) = b


extractedTuple :: forall t. '(Fst t, Snd t) :~: t
extractedTuple = unsafeCoerce Refl

reifyExtractedTuple :: forall t r. ('(Fst t, Snd t) ~ t => r) -> r
reifyExtractedTuple x = case extractedTuple @t of
  Refl -> x


data ProductCat :: (k1 -> k1 -> Type) -> (k2 -> k2 -> Type) -> (k1, k2) -> (k1, k2) -> Type where
  ProductCat :: c1 (Fst a) (Fst b) -> c2 (Snd a) (Snd b) -> ProductCat c1 c2 a b

instance (Category c1, Category c2) => Category (ProductCat c1 c2) where
  id = ProductCat id id

  ProductCat f1 f2 . ProductCat g1 g2 = ProductCat (f1 . g1) (f2 . g2)


type CBifunctor c1 c2 = CFunctor (ProductCat c1 c2)


cbimap :: CBifunctor c1 c2 d f => c1 a1 b1 -> c2 a2 b2 -> d (f '(a1, a2)) (f '(b1, b2))
cbimap f g = cfmap (ProductCat f g)

newtype BiHaskFunctor :: (k1 -> k2 -> Type) -> (k1, k2) -> Type where
  BiHaskFunctor :: p (Fst a) (Snd a) -> BiHaskFunctor p a

unBiHaskFunctor :: BiHaskFunctor p a -> p (Fst a) (Snd a)
unBiHaskFunctor (BiHaskFunctor m) = m

instance Bifunctor p => CFunctor (ProductCat (->) (->)) (->) (BiHaskFunctor p) where
  cfmap (ProductCat f g) (BiHaskFunctor m) = BiHaskFunctor (bimap f g m)
