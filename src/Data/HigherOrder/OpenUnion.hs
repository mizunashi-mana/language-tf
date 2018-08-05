{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.HigherOrder.OpenUnion where

import           Control.Category
import           Data.Categorical.Coproduct
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Data.Categorical.Inject
import           Data.Categorical.Inject.OpenUnion
import           Data.Extensible.Internal
import           Data.Functor.Const
import           Prelude                           hiding ((.))


data HOpenU t fs s r i where
  HOpenU :: Membership fs f -> t f s r i -> HOpenU t fs s r i


instance OpenUnion (:~>) HOpenU where
  unionHoist f = HCat $ \case
    HOpenU n m -> unHCat (f n) m

  unionInjection n = Injection
    (HCat (HOpenU n))
    (HCat $ \case
      ou@(HOpenU n' m) -> case compareMembership n n' of
        Right Refl -> unHCat injectRight m
        Left{}     -> unHCat injectLeft ou
    )


data ContextHOpenU c t fs s r i where
  ContextHOpenU :: c (t f s r i) => Membership fs f -> t f s r i -> ContextHOpenU c t fs s r i

absurdContextU :: HOpenU t '[] s r i -> ContextHOpenU c t '[] s r i
absurdContextU = unHCat absurdU

decomp0Context :: c (t f s r i) => HOpenU t '[f] s r i -> ContextHOpenU c t '[f] s r i
decomp0Context = ContextHOpenU here . unHCat decomp0

decompContext :: c (t f s r i)
  => (HOpenU t fs s r i -> ContextHOpenU c t fs s r i)
  -> HOpenU t (f ': fs) s r i -> ContextHOpenU c t (f ': fs) s r i
decompContext f x = case unHCat decomp x of
  HRight e -> ContextHOpenU here e
  HLeft  u -> case f u of
    ContextHOpenU n e -> ContextHOpenU (navNext n) e


instance Eq (HOpenU t '[] s r i) where
  x == _ = getConst $ unHCat absurdU x

instance Eq (t f s r i) => Eq (HOpenU t '[f] s r i) where
  x == y = unHCat decomp0 x == unHCat decomp0 y

instance (Eq (t f s r i), Eq (HOpenU t (g ': fs) s r i)) => Eq (HOpenU t (f ': g ': fs) s r i) where
  x == y = unHCat decomp x == case unHCat decomp y of
    l@HLeft{} -> l
    r         -> r


instance Ord (HOpenU t '[] s r i) where
  x `compare` _ = getConst $ unHCat absurdU x

instance Ord (t f s r i) => Ord (HOpenU t '[f] s r i) where
  x `compare` y = unHCat decomp0 x `compare` unHCat decomp0 y

instance (Ord (t f s r i), Ord (HOpenU t (g ': fs) s r i)) => Ord (HOpenU t (f ': g ': fs) s r i) where
  x `compare` y = unHCat decomp x `compare` case unHCat decomp y of
    l@HLeft{} -> l
    r         -> r


class ShowHOpenU t fs s r i where
  showHOpenU :: HOpenU t fs s r i -> ContextHOpenU Show t fs s r i

instance ShowHOpenU t '[] s r i where
  showHOpenU = absurdContextU

instance Show (t f s r i) => ShowHOpenU t '[f] s r i where
  showHOpenU = decomp0Context

instance (Show (t f s r i), ShowHOpenU t (g ': fs) s r i) => ShowHOpenU t (f ': g ': fs) s r i where
  showHOpenU = decompContext showHOpenU


instance ShowHOpenU t fs s r i => Show (HOpenU t fs s r i) where
  show x = case showHOpenU x of
    ContextHOpenU n e -> "HOpenU " ++ show n ++ " " ++ show e


instance CFunctor (:~>) (:~>) (HOpenU t '[] s) where
  cfmap _ = absurdU

instance (HFunctor (t f s)) => CFunctor (:~>) (:~>) (HOpenU t '[f] s) where
  cfmap f = comp0 . cfmap f . decomp0

instance (HFunctor (t f s), HFunctor (HOpenU t (g ': fs) s)) => CFunctor (:~>) (:~>) (HOpenU t (f ': g ': fs) s) where
  cfmap f = decompMap (comp0 . cfmap f) (weaken . cfmap f)
