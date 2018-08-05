{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Categorical.HigherOrder where

import           Control.Category
import           Data.Categorical.Algebra
import           Data.Categorical.Functor
import           Data.Categorical.Monad
import           Data.Functor.Compose
import           GHC.Generics
import           Prelude                  hiding (id, (.))


type NaturalTrans c f g = forall a. c (f a) (g a)

newtype HCat c f g where
  HCat :: NaturalTrans c f g -> HCat c f g

unHCat :: HCat c f g -> c (f a) (g a)
unHCat (HCat f) = f


instance Category c => Category (HCat c) where
  id = HCat id

  HCat f . HCat g = HCat (f . g)


type f ~> g = NaturalTrans (->) f g

type (:~>) = HCat (->)


type HFunctor = EndoFunctor (:~>)

type HMonad = CMonad (:~>)


newtype HFix f i = HIn
  { hout :: f (HFix f) i
  }
  deriving
    ( Generic
    )

instance FixOp (:~>) HFix where
  outMor = HCat hout

instance CofixOp (:~>) HFix where
  inMor = HCat HIn

deriving instance Eq (f (HFix f) i) => Eq (HFix f i)
deriving instance Ord (f (HFix f) i) => Ord (HFix f i)
deriving instance Show (f (HFix f) i) => Show (HFix f i)


newtype HCompose f g r i = HCompose
  { getHCompose :: f (g r) i
  }

instance (HFunctor f, HFunctor g) => CFunctor (:~>) (:~>) (HCompose f g) where
  cfmap f = HCat HCompose . cfmap (cfmap f) . HCat getHCompose


instance (Functor f) => CFunctor (:~>) (:~>) (Compose f) where
  cfmap f = HCat $ Compose . fmap (unHCat f) . getCompose
