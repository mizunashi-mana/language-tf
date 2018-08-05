{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Data.HigherOrder.Trans.MaybeApp where

import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           GHC.Generics


data HMaybeAppT f r i where
  HNothingAppT :: r i -> HMaybeAppT f r i
  HJustAppT    :: f r i -> HMaybeAppT f r i
  deriving
    ( Eq
    , Ord
    , Show
    , Generic
    )

instance HFunctor f => CFunctor (:~>) (:~>) (HMaybeAppT f) where
  cfmap f = HCat $ \case
    HNothingAppT x -> HNothingAppT $ unHCat f x
    HJustAppT    m -> HJustAppT $ unHCat (cfmap f) m
