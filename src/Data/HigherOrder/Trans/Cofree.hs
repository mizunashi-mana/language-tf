{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Data.HigherOrder.Trans.Cofree where

import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           GHC.Generics


data HCofreeT f a r i where
  HCofreeT :: a i -> f r i -> HCofreeT f a r i
  deriving
    ( Eq
    , Ord
    , Show
    , Generic
    )

instance HFunctor f => CFunctor (:~>) (:~>) (HCofreeT f a) where
  cfmap f = HCat $ \case
    HCofreeT x m -> HCofreeT x $ unHCat (cfmap f) m
