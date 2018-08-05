{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.HigherOrder.Trans.Annotation where

import           Control.Category
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Data.Categorical.Inject
import           Data.Categorical.Inject.OpenUnion
import           Data.Functor.Const
import           Data.HigherOrder.OpenUnion
import           Data.HigherOrder.Trans.Cofree
import           Prelude                           hiding ((.))


type HAnnotT f a = HCofreeT (f a) (Const a)

pattern HAnn :: f a r i -> a -> HAnnotT f a r i
pattern HAnn m x = HCofreeT (Const x) m

{-# COMPLETE HAnn #-}

unHAnn :: HAnnotT f a r i -> f a r i
unHAnn (HAnn m _) = m

annotation :: HAnnotT f a r i -> a
annotation (HAnn _ x) = x


newtype AnnHFunctor f ann r i where
  AnnHFunctor :: f ann r i -> AnnHFunctor f ann r i
  deriving (Eq, Ord, Show)

unAnnHFunctor :: AnnHFunctor f ann r i -> f ann r i
unAnnHFunctor (AnnHFunctor m) = m

instance HFunctor (f ann) => CFunctor (:~>) (:~>) (AnnHFunctor f ann) where
  cfmap f = HCat AnnHFunctor . cfmap f . HCat unAnnHFunctor

type HAnnUnion = HOpenU AnnHFunctor

injAnn :: Member fs f => f ann r :~> HAnnUnion fs ann r
injAnn = inject ouInjection . HCat AnnHFunctor
