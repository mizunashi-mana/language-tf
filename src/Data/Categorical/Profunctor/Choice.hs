{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Categorical.Profunctor.Choice where

import           Control.Category
import           Data.Categorical.Category.Product
import           Data.Categorical.Coproduct
import           Data.Categorical.Functor
import           Data.Categorical.Profunctor
import           Prelude                           hiding (id, (.))


class CProfunctor c c p => CChoice c p | p -> c where
  cleft :: CCoproduct c e => p a b -> p (e '(a, x)) (e '(b, x))
  cright :: CCoproduct c e => p a b -> p (e '(x, a)) (e '(x, b))

instance Category c => CChoice c (Prof c) where
  cleft (Prof f) = Prof . cfmap $ ProductCat f id
  cright (Prof f) = Prof . cfmap $ ProductCat id f
