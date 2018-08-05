{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}

module Data.Categorical.Monad where

import           Control.Category
import           Data.Categorical.Functor
import           Prelude                  hiding (id, (.))


-- |
--
-- cpure . cfmap cpure ≡ cfmap cpure . cpure
-- cpure . cfmap cjoin ≡ cpure . cjoin ≡ id
--
class EndoFunctor c f => CMonad c f | f -> c where
  cpure :: c a (f a)

  cjoin :: c (f (f a)) (f a)
  cjoin = cbind id

  cbind :: c a (f b) -> c (f a) (f b)
  cbind f = cjoin . cfmap f
