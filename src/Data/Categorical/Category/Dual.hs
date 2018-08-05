{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}

module Data.Categorical.Category.Dual where

import           Control.Category
import           Prelude          hiding (id, (.))


newtype Dual c a b where
  Dual :: c b a -> Dual c a b

unDual :: Dual c a b -> c b a
unDual (Dual f) = f


instance Category c => Category (Dual c) where
  id = Dual id

  Dual f . Dual g = Dual $ g . f
