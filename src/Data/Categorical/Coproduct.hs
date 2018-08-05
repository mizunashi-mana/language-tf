{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Categorical.Coproduct where

import           Control.Category
import           Data.Categorical.Category.Product
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Prelude                           hiding ((.))


class (Category c, CBifunctor c c c e) => CCoproduct c e | e -> c where
  injectLeft :: c a (e '(a, x))
  injectRight :: c a (e '(x, a))

  (+++) :: c a x -> c b x -> c (e '(a, b)) x

infixl 4 +++

-- {-# RULES
-- "coproduct/left"  forall f g. (f +++ g) . injectLeft  = f
-- "coproduct/right" forall f g. (f +++ g) . injectRight = g
-- #-}

coproductMap :: CCoproduct c e
  => c a1 b1 -> c a2 b2 -> c (e '(a1, a2)) (e '(b1, b2))
coproductMap f g = injectLeft . f +++ injectRight . g

coproductMapUnextracted :: forall c e a b. CCoproduct c e
  => c (Fst a) (Fst b) -> c (Snd a) (Snd b) -> c (e a) (e b)
coproductMapUnextracted f g = reifyExtractedTuple @a $ reifyExtractedTuple @b
  $ coproductMap f g


instance CCoproduct (->) (BiHaskFunctor Either) where
  injectLeft = BiHaskFunctor . Left
  injectRight = BiHaskFunctor . Right

  f +++ g = either f g . unBiHaskFunctor


data HCoproduct f i where
  HLeft :: Fst f i -> HCoproduct f i
  HRight :: Snd f i -> HCoproduct f i

instance CFunctor (ProductCat (:~>) (:~>)) (:~>) HCoproduct where
  cfmap (ProductCat f g) = coproductMapUnextracted f g

instance CCoproduct (:~>) HCoproduct where
  injectLeft = HCat HLeft
  injectRight = HCat HRight

  HCat f +++ HCat g = HCat $ \case
    HLeft  m -> f m
    HRight m -> g m

deriving instance (Eq (Fst f i), Eq (Snd f i)) => Eq (HCoproduct f i)
deriving instance (Ord (Fst f i), Ord (Snd f i)) => Ord (HCoproduct f i)
deriving instance (Show (Fst f i), Show (Snd f i)) => Show (HCoproduct f i)
