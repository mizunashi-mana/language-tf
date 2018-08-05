{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Categorical.Product where

import           Control.Arrow                     ((&&&))
import           Control.Category
import           Data.Categorical.Category.Product
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Prelude                           hiding ((.))


class (Category c, CBifunctor c c c p) => CProduct c p | p -> c where
  projectFirst :: c (p '(a, x)) a
  projectSecond :: c (p '(x, a)) a

  (***) :: c x a -> c x b -> c x (p '(a, b))

infixl 5 ***


productMap :: CProduct c p
  => c a1 b1 -> c a2 b2 -> c (p '(a1, a2)) (p '(b1, b2))
productMap f g = f . projectFirst *** g . projectSecond

productMapUnextracted :: forall c p a b. CProduct c p
  => c (Fst a) (Fst b) -> c (Snd a) (Snd b) -> c (p a) (p b)
productMapUnextracted f g = reifyExtractedTuple @a $ reifyExtractedTuple @b
  $ productMap f g


instance CProduct (->) (BiHaskFunctor (,)) where
  projectFirst = fst . unBiHaskFunctor
  projectSecond = snd . unBiHaskFunctor

  f *** g = BiHaskFunctor . (f &&& g)


data HProduct f i where
  HPair :: Fst f i -> Snd f i -> HProduct f i

hfst :: HProduct f i -> Fst f i
hfst (HPair f _) = f

hsnd :: HProduct f i -> Snd f i
hsnd (HPair _ f) = f

instance CFunctor (ProductCat (:~>) (:~>)) (:~>) HProduct where
  cfmap (ProductCat f g) = productMapUnextracted f g

instance CProduct (:~>) HProduct where
  projectFirst = HCat hfst

  projectSecond = HCat hsnd

  HCat f *** HCat g = HCat $ \m -> HPair (f m) (g m)
