{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Categorical.Inject.OpenUnion
  (
    Membership,
    Member(..),
    impossibleMembership,
    leadership,

    module Data.Categorical.Inject.OpenUnion,
  ) where

import           Control.Category
import           Data.Categorical.Algebra
import           Data.Categorical.Coproduct
import           Data.Categorical.Inject
import           Data.Extensible.Internal
import           Prelude                    hiding (id, (.))


class Category c => OpenUnion c ou | ou -> c where
  unionHoist :: (forall f. Membership fs f -> c (t f s a) b) -> c (ou t fs s a) b

  unionInjection :: Membership fs f -> Injection' c (t f s a) (ou t fs s a)

  decomp :: CCoproduct c e => c (ou t (f ': fs) s a) (e '(ou t fs s a, t f s a))
  decomp = decompMap injectRight injectLeft

  decompMap :: c (t f s a) x -> c (ou t fs s a) x -> c (ou t (f ': fs) s a) x
  decompMap f g = unionHoist $ \n ->
    leadership n (\Refl -> f) (\m -> g . unionInject m)

  decomp0 :: c (ou t '[f] s a) (t f s a)
  decomp0 = unionHoist $ \n ->
    leadership n (\Refl -> id) impossibleMembership

  comp :: forall e t fs f s a. CCoproduct c e => c (e '(ou t fs s a, t f s a)) (ou t (f ': fs) s a)
  comp = weaken +++ comp0

  comp0 :: c (t f s a) (ou t (f ': fs) s a)
  comp0 = unionInject here

unionInject :: OpenUnion c ou => Membership fs f -> c (t f s a) (ou t fs s a)
unionInject n = inject $ unionInjection n

unionHoistShip :: OpenUnion c ou => (forall f. Membership fs f -> Membership gs f) -> c (ou t fs s a) (ou t gs s a)
unionHoistShip f = unionHoist (unionInject . f)

weaken :: OpenUnion c ou => c (ou t fs s a) (ou t (f ': fs) s a)
weaken = unionHoistShip navNext

absurdU :: OpenUnion c ou => c (ou t '[] s a) b
absurdU = unionHoist impossibleMembership

ouInjection :: (Member fs f, OpenUnion c ou) => Injection' c (t f s a) (ou t fs s a)
ouInjection = unionInjection membership


ouEmptyAlgebra :: OpenUnion c ou => Algebra c (ou t '[] s) a
ouEmptyAlgebra = absurdU

comp0Algebra :: OpenUnion c ou => Algebra c (t f s) a -> Algebra c (ou t '[f] s) a
comp0Algebra f = f . decomp0

compAlgebra :: OpenUnion c ou => Algebra c (t f s) a -> Algebra c (ou t fs s) a -> Algebra c (ou t (f ': fs) s) a
compAlgebra = decompMap
