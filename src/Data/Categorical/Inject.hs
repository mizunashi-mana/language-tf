{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Categorical.Inject where

import           Control.Category
import           Data.Categorical.Category.Product
import           Data.Categorical.Coproduct
import           Data.Categorical.Profunctor
import           Prelude                           hiding (id, (.))


data Injection c a b s t where
  Injection :: c b t -> (forall e. CCoproduct c e => c s (e '(t, a))) -> Injection c a b s t

type Injection' c a s = Injection c a a s s

injection :: CCoproduct c e => c a s -> c s (e '(s, a)) -> Injection' c a s
injection f g = Injection f ((injectLeft +++ injectRight) . g)

instance Category c => CProfunctor c c (Injection c a b) where
  cdimap f g (Injection i p) = Injection (g . i) (cbimap g id . p . f)


inject :: Injection' c a s -> c a s
inject (Injection f _) = f

project :: CCoproduct c e => Injection' c a s -> c s (e '(s, a))
project (Injection _ g) = g


identityInjection :: Category c => Injection' c a a
identityInjection = Injection id injectRight
