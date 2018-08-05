{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Data.Categorical.Profunctor where

import           Control.Category
import           Prelude          ()


class (Category c1, Category c2) => CProfunctor c1 c2 p | p c1 -> c2, p c2 -> c1 where
  cdimap :: c1 a1 b1 -> c2 a2 b2 -> p b1 a2 -> p a1 b2


newtype Prof c a b = Prof
  { unProf :: c a b
  } deriving (Category)

instance Category c => CProfunctor c c (Prof c) where
  cdimap f g (Prof h) = Prof (g . h . f)
