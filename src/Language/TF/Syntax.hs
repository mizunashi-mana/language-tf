{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
#define USE_QUANTIFIED_CONSTRAINTS 1
#endif
#endif

#ifdef USE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

module Language.TF.Syntax where

import           Control.Category
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Data.HigherOrder.Trans.Annotation
import           Data.Kind
import           GHC.Generics
import           Prelude                           hiding ((.))


type Ast f ann = HFix (HAnnotT f ann)


data AstTag
  = ExprTag
  | VarTag
  | LitTag
  | DeclTag
  deriving
    ( Eq
    , Ord
    , Show
    , Enum
    , Bounded
    , Generic
    )

data AstTagProxy :: AstTag -> Type where
  ExprTagProxy :: AstTagProxy 'ExprTag
  VarTagProxy  :: AstTagProxy 'VarTag
  LitTagProxy  :: AstTagProxy 'LitTag
  DeclTagProxy :: AstTagProxy 'DeclTag

#ifndef USE_QUANTIFIED_CONSTRAINTS
type ConstraintAstTags (c :: Type -> Constraint) (r :: AstTag -> Type) =
  ( c (r 'ExprTag)
  , c (r 'VarTag)
  , c (r 'LitTag)
  , c (r 'DeclTag)
  )
#endif

data AstF :: Type -> (AstTag -> Type) -> AstTag -> Type where
  AstExprF :: ExprF ann r i -> AstF ann r i
  AstLitF  :: LitF  ann r i -> AstF ann r i
  AstVarF  :: VarF  ann r i -> AstF ann r i
  AstDeclF :: DeclF ann r i -> AstF ann r i

#ifdef USE_QUANTIFIED_CONSTRAINTS
deriving instance (forall i. Eq (r i)) => Eq (AstF ann r i)
deriving instance (forall i. Ord (r i)) => Ord (AstF ann r i)
deriving instance (forall i. Show (r i)) => Show (AstF ann r i)
#else
deriving instance ConstraintAstTags Eq r => Eq (AstF ann r i)
deriving instance ConstraintAstTags Ord r => Ord (AstF ann r i)
deriving instance ConstraintAstTags Show r => Show (AstF ann r i)
#endif

instance CFunctor (:~>) (:~>) (AstF ann) where
  cfmap f = HCat $ \case
    AstExprF e -> unHCat (HCat AstExprF . cfmap f) e
    AstLitF  l -> unHCat (HCat AstLitF  . cfmap f) l
    AstVarF  v -> unHCat (HCat AstVarF  . cfmap f) v
    AstDeclF d -> unHCat (HCat AstDeclF . cfmap f) d


data ExprF :: Type -> (AstTag -> Type) -> AstTag -> Type where
  LambdaF   :: r 'VarTag -> r 'ExprTag -> ExprF ann r 'ExprTag
  LetF      :: r 'DeclTag -> r 'ExprTag -> ExprF ann r 'ExprTag
  LetrecF   :: r 'DeclTag -> r 'ExprTag -> ExprF ann r 'ExprTag
  IfF       :: r 'ExprTag -> r 'ExprTag -> r 'ExprTag -> ExprF ann r 'ExprTag
  AppF      :: r 'ExprTag -> r 'ExprTag -> ExprF ann r 'ExprTag
  LitExprF  :: r 'LitTag -> ExprF ann r 'ExprTag
  VarExprF  :: r 'VarTag -> ExprF ann r 'ExprTag
  InfixAppF :: r 'ExprTag -> r 'VarTag -> r 'ExprTag -> ExprF ann r 'ExprTag

#ifdef USE_QUANTIFIED_CONSTRAINTS
deriving instance (forall i. Eq (r i)) => Eq (ExprF ann r i)
deriving instance (forall i. Ord (r i)) => Ord (ExprF ann r i)
deriving instance (forall i. Show (r i)) => Show (ExprF ann r i)
#else
deriving instance (ConstraintAstTags Eq r) => Eq (ExprF ann r i)
deriving instance (ConstraintAstTags Ord r) => Ord (ExprF ann r i)
deriving instance (ConstraintAstTags Show r) => Show (ExprF ann r i)
#endif

instance CFunctor (:~>) (:~>) (ExprF ann) where
  cfmap (HCat f) = HCat $ \case
    LambdaF v e -> LambdaF (f v) (f e)
    LetF    d e -> LetF (f d) (f e)
    LetrecF d e -> LetrecF (f d) (f e)
    IfF c e1 e2 -> IfF (f c) (f e1) (f e2)
    AppF e1 e2  -> AppF (f e1) (f e2)
    LitExprF l  -> LitExprF (f l)
    VarExprF v  -> VarExprF (f v)
    InfixAppF e1 op e2 -> InfixAppF (f e1) (f op) (f e2)


data LitF :: Type -> (AstTag -> Type) -> AstTag -> Type where
  NumLitF :: Integer -> LitF ann r 'LitTag

#ifdef USE_QUANTIFIED_CONSTRAINTS
deriving instance (forall i. Eq (r i)) => Eq (LitF ann r i)
deriving instance (forall i. Ord (r i)) => Ord (LitF ann r i)
deriving instance (forall i. Show (r i)) => Show (LitF ann r i)
#else
deriving instance (ConstraintAstTags Eq r) => Eq (LitF ann r i)
deriving instance (ConstraintAstTags Ord r) => Ord (LitF ann r i)
deriving instance (ConstraintAstTags Show r) => Show (LitF ann r i)
#endif

instance CFunctor (:~>) (:~>) (LitF ann) where
  cfmap _ = HCat $ \case
    NumLitF i -> NumLitF i


data VarF :: Type -> (AstTag -> Type) -> AstTag -> Type where
  VarF  :: String -> VarF ann r 'VarTag

#ifdef USE_QUANTIFIED_CONSTRAINTS
deriving instance (forall i. Eq (r i)) => Eq (VarF ann r i)
deriving instance (forall i. Ord (r i)) => Ord (VarF ann r i)
deriving instance (forall i. Show (r i)) => Show (VarF ann r i)
#else
deriving instance (ConstraintAstTags Eq r) => Eq (VarF ann r i)
deriving instance (ConstraintAstTags Ord r) => Ord (VarF ann r i)
deriving instance (ConstraintAstTags Show r) => Show (VarF ann r i)
#endif

instance CFunctor (:~>) (:~>) (VarF ann) where
  cfmap _ = HCat $ \case
    VarF s -> VarF s


data DeclF :: Type -> (AstTag -> Type) -> AstTag -> Type where
  DeclF  :: r 'VarTag -> r 'ExprTag -> DeclF ann r 'DeclTag

#ifdef USE_QUANTIFIED_CONSTRAINTS
deriving instance (forall i. Eq (r i)) => Eq (DeclF ann r i)
deriving instance (forall i. Ord (r i)) => Ord (DeclF ann r i)
deriving instance (forall i. Show (r i)) => Show (DeclF ann r i)
#else
deriving instance (ConstraintAstTags Eq r) => Eq (DeclF ann r i)
deriving instance (ConstraintAstTags Ord r) => Ord (DeclF ann r i)
deriving instance (ConstraintAstTags Show r) => Show (DeclF ann r i)
#endif

instance CFunctor (:~>) (:~>) (DeclF ann) where
  cfmap (HCat f) = HCat $ \case
    DeclF v e -> DeclF (f v) (f e)
