{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Language.TF.Printer where

import           Control.Category
import           Data.Categorical.Algebra
import           Data.Categorical.HigherOrder
import           Data.Categorical.Inject.OpenUnion
import           Data.Functor.Const
import           Data.HigherOrder.Trans.Annotation
import           Data.Text.Prettyprint.Doc
import           Language.TF.Syntax
import           Prelude                           hiding ((.))


class CanPrintAst f where
  astPrinter :: Algebra (:~>) (f ann) (Const (Doc doca))

instance CanPrintAst AstF where
  astPrinter = HCat $ Const . printAstF

deriving instance CanPrintAst f => CanPrintAst (AnnHFunctor f)

-- avoid ambiguous
instance CanPrintAst (HAnnUnion '[]) where
  astPrinter = ouEmptyAlgebra

instance CanPrintAst f => CanPrintAst (HAnnUnion '[f]) where
  astPrinter = comp0Algebra astPrinter

instance (CanPrintAst f, CanPrintAst (HAnnUnion (g ': fs))) => CanPrintAst (HAnnUnion (f ': g ': fs)) where
  astPrinter = compAlgebra astPrinter astPrinter


printAst :: (CanPrintAst f, HFunctor (f ann)) => Ast f ann i -> Doc doca
printAst = getConst . unHCat (cata $ astPrinter . HCat unHAnn)


printAstF :: AstF ann (Const (Doc doca)) i -> Doc doca
printAstF (AstExprF m) = printExprF m
printAstF (AstVarF  m) = printVarF  m
printAstF (AstLitF  m) = printLitF  m
printAstF (AstDeclF m) = printDeclF m

printExprF :: ExprF ann (Const (Doc doca)) i -> Doc doca
printExprF (LambdaF v e) = "(lambda" <+> getConst v <> "." <+> getConst e <> ")"
printExprF (LetF d e)    = "(let" <+> getConst d <+> "in" <+> getConst e <> ")"
printExprF (LetrecF d e) = "(letrec" <+> getConst d <+> "in" <+> getConst e <> ")"
printExprF (IfF c e1 e2) = "(if" <+> getConst c <+> "then" <+> getConst e1 <+> "else" <+> getConst e2 <> ")"
printExprF (AppF e1 e2)  = "(" <> getConst e1 <+> getConst e2 <> ")"
printExprF (LitExprF l)  = getConst l
printExprF (VarExprF v)  = getConst v
printExprF (InfixAppF e1 op e2) = "(" <> getConst e1 <+> getConst op <+> getConst e2 <> ")"

printVarF :: VarF ann (Const (Doc doca)) i -> Doc doca
printVarF (VarF s)  = pretty s

printLitF :: LitF ann (Const (Doc doca)) i -> Doc doca
printLitF (NumLitF i) = pretty i

printDeclF :: DeclF ann (Const (Doc doca)) i -> Doc doca
printDeclF (DeclF v e) = getConst v <+> "=" <+> getConst e
