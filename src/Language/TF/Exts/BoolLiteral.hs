{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Language.TF.Exts.BoolLiteral where

import           Control.Applicative
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Data.Functor
import           Data.Functor.Compose
import           Data.Kind
import           Data.Text.Prettyprint.Doc
import           Language.TF.Parser
import           Language.TF.Printer
import           Language.TF.Syntax


data BoolLitF ann (r :: AstTag -> Type) i where
  BoolLitF :: Bool -> BoolLitF ann r 'LitTag

deriving instance Eq (BoolLitF ann r i)
deriving instance Ord (BoolLitF ann r i)
deriving instance Show (BoolLitF ann r i)

instance CFunctor (:~>) (:~>) (BoolLitF ann) where
  cfmap _ = HCat $ \case
    BoolLitF b -> BoolLitF b


instance CanPrintAst BoolLitF where
  astPrinter = HCat $ Const . printBoolLitF

printBoolLitF :: BoolLitF ann r i -> Doc doca
printBoolLitF (BoolLitF b) = pretty b


instance CanParseAst BoolLitF where
  astParser r = HCat $ Compose . \case
    LitTagProxy -> injectP r boolLitF
    _           -> empty


-- |
--
-- <lit> ::= <bool> (* boolean literal *)
--
-- <bool> ::= True | False
--
boolLitF :: MPTokenParsing m => m (BoolLitF ann r 'LitTag)
boolLitF = parseHint "literal"
  $   BoolLitF <$>
    (   reserved "True"  $> True
    <|> reserved "False" $> False
    )

