{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Language.TF.Parser where

import           Control.Applicative
import           Control.Category
import           Data.Categorical.Functor
import           Data.Categorical.HigherOrder
import           Data.Categorical.Inject.OpenUnion
import           Data.Functor.Compose
import qualified Data.HashSet                      as HashSet
import           Data.HigherOrder.Trans.Annotation
import           Data.HigherOrder.Trans.Kleene
import           Data.Monoid                       (Alt (..))
import           Language.TF.Syntax
import           Prelude                           hiding ((.))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token                 (IdentifierStyle (..),
                                                    TokenParsing)
import qualified Text.Parser.Token                 as Tok
import qualified Text.Parser.Token.Highlight       as Highlight
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta               (Delta)


type MPTokenParsing m =
  ( TokenParsing m
  , CharParsing m
  , Parsing m
  , DeltaParsing m
  )

identStyle :: MPTokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "TF style"
  , _styleStart = lower
  , _styleLetter = alphaNum
  , _styleReserved = HashSet.fromList
    [ "lambda"
    , "let", "letrec", "in"
    , "if", "then", "else"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

identifier :: MPTokenParsing m => m String
identifier = Tok.ident identStyle

reserved :: MPTokenParsing m => String -> m ()
reserved = Tok.reserve identStyle

symbols :: MPTokenParsing m => [String] -> m String
symbols = getAlt . foldMap (Alt . Tok.symbol)

stimes1EndoM :: MPTokenParsing m => m a -> (a -> m a) -> m a
stimes1EndoM m r = do
  x <- m
  option x $ r x

stimesEndoM :: MPTokenParsing m => m a -> (a -> m a) -> m a
stimesEndoM m f = go m
  where
    go mx = stimes1EndoM mx (go . f)

parseHint :: MPTokenParsing m => String -> m a -> m a
parseHint h p = p <?> h

attachDelta :: MPTokenParsing m => m (Delta -> a) -> m a
attachDelta parser = parser <*> position


data AstTagParsing m f g ann r = AstTagParsing
  { astTagParser :: AstTagProxy :~> Compose m (HKleeneT g r)
  , injectParser :: Compose m (f ann (HKleeneT g r)) :~> Compose m (HKleeneT g r)
  }

injectP :: AstTagParsing m f g ann r -> m (f ann (HKleeneT g r) i) -> m (HKleeneT g r i)
injectP r = getCompose . unHCat (injectParser r) . Compose

astTagP :: AstTagParsing m f g ann r -> AstTagProxy i -> m (HKleeneT g r i)
astTagP r = getCompose . unHCat (astTagParser r)

class CanParseAst f where
  astParser :: (MPTokenParsing m)
    => AstTagParsing m f g ann r -> AstTagProxy :~> Compose m (HKleeneT g r)

instance CanParseAst AstF where
  astParser = parserAstF

instance CanParseAst f => CanParseAst (AnnHFunctor f) where
  astParser r = astParser nr
    where
      nr = r
        { injectParser = injectParser r . cfmap (HCat AnnHFunctor)
        }

instance CanParseAst f => CanParseAst (HAnnUnion '[f]) where
  astParser r = astParser $ r
    { injectParser = injectParser r . cfmap comp0
    }

instance (CanParseAst f, CanParseAst (HAnnUnion (g ': fs))) => CanParseAst (HAnnUnion (f ': g ': fs)) where
  astParser r = composeMPlus comp0Parser weakenParser
    where
      comp0Parser = astParser $ r
        { injectParser = injectParser r . cfmap comp0
        }

      weakenParser = astParser $ r
        { injectParser = injectParser r . cfmap weaken
        }

      composeMPlus :: Alternative m => x :~> Compose m a -> x :~> Compose m a -> x :~> Compose m a
      composeMPlus (HCat f) (HCat g) = HCat $ \x -> Compose $ getCompose (f x) <|> getCompose (g x)

hoistedAstParser :: MPTokenParsing m => m (Ast AstF Delta 'ExprTag)
hoistedAstParser = programParser

-- | A parser for language TF
--
-- <program> ::= <expr>
--
programParser :: (MPTokenParsing m, CanParseAst f, HFunctor (f Delta)) => m (Ast f Delta 'ExprTag)
programParser = getCompose (unHCat parserAst ExprTagProxy) <* eof

parserAst :: (MPTokenParsing m, CanParseAst f, HFunctor (f Delta))
  => AstTagProxy :~> Compose m (Ast f Delta)
parserAst = cfmap inMorKleene . astParser parsing
  where
    parsing = AstTagParsing
      { astTagParser = liftHNomore . parserAst
      , injectParser = HCat $ Compose . fmap HMoreT . attachDelta . fmap HAnn . getCompose
      }


liftHNomore :: HFunctor (f ann) => f ann r :~> f ann (HKleeneT g r)
liftHNomore = cfmap $ HCat HNomoreT

symbolVarF :: MPTokenParsing m => String -> m (VarF ann r 'VarTag)
symbolVarF sym = VarF <$> Tok.symbol sym

varExprF :: (MPTokenParsing m)
  => AstTagParsing m f g ann r -> m (f ann (HKleeneT g r) 'VarTag) -> m (ExprF ann (HKleeneT g r) 'ExprTag)
varExprF r p = VarExprF <$> injectP r p


parserAstF :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> AstTagProxy :~> Compose m (HKleeneT g r)
parserAstF r = HCat $ Compose . \case
  ExprTagProxy -> exprF r
  VarTagProxy  -> injectP r $ AstVarF  <$> varF
  LitTagProxy  -> injectP r $ AstLitF  <$> litF
  DeclTagProxy -> injectP r $ AstDeclF <$> declF r


-- |
--
-- <expr> ::= <expr1>
--          | "lambda" <var> "."" <expr>              (* lambda abstraction *)
--          | "let" <decl> "in" <expr>                (* local binding *)
--          | "letrec" <decl> "in" <expr>             (* recursive local binding *)
--          | "if" <expr> "then" <expr> "else" <expr> (* conditional expression *)
--
exprF :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
exprF r = expr1F r <|> injectP r expr'
  where
    expr' = fmap AstExprF
      $   LambdaF <$> (reserved "lambda" *> astTagP r VarTagProxy) <*> (Tok.symbol "." *> astTagP r ExprTagProxy)
      <|> LetF <$> (reserved "let" *> astTagP r DeclTagProxy) <*> (reserved "in" *> astTagP r ExprTagProxy)
      <|> LetrecF <$> (reserved "letrec" *> astTagP r DeclTagProxy) <*> (reserved "in" *> astTagP r ExprTagProxy)
      <|> IfF <$> (reserved "if" *> astTagP r ExprTagProxy)
        <*> (reserved "then" *> astTagP r ExprTagProxy) <*> (reserved "else" *> astTagP r ExprTagProxy)

-- |
--
-- <expr1> ::= <expr2> [ <relop> <expr2> ] (* apply relation *)
--
expr1F :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
expr1F r = stimes1EndoM expr2' expr1'
  where
    expr2' = expr2F r

    expr1' le = injectP r . fmap AstExprF $ InfixAppF le <$> injectP r opV <*> expr2'

    opV = AstVarF <$> relopF

-- |
--
-- <expr2> ::= <expr3> { <addop> <expr3> } (* apply weak associative operator *)
--
expr2F :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
expr2F r = stimesEndoM expr3' expr2'
  where
    expr3' = expr3F r

    expr2' le = injectP r . fmap AstExprF $ InfixAppF le <$> injectP r opV <*> expr3'

    opV = AstVarF <$> addopF

-- |
--
-- <expr3> ::= <expr4> { <mulop> <expr3> } (* apply strong associative operator *)
--
expr3F :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
expr3F r = stimesEndoM expr4' expr3'
  where
    expr4' = expr4F r

    expr3' le = injectP r . fmap AstExprF $ InfixAppF le <$> injectP r opV <*> expr4'

    opV = AstVarF <$> mulopF

-- |
--
-- <expr4> ::= <aexpr> { <aexpr> } (* apply expressions *)
--
expr4F :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
expr4F r = stimesEndoM aexpr' expr4'
  where
    aexpr' = aexprF r

    expr4' le = injectP r . fmap AstExprF $ AppF le <$> aexpr'

-- |
--
-- <aexpr> ::= <lit>
--           | <var>
--           | "(" <expr> ")"
--           | "!" <aexpr>     (* debug printing *)
--
aexprF :: (MPTokenParsing m)
  => AstTagParsing m AstF g ann r -> m (HKleeneT g r 'ExprTag)
aexprF r = Tok.parens (astTagP r ExprTagProxy) <|> injectP r aexpr'
  where
    aexpr' = fmap AstExprF
      $   VarExprF <$> astTagP r VarTagProxy
      <|> LitExprF <$> astTagP r LitTagProxy
      <|> debugTraceE

    debugTraceE = appVarF (AstVarF <$> symbolVarF "!") aexpr'

    appVarF vp ep = AppF <$> injectP r (AstExprF <$> varExprF r vp) <*> injectP r ep

-- |
--
-- <decl> ::= <var> "=" <expr> (* declaration *)
--
declF :: MPTokenParsing m => AstTagParsing m AstF g ann r -> m (DeclF ann (HKleeneT g r) 'DeclTag)
declF r = parseHint "declaration"
  $   DeclF <$> astTagP r VarTagProxy <* Tok.symbol "=" <*> astTagP r ExprTagProxy

-- |
--
-- <var> ::= <alpha> { <alpha> | <digit> } (* variable *)
--
-- <alpha> ::= "a" | "b" | ... | "z"
-- <digit> ::= "0" | "1" | ... | "9"
--
varF :: MPTokenParsing m => m (VarF ann r 'VarTag)
varF = parseHint "variable"
  $   VarF <$> identifier

-- |
--
-- <lit> ::= <number> (* number literal *)
--
-- <number> ::= <digit> { <digit> }
--
-- <digit> ::= "0" | "1" | ... | "9"
--
litF :: MPTokenParsing m => m (LitF ann r 'LitTag)
litF = parseHint "literal"
  $   NumLitF <$> Tok.natural

-- |
--
-- <relop> ::= "<"   (* less than *)
--           | "<="  (* less or equal *)
--           | ">"   (* greater than *)
--           | ">="  (* greater or equal *)
--
relopF :: MPTokenParsing m => m (VarF ann r 'VarTag)
relopF = VarF <$> symbols ["<", "<=", ">", ">="]

-- |
--
-- <addop> ::= "+" (* addition *)
--           | "-" (* subtraction *)
--
addopF :: MPTokenParsing m => m (VarF ann r 'VarTag)
addopF = VarF <$> symbols ["+", "-"]

-- |
--
-- <mulop> ::= "*" (* multiplication *)
--           | "/" (* division *)
--
mulopF :: MPTokenParsing m => m (VarF ann r 'VarTag)
mulopF = VarF <$> symbols ["*", "/"]
