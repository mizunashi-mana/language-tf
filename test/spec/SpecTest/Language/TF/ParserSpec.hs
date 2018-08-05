{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module SpecTest.Language.TF.ParserSpec where

import           Test.Hspec

import           Data.Categorical.HigherOrder
import           Data.HigherOrder.Trans.Annotation
import           Data.Proxy
import           Language.TF.Parser
import           Language.TF.Printer
import           Language.TF.Syntax
import           Text.Trifecta
import           Text.Trifecta.Delta


parseTFProgramString :: (HFunctor (f Delta), CanParseAst f) => Proxy f -> String -> Result (Ast f Delta 'ExprTag)
parseTFProgramString _ = parseString programParser mempty

parseTestTFString :: (HFunctor (f Delta), CanParseAst f, CanPrintAst f) => Proxy f -> String -> Maybe String
parseTestTFString p s = case parseTFProgramString p s of
  Success d -> Just . show $ printAst d
  Failure _ -> Nothing

parseTestStdTFString :: String -> Maybe String
parseTestStdTFString = parseTestTFString (Proxy :: Proxy AstF)

parseTestLiftedTFString :: String -> Maybe String
parseTestLiftedTFString = parseTestTFString (Proxy :: Proxy (AnnHFunctor AstF))


spec :: Spec
spec = do
  describe "AstF Parser" $ do
    it "parses valid programs" $ do
      parseTestStdTFString "let i = 20 in i + 1" `shouldBe` Just "(let i = 20 in (i + 1))"
      parseTestStdTFString "1 + 2 * 3 + 4" `shouldBe` Just "((1 + (2 * 3)) + 4)"
      parseTestStdTFString "1+2*3+4" `shouldBe` Just "((1 + (2 * 3)) + 4)"

    it "fails by invalid programs" $ do
      parseTestStdTFString "" `shouldBe` Nothing
      parseTestStdTFString "let i = 0 in" `shouldBe` Nothing

  describe "Lifted AstF Parser" $ do
    it "parses valid programs" $ do
      parseTestLiftedTFString "let i = 0 in i + 1" `shouldBe` Just "(let i = 0 in (i + 1))"
      parseTestLiftedTFString "1 + 2 * 3 + 4" `shouldBe` Just "((1 + (2 * 3)) + 4)"
      parseTestLiftedTFString "1+2*3+4" `shouldBe` Just "((1 + (2 * 3)) + 4)"

    it "fails by invalid programs" $ do
      parseTestLiftedTFString "" `shouldBe` Nothing
      parseTestLiftedTFString "let i = 0 in" `shouldBe` Nothing
