{-# LANGUAGE DataKinds #-}

module SpecTest.Language.TF.Exts.BoolLiteralSpec where

import           Test.Hspec

import           Data.HigherOrder.Trans.Annotation
import           Language.TF.Exts.BoolLiteral
import           Language.TF.Parser
import           Language.TF.Printer
import           Language.TF.Syntax
import           Text.Trifecta
import           Text.Trifecta.Delta

parseTFProgramString :: String -> Result (Ast (HAnnUnion '[BoolLitF, AstF]) Delta 'ExprTag)
parseTFProgramString = parseString programParser mempty

parseTestTFString :: String -> Maybe String
parseTestTFString s = case parseTFProgramString s of
  Success d -> Just . show $ printAst d
  Failure _ -> Nothing


spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses valid programs" $ do
      parseTestTFString "let i = 20 in i + 1" `shouldBe` Just "(let i = 20 in (i + 1))"
      parseTestTFString "1 + 2 * 3 + 4" `shouldBe` Just "((1 + (2 * 3)) + 4)"
      parseTestTFString "1+2*3+4" `shouldBe` Just "((1 + (2 * 3)) + 4)"
      parseTestTFString "let b = True in (lambda v. False) b" `shouldBe` Just "(let b = True in ((lambda v. False) b))"

    it "fails by invalid programs" $ do
      parseTestTFString "" `shouldBe` Nothing
      parseTestTFString "let i = 0 in" `shouldBe` Nothing
      parseTestTFString "let b = TrueFalse in b" `shouldBe` Nothing
