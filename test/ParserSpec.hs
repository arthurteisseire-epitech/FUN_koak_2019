module ParserSpec where

import Test.Hspec
import KoakAST
import Parser
import Data.Either

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parse number" $ do
        it "test parse int" $
            applyParser parseLiteral "2" `shouldBe` (Right . KDecimalConst) 2
        it "test parse double" $
            applyParser parseLiteral "2.0" `shouldBe` (Right . KDoubleConst) 2
        it "test parse number error" $
            applyParser parseLiteral "error" `shouldSatisfy` isLeft

    describe "parse identifier" $ do
        it "test one char" $
            applyParser parseIdentifier "a" `shouldBe` Right "a"
        it "test only alpha chars" $
            applyParser parseIdentifier "identifier" `shouldBe` Right "identifier"
        it "test numbers in identifier" $
            applyParser parseIdentifier "id3entifier2" `shouldBe` Right "id3entifier2"
        it "test number first" $
            applyParser parseIdentifier "2id" `shouldSatisfy` isLeft

    describe "parse primary" $ do
        it "test identifier" $
            applyParser parsePrimary "id" `shouldBe` (Right . KIdentifier) "id"
        it "test literal" $
            applyParser parsePrimary "2" `shouldBe` (Right . KLiteral . KDecimalConst) 2

    describe "parse postfix" $
        it "test primary without call_expr" $
            applyParser parsePostfix "toto" `shouldBe` (Right . KPrimary . KIdentifier) "toto"

    describe "parse unary" $
        it "test unary" $
            applyParser parseUnary "!toto" `shouldBe` Right (KUnary "!" ((KPostfix . KPrimary . KIdentifier) "toto"))

    describe "parse expression" $ do
        it "test expression" $
            applyParser parseExpression "2" `shouldBe` Right (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) Nothing)
        it "test expression" $
            applyParser parseExpression "2+3"
            `shouldBe`
            Right 
                (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                (Just ("+", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing)))
