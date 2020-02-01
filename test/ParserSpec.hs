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
        it "test expressions" $
            applyParser parsePrimary "(2+3:4-5:6*7)"
            `shouldBe`
            Right
                (KPrimaryExpressions $ KListExpr
                    [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                      (Just ("+", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing))
                    , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                      (Just ("-", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 5) Nothing))
                    , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 6)
                      (Just ("*", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 7) Nothing))
                    ])

    describe "parse postfix" $ do
        it "test primary identifier" $
            applyParser parsePostfix "toto" `shouldBe` (Right . KPrimary . KIdentifier) "toto"
        it "test primary decimal const" $
            applyParser parsePostfix "2" `shouldBe` (Right . KPrimary . KLiteral . KDecimalConst) 2
        it "test primary double" $
            applyParser parsePostfix "2.0" `shouldBe` (Right . KPrimary . KLiteral . KDoubleConst) 2
        it "test call expr" $
            applyParser parsePostfix "toto(1,2)"
            `shouldBe`
            Right (KFuncCall
                (KIdentifier "toto")
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) Nothing
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) Nothing
                           ]))

    describe "parse call expr" $ do
        it "test call expr with one args" $
            applyParser parseCallExpr "(1)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) Nothing ])

        it "test call expr with two args" $
            applyParser parseCallExpr "(1,2)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) Nothing
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) Nothing
                           ])

        it "test call expr with three args" $
            applyParser parseCallExpr "(1,2,3)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) Nothing
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) Nothing
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing
                           ])

    describe "parse unary" $
        it "test unary" $
            applyParser parseUnary "!toto" `shouldBe` Right (KUnOpUnary "!" ((KPostfix . KPrimary . KIdentifier) "toto"))

    describe "parse expression" $ do
        it "test expression" $
            applyParser parseExpression "2" `shouldBe` Right (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) Nothing)

        it "test nested expression" $
            applyParser parseExpression "2+3"
            `shouldBe`
            Right
                (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                (Just ("+", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing)))

        it "test double nested expression" $
            applyParser parseExpression "2-3-4"
            `shouldBe`
            Right
                (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) $
                    Just ("-", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) $
                        Just ("-", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                            Nothing)))

    describe "parse expressions" $ do
        it "test simple expression" $
            applyParser parseListExpr "2+3"
            `shouldBe`
            Right
                (KListExpr
                    [KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                    (Just ("+", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing))])

        it "test expressions separated by two points" $
            applyParser parseListExpr "2+3:4-5:6*7"
            `shouldBe`
            Right
                (KListExpr
                    [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                      (Just ("+", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) Nothing))
                    , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                      (Just ("-", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 5) Nothing))
                    , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 6)
                      (Just ("*", KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 7) Nothing))
                    ])
