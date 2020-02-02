module ParserSpec where

import Test.Hspec
import KoakAST
import Parser
import Data.Either

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parse binary operator" $ do
        it "test plus" $
            applyParser parseBinOp "+" `shouldBe` Right KBinOpPlus
        it "test less" $
            applyParser parseBinOp "-" `shouldBe` Right KBinOpLess
        it "test mul" $
            applyParser parseBinOp "*" `shouldBe` Right KBinOpMul
        it "test div" $
            applyParser parseBinOp "/" `shouldBe` Right KBinOpDiv

    describe "parse unary operator" $ do
        it "test not" $
            applyParser parseUnOp "!" `shouldBe` Right KUnOpNot
        it "test less" $
            applyParser parseUnOp "-" `shouldBe` Right KUnOpLess

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
            (Right $ KPrimaryExpressions
                (KListExpr
                    [ KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                        [ (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3) ]
                    , KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                        [ (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 5) ]
                    , KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 6)
                        [ (KBinOpMul, (KPostfix . KPrimary . KLiteral . KDecimalConst) 7) ]
                    ]))

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
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) []
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []
                           ]))

    describe "parse call expr" $ do
        it "test call expr with one args" $
            applyParser parseCallExpr "(1)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) [] ])

        it "test call expr with two args" $
            applyParser parseCallExpr "(1,2)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) []
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []
                           ])

        it "test call expr with three args" $
            applyParser parseCallExpr "(1,2,3)"
            `shouldBe`
            Right
                (KCallExpr [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) []
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []
                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3) []
                           ])

    describe "parse unary" $
        it "test unary" $
            applyParser parseUnary "!toto" `shouldBe` Right (KUnOpUnary KUnOpNot ((KPostfix . KPrimary . KIdentifier) "toto"))

    describe "parse expression" $ do
        it "test expression" $
            applyParser parseExpression "2" `shouldBe` Right (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) [])

        it "test nested expression" $
            applyParser parseExpression "2+3"
            `shouldBe`
            Right
                (KExpression
                    ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                    [(KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3)])

        it "test double nested expression" $
            applyParser parseExpression "2-3-4"
            `shouldBe`
            Right
                (KExpression
                    ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                    [ (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3)
                    , (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                    ])

    describe "parse expressions" $ do
        it "test simple expression" $
            applyParser parseListExpr "2+3"
            `shouldBe`
            Right
                (KListExpr
                    [ KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                        [ (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3) ]
                    ])

        it "test expressions separated by two points" $
            applyParser parseListExpr "2+3:4-5:6*7"
            `shouldBe`
            Right
                (KListExpr
                    [ KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                        [ (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3) ]
                    , KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4)
                        [ (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 5) ]
                    , KExpression
                        ((KPostfix . KPrimary . KLiteral . KDecimalConst) 6)
                        [ (KBinOpMul, (KPostfix . KPrimary . KLiteral . KDecimalConst) 7) ]
                    ])

    describe "parse type" $ do
        it "test parse int type" $
            applyParser parseType "int" `shouldBe` Right KIntType
        it "test parse double type" $
            applyParser parseType "double" `shouldBe` Right KDoubleType
        it "test parse double type" $
            applyParser parseType "void" `shouldBe` Right KVoidType

    describe "parse prototype arg" $ do
        it "test int arg" $
            applyParser parsePrototypeArg "num:int" `shouldBe` Right (KPrototypeArg "num" KIntType)
        it "test double arg" $
            applyParser parsePrototypeArg "n:double" `shouldBe` Right (KPrototypeArg "n" KDoubleType)

    describe "parse prototype args" $ do
        it "test with no argument" $
            applyParser parsePrototypeArgs "():double"
            `shouldBe`
            Right (KPrototypeArgs [] KDoubleType)

        it "test with one argument" $
            applyParser parsePrototypeArgs "(num:int):double"
            `shouldBe`
            Right (KPrototypeArgs [KPrototypeArg "num" KIntType] KDoubleType)

        it "test with multiple argument" $
            applyParser parsePrototypeArgs "(num:int num2:double num3:int):double"
            `shouldBe`
            Right (KPrototypeArgs [ KPrototypeArg "num" KIntType
                                  , KPrototypeArg "num2" KDoubleType
                                  , KPrototypeArg "num3" KIntType
                                  ] KDoubleType)

    describe "parse prototype" $
        it "test with multiple argument" $
            applyParser parsePrototype "addAll(num:int num2:double num3:int):double"
            `shouldBe`
            (Right (KPrototype "addAll" (KPrototypeArgs [ KPrototypeArg "num" KIntType
                                                        , KPrototypeArg "num2" KDoubleType
                                                        , KPrototypeArg "num3" KIntType
                                                        ] KDoubleType)))

    describe "parse defs" $ do
        it "test implementation of a function with args" $
            applyParser parseDefs "add(num1:int num2:int):int num1+num2"
            `shouldBe`
            (Right $ KDefs
                        (KPrototype "add" (KPrototypeArgs [ KPrototypeArg "num1" KIntType
                                                          , KPrototypeArg "num2" KIntType
                                                          ] KIntType))
                        (KListExpr
                            [ KExpression
                                ((KPostfix . KPrimary . KIdentifier) "num1")
                                [ (KBinOpPlus, (KPostfix . KPrimary . KIdentifier) "num2") ]
                            ]))

        it "test implementation of a function whithout args" $
            applyParser parseDefs "add():int 2+3"
            `shouldBe`
            (Right $ KDefs
                        (KPrototype "add" (KPrototypeArgs [] KIntType))
                        (KListExpr
                            [ KExpression
                                ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                                [ (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3) ]
                            ]))

    describe "parse kdefs" $ do
        it "test definition" $
            applyParser parseKDefs "def add():int 2+3;" `shouldBe` applyParser parseDefs "add():int 2+3"

        it "test expressions" $
            applyParser parseKDefs "2+3+4;" `shouldBe` KExpressions <$> applyParser parseExpressions "2+3+4"

        it "test definition fail" $
            applyParser parseKDefs "DEF 2+3+4;" `shouldSatisfy` isLeft
            
        it "test expressions fail" $
            applyParser parseKDefs "2+3+4" `shouldSatisfy` isLeft
    
    describe "parse statement" $ do
        it "test one kdefs" $
            applyParser parseStmt "def add():int 2+3;"
            `shouldBe`
            (Right $ KStmt (rights [ applyParser parseKDefs "def add():int 2+3;" ]))

        it "test multiple kdefs" $
            applyParser parseStmt "def add():int 2+3; def mul(num1:int num2:int):int num1*num2; 3+3; add();"
            `shouldBe`
            (Right $ KStmt (rights [ applyParser parseKDefs "def add():int 2+3;"
                                   , applyParser parseKDefs "def mul(num1:int num2:int):int num1*num2;"
                                   , applyParser parseKDefs "3+3;"
                                   , applyParser parseKDefs "add();"
                                   ]))

                    
