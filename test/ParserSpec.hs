module ParserSpec where

import           KoakAST
import           Parser
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parser" $ do
        it "test two expressions" $
            parseKoak "1-2-3; num1/num2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KListExpr
                           [ KExpression
                                 ((KPrimary . KLiteral . KDecimalConst) 1)
                                 [ (KBinOpLess, (KPrimary . KLiteral . KDecimalConst) 2)
                                 , (KBinOpLess, (KPrimary . KLiteral . KDecimalConst) 3)
                                 ]
                           ]
                     , KExpressions $
                       KListExpr
                           [ KExpression
                                 ((KPrimary . KIdentifier) "num1")
                                 [(KBinOpDiv, (KPrimary . KIdentifier) "num2")]
                           ]
                     ])
        it "test definition" $
            parseKoak "def add(num1:int num2:int):int num1+num2;" `shouldBe`
            Right
                (KStmt
                     [ KDefs
                           (KPrototype
                                "add"
                                (KPrototypeArgs [KPrototypeArg "num1" KIntType, KPrototypeArg "num2" KIntType] KIntType))
                           (KListExpr
                                [ KExpression
                                      ((KPrimary . KIdentifier) "num1")
                                      [(KBinOpPlus, (KPrimary . KIdentifier) "num2")]
                                ])
                     ])
        it "test function call" $
            parseKoak "add(2,4);" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KListExpr
                           [ KExpression
                                 (KFuncCall
                                      (KIdentifier "add")
                                      (KCallExpr
                                           [ KExpression ((KPrimary . KLiteral . KDecimalConst) 2) []
                                           , KExpression ((KPrimary . KLiteral . KDecimalConst) 4) []
                                           ]))
                                 []
                           ]
                     ])
        it "test if then" $
            parseKoak "if 1<2 then 1+2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KIf
                           (KExpression
                                ((KPrimary . KLiteral . KDecimalConst) 1)
                                [(KBinOpInf, (KPrimary . KLiteral . KDecimalConst) 2)])
                           (KListExpr
                                [ KExpression
                                      ((KPrimary . KLiteral . KDecimalConst) 1)
                                      [(KBinOpPlus, (KPrimary . KLiteral . KDecimalConst) 2)]
                                ])
                     ])
        it "test if then else" $
            parseKoak "if 1<2 then 1 else 2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KIfElse
                           (KExpression
                                ((KPrimary . KLiteral . KDecimalConst) 1)
                                [(KBinOpInf, (KPrimary . KLiteral . KDecimalConst) 2)])
                           (KListExpr [KExpression ((KPrimary . KLiteral . KDecimalConst) 1) []])
                           (KListExpr [KExpression ((KPrimary . KLiteral . KDecimalConst) 2) []])
                     ])
        it "test while" $
            parseKoak "while i<10 do i=i+1;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KWhile
                           (KExpression
                                ((KPrimary . KIdentifier) "i")
                                [(KBinOpInf, (KPrimary . KLiteral . KDecimalConst) 10)])
                           (KListExpr
                                [ KExpression
                                      ((KPrimary . KIdentifier) "i")
                                      [ (KBinOpAssign, (KPrimary . KIdentifier) "i")
                                      , (KBinOpPlus, (KPrimary . KLiteral . KDecimalConst) 1)
                                      ]
                                ])
                     ])
        it "test for" $
            parseKoak "for i=0,i<10,i=i+1 in res=res+i*2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KFor
                           (KExpression
                                ((KPrimary . KIdentifier) "i")
                                [(KBinOpAssign, (KPrimary . KLiteral . KDecimalConst) 0)])
                           (KExpression
                                ((KPrimary . KIdentifier) "i")
                                [(KBinOpInf, (KPrimary . KLiteral . KDecimalConst) 10)])
                           (KExpression
                                ((KPrimary . KIdentifier) "i")
                                [ (KBinOpAssign, (KPrimary . KIdentifier) "i")
                                , (KBinOpPlus, (KPrimary . KLiteral . KDecimalConst) 1)
                                ])
                           (KListExpr
                                [ KExpression
                                      ((KPrimary . KIdentifier) "res")
                                      [ (KBinOpAssign, (KPrimary . KIdentifier) "res")
                                      , (KBinOpPlus, (KPrimary . KIdentifier) "i")
                                      , (KBinOpMul, (KPrimary . KLiteral . KDecimalConst) 2)
                                      ]
                                ])
                     ])
