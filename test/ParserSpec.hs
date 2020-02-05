module ParserSpec where

import           Data.Either
import           KoakAST
import           Parser
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parser" $ do
        it "test two expressions" $
            parseKoak "1-2-3; num1/num2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KListExpr
                           [ KExpression
                                 ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                                 [ (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                                 , (KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 3)
                                 ]
                           ]
                     , KExpressions $
                       KListExpr
                           [ KExpression
                                 ((KPostfix . KPrimary . KIdentifier) "num1")
                                 [(KBinOpDiv, (KPostfix . KPrimary . KIdentifier) "num2")]
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
                                      ((KPostfix . KPrimary . KIdentifier) "num1")
                                      [(KBinOpPlus, (KPostfix . KPrimary . KIdentifier) "num2")]
                                ])
                     ])
        it "test function call" $
            parseKoak "add(2,4);" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KListExpr
                           [ KExpression
                                 (KPostfix $
                                  KFuncCall
                                      (KIdentifier "add")
                                      (KCallExpr
                                           [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []
                                           , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4) []
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
                                ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                                [(KBinOpInf, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)])
                           (KListExpr
                                [ KExpression
                                      ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                                      [(KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]
                                ])
                     ])
        it "test if then else" $
            parseKoak "if 1<2 then 1 else 2;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KIfElse
                           (KExpression
                                ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                                [(KBinOpInf, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)])
                           (KListExpr [KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) []])
                           (KListExpr [KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []])
                     ])
        it "test while" $
            parseKoak "while i<10 do i=i+1;" `shouldBe`
            Right
                (KStmt
                     [ KExpressions $
                       KWhile
                           (KExpression
                                ((KPostfix . KPrimary . KIdentifier) "i")
                                [(KBinOpInf, (KPostfix . KPrimary . KLiteral . KDecimalConst) 10)])
                           (KListExpr
                                [ KExpression
                                      ((KPostfix . KPrimary . KIdentifier) "i")
                                      [ (KBinOpAssign, (KPostfix . KPrimary . KIdentifier) "i")
                                      , (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
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
                                ((KPostfix . KPrimary . KIdentifier) "i")
                                [(KBinOpAssign, (KPostfix . KPrimary . KLiteral . KDecimalConst) 0)])
                           (KExpression
                                ((KPostfix . KPrimary . KIdentifier) "i")
                                [(KBinOpInf, (KPostfix . KPrimary . KLiteral . KDecimalConst) 10)])
                           (KExpression
                                ((KPostfix . KPrimary . KIdentifier) "i")
                                [ (KBinOpAssign, (KPostfix . KPrimary . KIdentifier) "i")
                                , (KBinOpPlus, (KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                                ])
                           (KListExpr
                                [ KExpression
                                      ((KPostfix . KPrimary . KIdentifier) "res")
                                      [ (KBinOpAssign, (KPostfix . KPrimary . KIdentifier) "res")
                                      , (KBinOpPlus, (KPostfix . KPrimary . KIdentifier) "i")
                                      , (KBinOpMul, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)
                                      ]
                                ])
                     ])
