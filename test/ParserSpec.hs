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
        it "test condition" $
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
