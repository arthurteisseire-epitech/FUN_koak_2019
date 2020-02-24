{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVMSpec where

import           KoakAST
import           KoakToLLVM
import           Test.Hspec

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Global
import           LLVM.AST.Type              as AST

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "expression to BasicBlock" $ do
        it "test 1" $
            kExpressionToBasicBlock (KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1) []) `shouldBe`
            BasicBlock (Name "entry") [] (Do $ Ret (Just $ ConstantOperand (C.Int 32 1)) [])
        it "test 1-2" $
            kExpressionToBasicBlock
                (KExpression
                     ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                     [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]) 
            `shouldBe`
            BasicBlock
                (Name "entry")
                [Name "res" := AST.Sub False False (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []]
                (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
    describe "function definition" $ do
        it "test function main definition" $
            kDefToGlobalDef
                (KDefs
                     (KPrototype "main" (KPrototypeArgs [] KIntType))
                     (KListExpr
                          [ KExpression
                                ((KPostfix . KPrimary . KLiteral . KDecimalConst) 44)
                                [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]
                          ])) 
            `shouldBe`
            GlobalDefinition
                functionDefaults
                    { name = Name "main"
                    , parameters = ([], False)
                    , returnType = AST.i32
                    , basicBlocks =
                          [ BasicBlock
                                (Name "entry")
                                [ Name "res" :=
                                  AST.Sub False False (ConstantOperand (C.Int 32 44)) (ConstantOperand (C.Int 32 2)) []
                                ]
                                (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
                          ]
                    }
        it "test function with args" $
            kDefToGlobalDef
                (KDefs
                     (KPrototype
                          "sub"
                          (KPrototypeArgs [KPrototypeArg "a" KIntType, KPrototypeArg "b" KIntType] KIntType))
                     (KListExpr
                          [ KExpression
                                ((KPostfix . KPrimary . KIdentifier) "a")
                                [(KBinOpLess, (KPostfix . KPrimary . KIdentifier) "b")]
                          ])) 
            `shouldBe`
            GlobalDefinition
                functionDefaults
                    { name = Name "sub"
                    , parameters = ([Parameter AST.i32 (Name "a") [], Parameter AST.i32 (Name "b") []], False)
                    , returnType = AST.i32
                    , basicBlocks =
                          [ BasicBlock
                                (Name "entry")
                                [ Name "res" :=
                                  AST.Sub
                                      False
                                      False
                                      (LocalReference AST.i32 (Name "a"))
                                      (LocalReference AST.i32 (Name "b"))
                                      []
                                ]
                                (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
                          ]
                    }
    describe "call function" $ do
        it "test call function without args" $
            kCallToLLVMCall (KFuncCall (KIdentifier "ret1") (KCallExpr [])) 
            `shouldBe`
            AST.Call
                Nothing
                AST.C
                []
                (Right
                     (ConstantOperand
                          (C.GlobalReference
                               (PointerType (FunctionType AST.i32 [] False) (AST.AddrSpace 0))
                               (Name "ret1"))))
                []
                []
                []
        it "test call function with args" $
            kCallToLLVMCall
                (KFuncCall
                     (KIdentifier "add")
                     (KCallExpr
                          [ KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 2) []
                          , KExpression ((KPostfix . KPrimary . KLiteral . KDecimalConst) 4) []
                          ])) 
            `shouldBe`
            AST.Call
                Nothing
                AST.C
                []
                (Right
                     (ConstantOperand
                          (C.GlobalReference
                               (PointerType (FunctionType AST.i32 [AST.i32, AST.i32] False) (AST.AddrSpace 0))
                               (Name "add"))))
                [(ConstantOperand (C.Int 32 2), []), (ConstantOperand (C.Int 32 4), [])]
                []
                []
    describe "expressions to llvm main" $
        it "test that expressions are in llvm main" $
            koakToLLVM
                (KStmt [ KExpressions
                            (KListExpr
                                  [ KExpression
                                      ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3)
                                      [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 4)]
                                  ])
                       , KExpressions
                             (KListExpr
                                   [ KExpression
                                       ((KPostfix . KPrimary . KLiteral . KDecimalConst) 3)
                                       [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 4)]
                                   ])
                       ])
            `shouldBe`
            [ GlobalDefinition
                functionDefaults
                    { name = Name "main"
                    , parameters = ([], False)
                    , returnType = AST.i32
                    , basicBlocks =
                          [ BasicBlock
                                (Name "entry")
                                [ Name "res" :=
                                  AST.Sub
                                      False
                                      False
                                      (ConstantOperand $ C.Int 32 3)
                                      (ConstantOperand $ C.Int 32 4)
                                      []
                                ]
                                (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
                          , BasicBlock
                               (Name "entry")
                               [ Name "res" :=
                                 AST.Sub
                                     False
                                     False
                                     (ConstantOperand $ C.Int 32 3)
                                     (ConstantOperand $ C.Int 32 4)
                                     []
                               ]
                               (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
                          ]
                    }
            ]
    describe "function definition calling an other one" $
        it "test call function with args from main" $
        koakToLLVM
            (KStmt
                 [ KDefs
                       (KPrototype
                            "add"
                            (KPrototypeArgs [KPrototypeArg "x" KIntType, KPrototypeArg "y" KIntType] KIntType))
                       (KListExpr
                            [ KExpression
                                  (KPostfix (KPrimary (KIdentifier "x")))
                                  [(KBinOpPlus, KPostfix (KPrimary (KIdentifier "y")))]
                            ])
                 , KExpressions
                       (KListExpr
                            [ KExpression
                                  (KPostfix
                                       (KFuncCall
                                            (KIdentifier "add")
                                            (KCallExpr
                                                 [ KExpression (KPostfix (KPrimary (KLiteral (KDecimalConst 3)))) []
                                                 , KExpression (KPostfix (KPrimary (KLiteral (KDecimalConst 4)))) []
                                                 ])))
                                  []
                            ])
                 ]) 
        `shouldBe`
        [ GlobalDefinition
              functionDefaults
                  { name = Name "main"
                  , parameters = ([], False)
                  , returnType = AST.i32
                  , basicBlocks =
                        [ BasicBlock
                              (Name "entry")
                              [ Name "callRes" :=
                                AST.Call
                                    Nothing
                                    AST.C
                                    []
                                    (Right
                                         (ConstantOperand
                                              (C.GlobalReference
                                                   (PointerType
                                                        (FunctionType AST.i32 [AST.i32, AST.i32] False)
                                                        (AST.AddrSpace 0))
                                                   (Name "add"))))
                                    [(ConstantOperand (C.Int 32 3), []), (ConstantOperand (C.Int 32 4), [])]
                                    []
                                    []
                              ]
                              (Do $ Ret (Just $ LocalReference AST.i32 (Name "callRes")) [])
                        ]
                  }
        , GlobalDefinition
              functionDefaults
                  { name = Name "add"
                  , parameters = ([Parameter AST.i32 (Name "x") [], Parameter AST.i32 (Name "y") []], False)
                  , returnType = AST.i32
                  , basicBlocks =
                        [ BasicBlock
                              (Name "entry")
                              [ Name "res" :=
                                AST.Add
                                    False
                                    False
                                    (LocalReference AST.i32 (Name "x"))
                                    (LocalReference AST.i32 (Name "y"))
                                    []
                              ]
                              (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
                        ]
                  }
        ]
