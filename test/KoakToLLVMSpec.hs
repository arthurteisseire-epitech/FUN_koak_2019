{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVMSpec where

import           KoakAST
import           KoakToLLVM
import           Test.Hspec

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Float             as F
import           LLVM.AST.Global
import           LLVM.AST.IntegerPredicate  as AST
import           LLVM.AST.Type              as AST

import           LLVM.Context
import           LLVM.Module
import           LLVM.Target

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "expression to instruction" $ do
        it "test 1-2" $
            kExpressionToLInstruction
                (KExpression
                     ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                     [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]) `shouldBe`
            AST.Sub False False (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []
        it "test 1.0-2.0" $
            kExpressionToLInstruction
                (KExpression
                     ((KPostfix . KPrimary . KLiteral . KDoubleConst) 1.0)
                     [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDoubleConst) 2.0)]) `shouldBe`
            AST.Sub
                False False
                (ConstantOperand (C.Float (F.Single 1.0)))
                (ConstantOperand (C.Float (F.Single 2.0)))
                []

    describe "expression to BasicBlock" $ do
        it "test 1-2" $
            kExpressionToBasicBlock
                (KExpression
                     ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                     [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]) `shouldBe`
            BasicBlock
                (Name "entry")
                [ Name "res" :=
                  AST.Sub False False (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []
                ]
                (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])

    describe "function definition" $ do
        it "test function main definition" $
            kDefToGlobalDef
                (KStmt
                     [ KDefs
                           (KPrototype
                                "main"
                                (KPrototypeArgs [] KIntType))
                           (KListExpr
                                [ KExpression
                                      ((KPostfix . KPrimary . KLiteral . KDecimalConst) 44)
                                      [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]
                                ])
                     ]) `shouldBe`
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
