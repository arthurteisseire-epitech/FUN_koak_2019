{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVMSpec where

import           KoakAST
import           KoakToLLVM
import           Test.Hspec

import           LLVM.AST                  as AST
import           LLVM.AST.Constant         as C
import           LLVM.AST.Float            as F
import           LLVM.AST.IntegerPredicate as AST
import           LLVM.AST.Type              as AST

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
            AST.FSub AST.noFastMathFlags (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []
        it "test 1.0-2.0" $
            kExpressionToLInstruction
                (KExpression
                     ((KPostfix . KPrimary . KLiteral . KDoubleConst) 1.0)
                     [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDoubleConst) 2.0)]) `shouldBe`
            AST.FSub
                AST.noFastMathFlags
                (ConstantOperand (C.Float (F.Single 1.0)))
                (ConstantOperand (C.Float (F.Single 2.0)))
                []

    describe "expression to BasicBlock" $
        it "test 1-2" $
        kExpressionToBasicBlock
            (KExpression
                 ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
                 [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]) `shouldBe`
        BasicBlock
            (Name "entry")
            [ Name "res" :=
              AST.FSub AST.noFastMathFlags (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []
            ]
            (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])
