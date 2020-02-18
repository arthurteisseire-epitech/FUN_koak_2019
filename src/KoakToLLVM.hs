{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVM where

import           KoakAST

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Float             as F
import           LLVM.AST.IntegerPredicate  as AST
import           LLVM.AST.Type              as AST

import           Data.Maybe

kExpressionToBasicBlock :: KExpression -> BasicBlock
kExpressionToBasicBlock expr =
    BasicBlock
        (Name "entry")
        [ Name "res" :=
          ((binOpConvert . getBinOp) expr)
              AST.noFastMathFlags
              ((kLiteralToLOperand . getFirstKLiteral) expr)
              ((kLiteralToLOperand . getSecondKLiteral) expr)
              []
        ]
        (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])

kExpressionToLInstruction :: KExpression -> Instruction
kExpressionToLInstruction expr =
    ((binOpConvert . getBinOp) expr)
        AST.noFastMathFlags
        ((kLiteralToLOperand . getFirstKLiteral) expr)
        ((kLiteralToLOperand . getSecondKLiteral) expr)
        []

getBinOp :: KExpression -> KBinOp
getBinOp (KExpression _ [(binOp, _)]) = binOp

getFirstKLiteral :: KExpression -> KLiteral
getFirstKLiteral (KExpression (KPostfix (KPrimary primary)) _) = getValueFromPrimary primary

getSecondKLiteral :: KExpression -> KLiteral
getSecondKLiteral (KExpression _ [(_, (KPostfix (KPrimary primary)))]) = getValueFromPrimary primary

getValueFromPrimary :: KPrimary -> KLiteral
getValueFromPrimary (KLiteral x) = x

kLiteralToLOperand :: KLiteral -> Operand
kLiteralToLOperand (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))
kLiteralToLOperand (KDoubleConst x) = ConstantOperand (C.Float (F.Single (realToFrac x)))

binOpConvert :: KBinOp -> FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction
binOpConvert KBinOpLess = AST.FSub
