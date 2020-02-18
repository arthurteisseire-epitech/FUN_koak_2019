{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVM where

import           KoakAST

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
import           LLVM.Prelude
import           LLVM.Target

import           Data.Maybe

kDefToGlobalDef :: KStmt -> Definition
kDefToGlobalDef (KStmt [KDefs (KPrototype funcName (KPrototypeArgs args KIntType)) (KListExpr [expr])]) =
    GlobalDefinition
        functionDefaults
            { name = mkName funcName
            , parameters = (kArgsToLArgs args, False)
            , returnType = AST.i32
            , basicBlocks = [kExpressionToBasicBlock expr]
            }

kArgsToLArgs :: [KPrototypeArg] -> [Parameter]
kArgsToLArgs args = map (\(KPrototypeArg kId kType) -> Parameter (kReturnTypeToLReturnType kType) (mkName kId) []) args

kReturnTypeToLReturnType :: KType -> Type
kReturnTypeToLReturnType KIntType = AST.i32

kExpressionToBasicBlock :: KExpression -> BasicBlock
kExpressionToBasicBlock expr =
    BasicBlock
        (Name "entry")
        [ Name "res" :=
          ((binOpConvert . getBinOp) expr)
              False
              False
              ((kPrimaryToOperand . getFirstKPrimary) expr)
              ((kPrimaryToOperand . getSecondKPrimary) expr)
              []
        ]
        (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])

kExpressionToLInstruction :: KExpression -> Instruction
kExpressionToLInstruction expr =
    ((binOpConvert . getBinOp) expr)
        False
        False
        ((kPrimaryToOperand . getFirstKPrimary) expr)
        ((kPrimaryToOperand . getSecondKPrimary) expr)
        []

getBinOp :: KExpression -> KBinOp
getBinOp (KExpression _ [(binOp, _)]) = binOp

getFirstKPrimary :: KExpression -> KPrimary
getFirstKPrimary (KExpression (KPostfix (KPrimary primary)) _) = primary

getSecondKPrimary :: KExpression -> KPrimary
getSecondKPrimary (KExpression _ [(_, (KPostfix (KPrimary primary)))]) = primary

getValueFromPostfix :: KPostfix -> KLiteral
getValueFromPostfix (KPrimary (KLiteral x)) = x

kPrimaryToOperand :: KPrimary -> Operand
kPrimaryToOperand (KLiteral literal) = kLiteralToLOperand literal
kPrimaryToOperand (KIdentifier identifier) = LocalReference AST.i32 (mkName identifier)

kLiteralToLOperand :: KLiteral -> Operand
kLiteralToLOperand (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))
kLiteralToLOperand (KDoubleConst x) = ConstantOperand (C.Float (F.Single (realToFrac x)))

binOpConvert :: KBinOp -> Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
binOpConvert KBinOpLess = AST.Sub
