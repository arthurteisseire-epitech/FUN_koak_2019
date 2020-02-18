module KoakToLLVM where

import           KoakAST

import           LLVM.AST                   as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C


koakToLLVM :: KExpression -> Instruction
koakToLLVM expr = ((binOpConvert . getBinOp) expr) AST.noFastMathFlags ((intConvert . getFirstInt) expr) ((intConvert . getSecondInt) expr) []

getBinOp :: KExpression -> KBinOp
getBinOp (KExpression _ [(binOp, _)]) = binOp

getFirstInt :: KExpression -> KLiteral
getFirstInt (KExpression (KPostfix (KPrimary primary)) _) = getValueFromPrimary primary

getSecondInt :: KExpression -> KLiteral
getSecondInt (KExpression _ [(_, (KPostfix (KPrimary primary)))]) = getValueFromPrimary primary

getValueFromPrimary :: KPrimary -> KLiteral
getValueFromPrimary (KLiteral x) = x

intConvert :: KLiteral -> Operand
intConvert (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))

binOpConvert :: KBinOp -> FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction
binOpConvert KBinOpLess = AST.FSub

