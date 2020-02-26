{-# LANGUAGE OverloadedStrings #-}

module KoakToLLVM where

import           KoakAST

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Float             as F
import           LLVM.AST.Global
import           LLVM.AST.Type              as AST

koakToLLVM :: KStmt -> [Definition]
koakToLLVM (KStmt defs) = kExpressionsToMain (unsafeKDefsToKExpressions expressions) : map kDefToGlobalDef definitions
  where
    expressions = filter isKExpressions defs
    definitions = filter (not . isKExpressions) defs

    unsafeKDefsToKExpressions = map (\(KExpressions exprs) -> exprs)

    isKExpressions (KExpressions _) = True
    isKExpressions _ = False

kCallToLLVMCall :: KPostfix -> Instruction
kCallToLLVMCall (KFuncCall (KIdentifier identifier) (KCallExpr params)) =
    AST.Call
        Nothing
        AST.C
        []
        (Right
             (ConstantOperand
                  (C.GlobalReference
                       (PointerType (FunctionType AST.i32 getReturnTypes False) (AST.AddrSpace 0))
                       (mkName identifier))))
        (map (\param -> (exprToFirstOperand param, [])) params)
        []
        []
  where
    getReturnTypes = map (const AST.i32) params

exprToFirstOperand :: KExpression -> Operand
exprToFirstOperand = postfixToOperand . getFirstPostfix

kExpressionsToMain :: [KExpressions] -> Definition
kExpressionsToMain expressions = 
    GlobalDefinition
        functionDefaults
            { name = mkName "main"
            , parameters = ([], False)
            , returnType = AST.i32
            , basicBlocks = concatMap (\(KListExpr exprs) -> map kExpressionToBasicBlock exprs) expressions
            }

kDefToGlobalDef :: KDefs -> Definition
kDefToGlobalDef (KDefs (KPrototype funcName (KPrototypeArgs params KIntType)) (KListExpr exprs)) =
    GlobalDefinition
        functionDefaults
            { name = mkName funcName
            , parameters = (kArgsToLArgs params, False)
            , returnType = AST.i32
            , basicBlocks = map kExpressionToBasicBlock exprs
            }

kArgsToLArgs :: [KPrototypeArg] -> [Parameter]
kArgsToLArgs = map (\(KPrototypeArg kId kType) -> Parameter (kReturnTypeToLReturnType kType) (mkName kId) [])

kReturnTypeToLReturnType :: KType -> Type
kReturnTypeToLReturnType KIntType = AST.i32

kExpressionToBasicBlock :: KExpression -> BasicBlock
kExpressionToBasicBlock expr@(KExpression (KPostfix (KPrimary _)) []) =
    BasicBlock
        (Name "entry")
        []
        (Do $ Ret (Just
            ((postfixToOperand . getFirstPostfix) expr)) [])

kExpressionToBasicBlock (KExpression (KPostfix call@(KFuncCall _ _)) _) =
    BasicBlock
        (Name "entry")
        ["callRes" := kCallToLLVMCall call]
        (Do $ Ret (Just $
            LocalReference AST.i32 (Name "callRes")) [])

kExpressionToBasicBlock expr@(KExpression firstUnary pairs) =
    BasicBlock
        (Name "entry")
        ((UnName 0 :=
          (binOpConvert . getFirstBinOp)
              expr
              ((postfixToOperand . getFirstPostfix) expr)
              ((postfixToOperand . getSecondPostfix) expr)
              []) : zipWith binOpUnaryPairToNamedInstruction [0..] (tail pairs))
        (Do $ Ret (Just $ LocalReference AST.i32 (UnName . fromIntegral $ length pairs - 1)) [])

binOpUnaryPairToNamedInstruction :: Word -> (KBinOp, KUnary) -> Named Instruction
binOpUnaryPairToNamedInstruction idx (binOp, KPostfix postfix) =
    UnName (idx + 1) := binOpConvert binOp (LocalReference AST.i32 (UnName idx)) (postfixToOperand postfix) []

getFirstPostfix :: KExpression -> KPostfix
getFirstPostfix (KExpression (KPostfix postfix) _) = postfix

getSecondPostfix :: KExpression -> KPostfix
getSecondPostfix (KExpression _ ((_, KPostfix postfix):_)) = postfix

postfixToOperand :: KPostfix -> Operand
postfixToOperand (KPrimary (KIdentifier identifier)) = LocalReference AST.i32 (mkName identifier)
postfixToOperand (KPrimary (KLiteral literal)) = kLiteralToLOperand literal
  where
    kLiteralToLOperand (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))
    kLiteralToLOperand (KDoubleConst x) = ConstantOperand (C.Float (F.Single (realToFrac x)))

getFirstBinOp :: KExpression -> KBinOp
getFirstBinOp (KExpression _ ((binOp, _):_)) = binOp

binOpConvert :: KBinOp -> Operand -> Operand -> InstructionMetadata -> Instruction
binOpConvert KBinOpLess = AST.Sub False False
binOpConvert KBinOpPlus = AST.Add False False
binOpConvert KBinOpMul  = AST.Mul False False
binOpConvert KBinOpDiv  = AST.SDiv False
