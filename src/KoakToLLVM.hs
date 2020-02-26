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
    exprToFirstOperand (KExpression postfix _) = postfixToOperand postfix

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
kExpressionToBasicBlock expr@(KExpression firstPostfix@(KPrimary _) []) =
    BasicBlock
        (Name "entry")
        []
        (Do $ Ret (Just $ postfixToOperand firstPostfix) [])

kExpressionToBasicBlock (KExpression call@(KFuncCall _ _) _) =
    BasicBlock
        (Name "entry")
        ["callRes" := kCallToLLVMCall call]
        (Do $ Ret (Just $
            LocalReference AST.i32 (Name "callRes")) [])

kExpressionToBasicBlock (KExpression firstPostfix (x:xs)) =
    BasicBlock
        (Name "entry")
        ((UnName 0 := arithmetic (fst x) (postfixToOperand firstPostfix) (postfixToOperand $ snd x) []) :
         zipWith binOpUnaryPairToNamedInstruction [0 ..] xs)
        (Do $ Ret (Just $ LocalReference AST.i32 (UnName $ fromIntegral $ length xs)) [])

binOpUnaryPairToNamedInstruction :: Word -> (KBinOp, KPostfix) -> Named Instruction
binOpUnaryPairToNamedInstruction idx (binOp, postfix) =
    UnName (idx + 1) := arithmetic binOp (LocalReference AST.i32 $ UnName idx) (postfixToOperand postfix) []

postfixToOperand :: KPostfix -> Operand
postfixToOperand (KPrimary (KIdentifier identifier)) = LocalReference AST.i32 (mkName identifier)
postfixToOperand (KPrimary (KLiteral literal)) = kLiteralToLOperand literal
  where
    kLiteralToLOperand (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))
    kLiteralToLOperand (KDoubleConst x) = ConstantOperand (C.Float (F.Single (realToFrac x)))

arithmetic :: KBinOp -> Operand -> Operand -> InstructionMetadata -> Instruction
arithmetic KBinOpLess = AST.Sub False False
arithmetic KBinOpPlus = AST.Add False False
arithmetic KBinOpMul  = AST.Mul False False
arithmetic KBinOpDiv  = AST.SDiv False
