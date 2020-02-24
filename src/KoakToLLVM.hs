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
exprToFirstOperand = kPrimaryToOperand . getFirstKPrimary

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
            ((kPrimaryToOperand . getFirstKPrimary) expr)) [])

kExpressionToBasicBlock (KExpression (KPostfix call@(KFuncCall _ _)) _) =
    BasicBlock
        (Name "entry")
        ["callRes" := kCallToLLVMCall call]
        (Do $ Ret (Just $
            LocalReference AST.i32 (Name "callRes")) [])

kExpressionToBasicBlock expr =
    BasicBlock
        (Name "entry")
        [ Name "res" :=
          (binOpConvert . getBinOp)
              expr
              ((kPrimaryToOperand . getFirstKPrimary) expr)
              ((kPrimaryToOperand . getSecondKPrimary) expr)
              []
        ]
        (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])

getBinOp :: KExpression -> KBinOp
getBinOp (KExpression _ [(binOp, _)]) = binOp

getFirstKPrimary :: KExpression -> KPrimary
getFirstKPrimary (KExpression (KPostfix (KPrimary primary)) _) = primary

getSecondKPrimary :: KExpression -> KPrimary
getSecondKPrimary (KExpression _ [(_, KPostfix (KPrimary primary))]) = primary

getValueFromPostfix :: KPostfix -> KLiteral
getValueFromPostfix (KPrimary (KLiteral x)) = x

kPrimaryToOperand :: KPrimary -> Operand
kPrimaryToOperand (KLiteral literal) = kLiteralToLOperand literal
kPrimaryToOperand (KIdentifier identifier) = LocalReference AST.i32 (mkName identifier)

kLiteralToLOperand :: KLiteral -> Operand
kLiteralToLOperand (KDecimalConst x) = ConstantOperand (C.Int 32 (toInteger x))
kLiteralToLOperand (KDoubleConst x) = ConstantOperand (C.Float (F.Single (realToFrac x)))

binOpConvert :: KBinOp -> Operand -> Operand -> InstructionMetadata -> Instruction
binOpConvert KBinOpLess = AST.Sub False False
binOpConvert KBinOpPlus = AST.Add False False
binOpConvert KBinOpMul  = AST.Mul False False
binOpConvert KBinOpDiv  = AST.SDiv False
