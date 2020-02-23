{-# LANGUAGE OverloadedStrings #-}

module LLVMTestOp where

import LLVM.AST as AST
import LLVM.AST.Global
import LLVM.AST.Type as AST

defMul :: Definition
defMul = GlobalDefinition functionDefaults
    { name = Name "mul"
    , parameters =
        ( [ Parameter AST.i32 (Name "a") []
          , Parameter AST.i32 (Name "b") [] ]
        , False
        )
    , returnType = AST.i32
    , basicBlocks = [entry]
    }
    where
        entry = BasicBlock
            (Name "entry")
            [Name "res" := AST.Mul
                False
                False
                (LocalReference AST.i32 (Name "a"))
                (LocalReference AST.i32 (Name "b"))
                []
            ]
            (Do $ Ret
                (Just $ LocalReference AST.i32 (Name "res"))
                []
            )

defSub :: Definition
defSub = GlobalDefinition functionDefaults
    { name = Name "sub"
    , parameters =
        ( [ Parameter AST.i32 (Name "a") []
          , Parameter AST.i32 (Name "b") [] ]
        , False
        )
    , returnType = AST.i32
    , basicBlocks = [entry]
    }
    where
        entry = BasicBlock
            (Name "entry")
            [Name "res" := AST.Sub
                False
                False
                (LocalReference AST.i32 (Name "a"))
                (LocalReference AST.i32 (Name "b"))
                []
            ]
            (Do $ Ret
                (Just $ LocalReference AST.i32 (Name "res"))
                []
            )

llvmTestOp :: AST.Module
llvmTestOp = defaultModule
    { moduleName = "OpModule"
    , moduleDefinitions = [defSub, defMul]
    }