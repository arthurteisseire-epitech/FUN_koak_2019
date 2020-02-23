{-# LANGUAGE OverloadedStrings #-}

module LLVMTestMain where

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Global
import           LLVM.AST.Type              as AST

defSub :: Definition
defSub = GlobalDefinition functionDefaults
    { name = Name "sub"
    , parameters =
        ([
            Parameter AST.i32 (Name "a") []
            , Parameter AST.i32 (Name "b") []
        ]
        , False
        )
    , returnType = AST.i32
    , basicBlocks = []
    }

defMain :: Definition
defMain =
    GlobalDefinition
        functionDefaults {name = Name "main", parameters = ([], False), returnType = AST.i32, basicBlocks = [entry]}
  where
    entry =
        BasicBlock
            (Name "entry")
            [Name "SubRes" := AST.Call
                Nothing
                AST.C
                []
                (Right (ConstantOperand (C.GlobalReference
                    (PointerType (FunctionType AST.i32 [AST.i32, AST.i32] False) (AST.AddrSpace 0))
                    (Name "sub"))))
                [(ConstantOperand (C.Int 32 4), []), (ConstantOperand (C.Int 32 2), [])]
                []
                []
            ]
            (Do $ Ret
                (Just $ LocalReference AST.i32 (Name "SubRes"))
                []
            )

llvmTestMain :: AST.Module
llvmTestMain = defaultModule {moduleName = "mainModule", moduleDefinitions = [defSub, defMain]}
