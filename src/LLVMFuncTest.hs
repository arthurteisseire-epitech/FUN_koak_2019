{-# LANGUAGE OverloadedStrings #-}

module LLVMFuncTest where

import LLVM.AST as AST
import LLVM.AST.Global
import LLVM.AST.Type as AST

defMin :: Definition
defMin = GlobalDefinition functionDefaults
    { name = Name "min"
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
            [Name "res" := AST.FSub
                AST.noFastMathFlags
                (LocalReference AST.i32 (Name "a"))
                (LocalReference AST.i32 (Name "b"))
                []
            ]
            (Do $ Ret
                (Just $ LocalReference AST.i1 (Name "res"))
                []
            )

llvmTestModule :: AST.Module
llvmTestModule = defaultModule
    { moduleName = "min"
    , moduleDefinitions = [defMin]
    }
