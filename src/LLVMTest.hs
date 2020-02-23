{-# LANGUAGE OverloadedStrings #-}

module LLVMTest where

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
import           LLVM.Target

defMin :: Definition
defMin =
    GlobalDefinition
        functionDefaults {name = Name "main", parameters = ([], False), returnType = AST.i32, basicBlocks = [entry]}
  where
    entry =
        BasicBlock
            (Name "entry")
            [ Name "res" :=
              AST.Sub False False (ConstantOperand (C.Int 32 3)) (ConstantOperand (C.Int 32 1)) []
            ]
            (Do $ Ret (Just $ LocalReference AST.i32 (Name "res")) [])

llvmTestModule :: AST.Module
llvmTestModule = defaultModule {moduleName = "main", moduleDefinitions = [defMin]}
