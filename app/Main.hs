module Main where

import LLVM.AST as AST
import LLVM.AST.CallingConvention as AST
import LLVM.AST.Constant as C
import LLVM.AST.Global
import LLVM.AST.IntegerPredicate as AST
import LLVM.AST.Type as AST
import LLVM.AST.AddrSpace as AST

import LLVM.Context
import LLVM.Module
import LLVM.Target

import qualified Data.ByteString.Char8 as BS

import LLVMSubExample

main :: IO ()
main = withContext $ \ctx -> do
--    tm <- getDefaultTargetTriple
    -- writeObjectToFile tm "a.out" myModule
    llvm <- withModuleFromAST ctx llvmTestModule moduleLLVMAssembly
    BS.putStrLn llvm
