{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LLVM.AST                   as AST
import           LLVM.AST.AddrSpace         as AST
import           LLVM.AST.CallingConvention as AST
import           LLVM.AST.Constant          as C
import           LLVM.AST.Global
import           LLVM.AST.IntegerPredicate  as AST
import           LLVM.AST.Type              as AST

import           LLVM.Context
import           LLVM.Module
import           LLVM.Target

import qualified Data.ByteString.Char8      as BS

import           LLVMTestMain
import           LLVMTestOp

import           System.Environment
import           System.Exit
import           System.IO

import           KoakToLLVM
import           Parser

main :: IO ()
main = getArgs >>= interpretFile . head >>= printLLVM . llvmTestModule

interpretFile :: String -> IO Definition
interpretFile filename = openFile filename ReadMode >>= hGetContents >>= srcToDef

srcToDef :: String -> Definition
srcToDef src = kDefToGlobalDef <$> parse src
  where
    parse src =
        case parseKoak src of
            Left errorMsg -> putStrLn errorMsg >> exitWith (ExitFailure 84)
            Right koakAst -> pure koakAst

llvmTestModule :: Definition -> AST.Module
llvmTestModule def = defaultModule {moduleName = "main", moduleDefinitions = [def]}

printLLVM :: AST.Module -> IO ()
printLLVM m =
    withContext $ \ctx -> do
        llvm <- withModuleFromAST ctx m moduleLLVMAssembly
        BS.putStrLn llvm
--            withModuleFromAST ctx llvmTestMain (writeBitcodeToFile $ File "test.ll")
--            withModuleFromAST ctx llvmTestOp (writeBitcodeToFile $ File "op.ll")
