{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LLVM.AST              as AST

import           LLVM.Context
import           LLVM.Module

import qualified Data.ByteString.Char8 as BS

import           System.Environment
import           System.Exit
import           System.IO

import           KoakToLLVM
import           Parser

main :: IO ()
main = getArgs >>= mapM interpretFile >>= mapM_ (printLLVM . llvmTestModule)

interpretFile :: String -> IO [Definition]
interpretFile filename = openFile filename ReadMode >>= hGetContents >>= eitherToIO . srcToDef

srcToDef :: String -> Either String [Definition]
srcToDef s = koakToLLVM <$> parseKoak s

llvmTestModule :: [Definition] -> AST.Module
llvmTestModule def = defaultModule {moduleName = "main", moduleDefinitions = def}

printLLVM :: AST.Module -> IO ()
printLLVM m =
    withContext $ \ctx -> do
        llvm <- withModuleFromAST ctx m moduleLLVMAssembly
        BS.putStrLn llvm
--            withModuleFromAST ctx llvmTestMain (writeBitcodeToFile $ File "test.ll")
--            withModuleFromAST ctx llvmTestOp (writeBitcodeToFile $ File "op.ll")

eitherToIO :: Either String a -> IO a
eitherToIO = either dieWithMsg pure

dieWithMsg :: String -> IO a
dieWithMsg s = putStrLn s >> exitWith (ExitFailure 84)
