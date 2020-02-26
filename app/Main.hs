{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LLVM.AST              as AST

import           LLVM.Context
import           LLVM.Module

import qualified Data.ByteString.Char8 as BS

import           System.Environment
import           System.Exit

import           Control.Monad ((>=>))
import           KoakToLLVM (koakToLLVM)
import           Parser (parseKoak)

main :: IO ()
main = getArgs >>= mapM_ printLLVMFromFilename

printLLVMFromFilename :: String -> IO ()
printLLVMFromFilename s = interpretFile s >>= llvmModuleToString . mkModule >>= BS.putStrLn

interpretFile :: String -> IO [Definition]
interpretFile = readFile >=> srcToDef

srcToDef :: String -> IO [Definition]
srcToDef s = koakToLLVM <$> parse s
  where
    parse = eitherToIO . parseKoak

mkModule :: [Definition] -> AST.Module
mkModule def = defaultModule {moduleName = "main", moduleDefinitions = def}

llvmModuleToString :: AST.Module -> IO BS.ByteString
llvmModuleToString m = withContext $ \ctx -> withModuleFromAST ctx m moduleLLVMAssembly

eitherToIO :: Either String a -> IO a
eitherToIO = either dieWithMsg pure

dieWithMsg :: String -> IO a
dieWithMsg s = putStrLn s >> exitWith (ExitFailure 84)
