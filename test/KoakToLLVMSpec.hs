module KoakToLLVMSpec where

import           KoakAST
import           KoakToLLVM
import           Test.Hspec

import           LLVM.AST                   as AST
import           LLVM.AST.Constant          as C

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "koak to llvm" $
    it "test 1-2" $
    koakToLLVM
        (KExpression
             ((KPostfix . KPrimary . KLiteral . KDecimalConst) 1)
             [(KBinOpLess, (KPostfix . KPrimary . KLiteral . KDecimalConst) 2)]) `shouldBe`
    AST.FSub AST.noFastMathFlags (ConstantOperand (C.Int 32 1)) (ConstantOperand (C.Int 32 2)) []
