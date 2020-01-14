module ParserSpec where

import Test.Hspec
import KoakAST

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test parser" $
    it "test parse int" $
        KDoubleConst 2 `shouldBe` KDoubleConst 2
