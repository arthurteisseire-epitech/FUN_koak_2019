module ParserSpec where

import Test.Hspec
import KoakAST

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test parser" $
    it "test parse int" $
        KInt 2 `shouldBe` KInt 2
