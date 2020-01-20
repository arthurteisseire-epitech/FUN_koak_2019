module ParserSpec where

import Test.Hspec
import KoakAST
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test parser" $ do
    it "test parse int" $
        applyParser parseNumber "2" `shouldBe` Right (KDecimalConst 2)
    it "test parse double" $
        applyParser parseNumber "2.0" `shouldBe` Right (KDoubleConst 2)
    it "test parse number error" $
        applyParser parseNumber "error" `shouldBe` Left errorMsg
