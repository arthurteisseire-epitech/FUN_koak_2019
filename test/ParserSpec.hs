module ParserSpec where

import Test.Hspec
import KoakAST
import Parser
import Data.Either

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "test parse number" $ do
        it "test parse int" $
            applyParser parseNumber "2" `shouldBe` Right (KDecimalConst 2)
        it "test parse double" $
            applyParser parseNumber "2.0" `shouldBe` Right (KDoubleConst 2)
        it "test parse number error" $
            applyParser parseNumber "error" `shouldSatisfy` isLeft

    describe "test parse identifier" $ do
        it "test only alpha chars" $
            applyParser parseIdentifier "identifier" `shouldBe` Right "identifier"
        it "test numbers in identifier" $
            applyParser parseIdentifier "id3entifier2" `shouldBe` Right "id3entifier2"
        it "test number first" $
            applyParser parseIdentifier "2id" `shouldSatisfy` isLeft
