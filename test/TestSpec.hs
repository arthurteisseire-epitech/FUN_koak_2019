module TestSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test" $
    it "test spec lib" $
        1 `shouldBe` 1
