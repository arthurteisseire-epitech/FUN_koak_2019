module CheckKoakASTSpec where

import           Data.Either (isLeft, isRight)
import           Parser      (parseKoak)
import CheckKoakAST
import           Test.Hspec
import KoakAST (KStmt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "identifier error" $ do
        it "call function without prototype" $
            parseAndCheckKoak "add(1,2);" `shouldSatisfy` isLeft
        it "call function with a mismatch prototype" $
            parseAndCheckKoak "add(1,2); def sub(n1:int n2:int):int n1-n2;" `shouldSatisfy` isLeft
        it "call function with a match prototype on the second position" $
            parseAndCheckKoak "add(1,2); def sub(n1:int n2:int):int n1-n2; def add(n1:int n2:int):int n1+n2;" `shouldSatisfy` isRight

    -- TODO : describe "prototype mismatch with good identifier"
    -- TODO : describe "type error"
    -- TODO : describe " error"

parseAndCheckKoak :: String -> Either String KStmt
parseAndCheckKoak s = parseKoak s >>= checkKoakAST
