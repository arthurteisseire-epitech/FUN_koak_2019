module CheckKoakASTSpec where

import           Data.Either (isLeft)
import           Parser      (parseKoak)
import CheckKoakAST
import           Test.Hspec
import KoakAST (KStmt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "identifier error" $ do
        it "call function without prototype" $
            parseAndCheckKoak "add(1,2);" `shouldSatisfy` isLeft
        it "call function with a mismatch prototype" $
            parseAndCheckKoak "add(1,2); def sub(n1:int n2:int):int n1-n2;" `shouldSatisfy` isLeft

    -- TODO : describe "type error"
    -- TODO : describe " error"

parseAndCheckKoak :: String -> Either String KStmt
parseAndCheckKoak s = parseKoak s >>= checkKoakAST
