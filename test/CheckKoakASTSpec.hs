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
    describe "identifier error" $
        it "call unexsting function call" $ do
            parseAndCheckKoak "add(1,2);" `shouldSatisfy` isLeft

    -- TODO : describe "type error"
    -- TODO : describe " error"

parseAndCheckKoak :: String -> Either String KStmt
parseAndCheckKoak s = parseKoak s >>= checkKoakAST
