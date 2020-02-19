module Parser
    ( parseKoak
    ) where

import           Control.Applicative          (Alternative, (<|>))
import           Control.Monad
import           Data.Char
import           KoakAST
import           ParseUtils
import           Text.ParserCombinators.ReadP

parseKoak :: String -> Either String KStmt
parseKoak = applyParser parseStmt

applyParser :: ReadP a -> String -> Either String a
applyParser parser s
    | null res = Left "Syntax error"
    | otherwise = Right . fst . last $ res
  where
    res = readP_to_S parser s

parseStmt :: ReadP KStmt
parseStmt = do
    stmt <- sepBy1 parseKDefs ((many . oneOf) " \n")
    return $ KStmt stmt

parseKDefs :: ReadP KDefs
parseKDefs = (string "def " *> parseDefs <* char ';') <|> (KExpressions <$> parseExpressions <* char ';')

parseDefs :: ReadP KDefs
parseDefs = do
    prototype <- parsePrototype
    char ' '
    expressions <- parseExpressions
    return $ KDefs prototype expressions

parsePrototype :: ReadP KPrototype
parsePrototype = do
    identifier <- parseIdentifier
    args <- parsePrototypeArgs
    return $ KPrototype identifier args

parsePrototypeArgs :: ReadP KPrototypeArgs
parsePrototypeArgs = do
    char '('
    args <- sepBy parsePrototypeArg (char ' ')
    string "):"
    returnType <- parseType
    return $ KPrototypeArgs args returnType

parsePrototypeArg :: ReadP KPrototypeArg
parsePrototypeArg = do
    identifier <- parseIdentifier
    char ':'
    argType <- parseType
    return $ KPrototypeArg identifier argType

parseType :: ReadP KType
parseType =
    (string "int" >> return KIntType) <|> (string "double" >> return KDoubleType) <|>
    (string "void" >> return KVoidType)

parseExpressions :: ReadP KExpressions
parseExpressions = parseListExpr <|> parseIfElse <|> parseIf <|> parseWhile <|> parseFor

parseFor :: ReadP KExpressions
parseFor = do
    string "for "
    assign <- parseExpression
    char ','
    condition <- parseExpression
    char ','
    increment <- parseExpression
    string " in "
    inExpr <- parseExpressions
    return $ KFor assign condition increment inExpr

parseWhile :: ReadP KExpressions
parseWhile = do
    string "while "
    condition <- parseExpression
    string " do "
    doExpr <- parseExpressions
    return $ KWhile condition doExpr

parseIfElse :: ReadP KExpressions
parseIfElse = do
    (condition, thenExpr) <- parseIfThen
    string " else "
    elseExpr <- parseExpressions
    return $ KIfElse condition thenExpr elseExpr

parseIf :: ReadP KExpressions
parseIf = do
    (condition, thenExpr) <- parseIfThen
    return $ KIf condition thenExpr

parseIfThen :: ReadP (KExpression, KExpressions)
parseIfThen = do
    string "if "
    condition <- parseExpression
    string " then "
    thenExpr <- parseExpressions
    return (condition, thenExpr)

parseListExpr :: ReadP KExpressions
parseListExpr = KListExpr <$> sepBy1 parseExpression (char ':')

parseExpression :: ReadP KExpression
parseExpression = do
    (u, m) <- sepByPair parseUnary parseBinOp
    return (KExpression u m)

parseUnary :: ReadP KUnary
parseUnary =
    (do u <- parseUnOp
        KUnOpUnary u <$> parseUnary) <|>
    (KPostfix <$> parsePostfix)

parsePostfix :: ReadP KPostfix
parsePostfix = do
    p <- parsePrimary
    (KFuncCall p <$> parseCallExpr) <|> return (KPrimary p)

parseCallExpr :: ReadP KCallExpr
parseCallExpr = do
    char '('
    expressions <- sepBy1 parseExpression (char ',')
    char ')'
    return . KCallExpr $ expressions

parsePrimary :: ReadP KPrimary
parsePrimary =
    (KIdentifier <$> parseIdentifier) <|> (KLiteral <$> parseLiteral) <|>
    (do char '('
        exprs <- parseExpressions
        char ')'
        return (KPrimaryExpressions exprs))

parseIdentifier :: ReadP KIdentifier
parseIdentifier = satisfy isAlpha <:> munch isAlphaNum

parseLiteral :: ReadP KLiteral
parseLiteral = (KDoubleConst . readDouble <$> checkParseDouble) <|> (KDecimalConst . readInt <$> checkParseInt)

parseBinOp :: ReadP KBinOp
parseBinOp =
    (string "+" >> return KBinOpPlus) <|> (string "-" >> return KBinOpLess) <|> (string "*" >> return KBinOpMul) <|>
    (string "/" >> return KBinOpDiv) <|>
    (string "<" >> return KBinOpInf) <|>
    (string "=" >> return KBinOpAssign)

parseUnOp :: ReadP KUnOp
parseUnOp = (string "!" >> return KUnOpNot) <|> (string "-" >> return KUnOpLess)

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read
