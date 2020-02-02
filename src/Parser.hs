module Parser where

import           Control.Applicative          (Alternative, (<|>))
import           Control.Monad
import           Data.Char
import           KoakAST
import           ParseUtils
import           Text.ParserCombinators.ReadP

applyParser :: ReadP a -> String -> Either String a
applyParser parser s
    | null res = Left "Any Error"
    | otherwise = Right . fst . last $ res
  where
    res = readP_to_S parser s

parsePrototypeArgs :: ReadP KPrototypeArgs
parsePrototypeArgs = do
    char '('
    arg <- parsePrototypeArg
    string "):"
    returnType <- parseType
    return $ KPrototypeArgs [arg] returnType

parsePrototypeArg :: ReadP KPrototypeArg
parsePrototypeArg = do
    identifier <- parseIdentifier
    char ':'
    argType <- parseType
    return $ KPrototypeArg identifier argType

parseType :: ReadP KType
parseType =
    (string "int" >> return KIntType) <|>
    (string "double" >> return KDoubleType) <|>
    (string "void" >> return KVoidType)

parseExpressions :: ReadP KExpressions
parseExpressions = parseListExpr -- TODO : Add '<|> parseFor <|> parseIf <|> parseWhile'

parseListExpr :: ReadP KExpressions
parseListExpr = do
    e <- parseExpression
    (KListExpr suffix) <- option (KListExpr []) parseListExprSuffix
    return $ KListExpr (e : suffix)

parseListExprSuffix :: ReadP KExpressions
parseListExprSuffix = do
    char ':'
    e <- parseExpression
    (KListExpr s) <- option (KListExpr []) parseListExprSuffix
    return $ KListExpr (e : s)

parseExpression :: ReadP KExpression
parseExpression = do
    u <- parseUnary
    m <- option Nothing parseExpressionSuffix
    return (KExpression u m)

parseExpressionSuffix :: ReadP (Maybe (KBinOp, KExpression))
parseExpressionSuffix = do
    c <- oneOf "+-*/"
    e <- parseExpression
    (return . Just) ([c], e)

parseUnary :: ReadP KUnary
parseUnary =
    (do u <- oneOf "!-"
        KUnOpUnary [u] <$> parseUnary) <|>
    (KPostfix <$> parsePostfix)

parsePostfix :: ReadP KPostfix
parsePostfix = do
    p <- parsePrimary
    (KFuncCall p <$> parseCallExpr) <|> return (KPrimary p)

parseCallExpr :: ReadP KCallExpr
parseCallExpr = do
    char '('
    expr <- parseExpression
    expressions <- option [] parseCallExprSuffix
    char ')'
    return . KCallExpr $ expr : expressions

parseCallExprSuffix :: ReadP [KExpression]
parseCallExprSuffix = do
    char ','
    expr <- parseExpression
    p <- option [] parseCallExprSuffix
    return $ expr : p

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
parseLiteral = (KDoubleConst . readDouble <$> double) <|> (KDecimalConst . readInt <$> integer)

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read
