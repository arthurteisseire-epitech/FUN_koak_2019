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
    (string "int" >> return KIntType) <|>
    (string "double" >> return KDoubleType) <|>
    (string "void" >> return KVoidType)

parseExpressions :: ReadP KExpressions
parseExpressions = parseListExpr -- TODO : Add '<|> parseFor <|> parseIf <|> parseWhile'

parseListExpr :: ReadP KExpressions
parseListExpr = KListExpr <$> sepBy1 parseExpression (char ':')

parseExpression :: ReadP KExpression
parseExpression = do
    u <- parseUnary
    m <- option [] parseExpressionSuffix
    return (KExpression u m)

parseExpressionSuffix :: ReadP [(KBinOp, KUnary)]
parseExpressionSuffix = do
    c <- parseBinOp
    u <- parseUnary
    e <- option [] parseExpressionSuffix
    return $ (c, u) : e

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
parseLiteral = (KDoubleConst . readDouble <$> double) <|> (KDecimalConst . readInt <$> integer)

parseBinOp :: ReadP KBinOp
parseBinOp = (string "+" >> return KBinOpPlus) <|>
             (string "-" >> return KBinOpLess) <|>
             (string "*" >> return KBinOpMul) <|>
             (string "/" >> return KBinOpDiv)

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read
