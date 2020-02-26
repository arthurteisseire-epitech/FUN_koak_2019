module Parser
    ( parseKoak
    ) where

import           Control.Applicative          ((<|>))
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
    _ <- char ' '
    expressions <- parseExpressions
    return $ KDefs prototype expressions

parsePrototype :: ReadP KPrototype
parsePrototype = do
    identifier <- parseIdentifier
    args <- parsePrototypeArgs
    return $ KPrototype identifier args

parsePrototypeArgs :: ReadP KPrototypeArgs
parsePrototypeArgs = do
    _ <- char '('
    args <- sepBy parsePrototypeArg (char ' ')
    _ <- string "):"
    returnType <- parseType
    return $ KPrototypeArgs args returnType

parsePrototypeArg :: ReadP KPrototypeArg
parsePrototypeArg = do
    identifier <- parseIdentifier
    _ <- char ':'
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
    _ <- string "for "
    assign <- parseExpression
    _ <- char ','
    condition <- parseExpression
    _ <- char ','
    increment <- parseExpression
    _ <- string " in "
    inExpr <- parseExpressions
    return $ KFor assign condition increment inExpr

parseWhile :: ReadP KExpressions
parseWhile = do
    _ <- string "while "
    condition <- parseExpression
    _ <- string " do "
    doExpr <- parseExpressions
    return $ KWhile condition doExpr

parseIfElse :: ReadP KExpressions
parseIfElse = do
    (condition, thenExpr) <- parseIfThen
    _ <- string " else "
    elseExpr <- parseExpressions
    return $ KIfElse condition thenExpr elseExpr

parseIf :: ReadP KExpressions
parseIf = do
    (condition, thenExpr) <- parseIfThen
    return $ KIf condition thenExpr

parseIfThen :: ReadP (KExpression, KExpressions)
parseIfThen = do
    _ <- string "if "
    condition <- parseExpression
    _ <- string " then "
    thenExpr <- parseExpressions
    return (condition, thenExpr)

parseListExpr :: ReadP KExpressions
parseListExpr = KListExpr <$> sepBy1 parseExpression (char ':')

parseExpression :: ReadP KExpression
parseExpression = do
    (u, m) <- sepByPair parsePostfix parseBinOp
    return (KExpression u m)

parsePostfix :: ReadP KPostfix
parsePostfix = do
    p <- parsePrimary
    (KFuncCall p <$> parseCallExpr) <|> return (KPrimary p)

parseCallExpr :: ReadP KCallExpr
parseCallExpr = do
    _ <- char '('
    expressions <- sepBy1 parseExpression (char ',')
    _ <- char ')'
    return . KCallExpr $ expressions

parsePrimary :: ReadP KPrimary
parsePrimary =
    (KIdentifier <$> parseIdentifier) <|> (KLiteral <$> parseLiteral) <|>
    (do _ <- char '('
        exprs <- parseExpressions
        _ <- char ')'
        return (KPrimaryExpressions exprs))

parseIdentifier :: ReadP KIdentifier
parseIdentifier = satisfy isAlpha <:> munch isAlphaNum

parseLiteral :: ReadP KLiteral
parseLiteral = (KDoubleConst . read <$> checkParseDouble) <|> (KDecimalConst . read <$> checkParseInt)

parseBinOp :: ReadP KBinOp
parseBinOp =
    (string "+" >> return KBinOpPlus) <|> (string "-" >> return KBinOpLess) <|> (string "*" >> return KBinOpMul) <|>
    (string "/" >> return KBinOpDiv) <|>
    (string "<" >> return KBinOpInf) <|>
    (string "=" >> return KBinOpAssign)
