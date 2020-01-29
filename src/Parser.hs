module Parser where

import           Control.Applicative          (liftA2, (<|>))
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
parsePostfix = KPrimary <$> parsePrimary -- TODO : add 'call_expr?'

parsePrimary :: ReadP KPrimary
parsePrimary = (KIdentifier <$> parseIdentifier) <|> (KLiteral <$> parseLiteral) -- TODO : add '<|> expressionsParser'

parseIdentifier :: ReadP KIdentifier
parseIdentifier = satisfy isAlpha <:> munch isAlphaNum

parseLiteral :: ReadP KLiteral
parseLiteral = (KDoubleConst . rDouble <$> double) <|> (KDecimalConst . rInt <$> integer)
  where
    rDouble = read :: String -> Double
    rInt = read :: String -> Int
