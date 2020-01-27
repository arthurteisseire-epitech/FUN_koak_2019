module Parser where

import           Control.Applicative          ((<|>))
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

parseUnary :: ReadP KUnary
parseUnary =
    (do u <- oneOf unOp
        KUnary [u] <$> parseUnary) <|>
    (KPostfix <$> parsePostfix)
  where
    unOp = "!-"

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
