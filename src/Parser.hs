module Parser where

import KoakAST
import Text.ParserCombinators.ReadP
import ParseUtils
import Control.Applicative ((<|>))
import Control.Monad
import Data.Char

applyParser :: ReadP a -> String -> Either String a
applyParser parser s
    | null res = Left "Any Error"
    | otherwise = Right . fst . last $ res
  where
    res = readP_to_S parser s

parsePostfix :: ReadP KPostfix
parsePostfix = KPrimary <$> parsePrimary

parsePrimary :: ReadP KPrimary
parsePrimary = (KIdentifier <$> parseIdentifier) <|> (KLiteral <$> parseLiteral) -- TODO : add '<|> expressionsParser'

parseIdentifier :: ReadP KIdentifier
parseIdentifier = satisfy isAlpha <:> munch isAlphaNum

parseLiteral :: ReadP KLiteral
parseLiteral = (KDoubleConst . rDouble <$> double) <|> (KDecimalConst . rInt <$> integer)
  where
    rDouble = read :: String -> Double
    rInt = read :: String -> Int
