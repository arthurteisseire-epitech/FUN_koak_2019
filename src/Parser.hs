module Parser where

import KoakAST
import Text.ParserCombinators.ReadP
import ParseUtils
import Control.Applicative ((<|>))

applyParser :: ReadP (Either String a) -> String -> Either String a
applyParser parser s
    | null res = Left "Any Error"
    | otherwise = fst . last $ res
  where
    res = readP_to_S parser s

parseNumber :: ReadP (Either String KLiteral)
parseNumber = Right <$> ((KDoubleConst . rDouble <$> double) <|> (KDecimalConst . rInt <$> integer))
  where
    rDouble = read :: String -> Double
    rInt = read :: String -> Int
