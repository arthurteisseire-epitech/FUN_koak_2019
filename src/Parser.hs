module Parser where

import KoakAST
import Text.ParserCombinators.ReadP
import ParseUtils
import Control.Applicative ((<|>))

applyParser :: ReadP a -> String -> Either String a
applyParser parser s
    | null res = Left errorMsg
    | otherwise = Right . fst . last $ res
  where
    res = readP_to_S parser s

errorMsg :: String
errorMsg = "Any Error"

parseNumber :: ReadP KLiteral
parseNumber = (KDoubleConst . rDouble <$> double) <|> (KDecimalConst . rInt <$> integer)
  where
    rDouble = read :: String -> Double
    rInt = read :: String -> Int
