module ParseUtils where

import           Control.Applicative
import           Data.Char
import           Text.ParserCombinators.ReadP

double :: ReadP String
double = integer <++> decimalPart
  where
    decimalPart = char '.' <:> number

integer :: ReadP String
integer = plus <|> minus <|> number

number :: ReadP String
number = munch1 isDigit

plus :: ReadP String
plus = char '+' *> number

minus :: ReadP String
minus = char '-' <:> number

(<++>) :: ReadP String -> ReadP String -> ReadP String
(<++>) = liftA2 (++)

(<:>) :: ReadP Char -> ReadP String -> ReadP String
(<:>) a b = fmap (:) a <*> b

oneOf :: String -> ReadP Char
oneOf s = satisfy (`elem` s)
