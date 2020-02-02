module ParseUtils where

import           Control.Applicative          ((<|>), liftA2)
import           Control.Monad                (liftM2)
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

(<:>) :: ReadP a -> ReadP [a] -> ReadP [a]
(<:>) a b = fmap (:) a <*> b

oneOf :: String -> ReadP Char
oneOf s = satisfy (`elem` s)

sepByPair :: ReadP a -> ReadP sep -> ReadP (a, [(sep, a)])
sepByPair parser sep = do
    p <- parser
    h <- sepByPairHelper parser sep <|> return []
    return (p, h)

sepByPairHelper :: ReadP a -> ReadP sep -> ReadP [(sep, a)]
sepByPairHelper parser sep = do
    s <- sep
    p <- parser
    e <- (sepByPairHelper parser sep) <|> return []
    return $ (s, p) : e
