module Common.ParserPlus where

import Common.Parser

import Data.Char
import Control.Applicative
import Control.Monad

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c
     then return c
     else failure

orNot :: (a -> Parser a) -> a -> Parser a
orNot f a = f a <|> return a

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

char :: Char -> Parser Char
char c = satisfy (c ==)

chainl :: (Semigroup a) => Parser a -> a -> Parser a
chainl p a = do { a <- p; rest a } <|> return a
  where rest a = do { b <- p; rest (a <> b) } <|> return a

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

line :: Parser String
line = do { line <- some item; oneOf "\n\r"; return line }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a}

reserved :: String -> Parser String
reserved s = token (string s)

white :: Parser Char
white = oneOf " \n\r"

spaces :: Parser [Char]
spaces = many white

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
