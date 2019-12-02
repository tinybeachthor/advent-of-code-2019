module Day02.Input
  ( get
  ) where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

get :: String -> IO ([Integer])
get filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Integer]
parser = some $ do {i <- natural; char ',' <|> white; return i}
