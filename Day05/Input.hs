module Day05.Input (get) where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

get :: String -> IO ([Int])
get filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Int]
parser = some $ do {i <- number; char ',' <|> white; return i}
