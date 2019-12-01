module Day01.Input
  ( get
  ) where

import Common.Parser
import Common.ParserPlus

get :: String -> IO ([Integer])
get filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Integer]
parser = chainl p []
  where
    p = do
      mass <- natural
      spaces
      return . pure $ mass
