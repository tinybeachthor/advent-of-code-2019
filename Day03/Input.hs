module Day03.Input (get, Segment(..), Wire) where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

data Segment
  = U Int
  | D Int
  | L Int
  | R Int
  deriving (Show, Eq)

type Wire = [Segment]

get :: String -> IO ([Wire])
get filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Wire]
parser = some $
  wireParser >>= \wire -> spaces >> return wire

wireParser :: Parser Wire
wireParser = some $ do
  segmentParser >>= \segment -> orNot char ',' >> return segment

segmentParser :: Parser Segment
segmentParser = do
  heading <- item
  length <- fromIntegral <$> natural
  return $ case heading of
             'U' -> U
             'D' -> D
             'R' -> R
             'L' -> L
             _ -> error "Invalid segment heading."
         $ length
