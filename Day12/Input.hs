module Day12.Input where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

data Position = Position
  { _x :: Int
  , _y :: Int
  , _z :: Int
  }

instance Show Position where
  show p = "<x="  ++ (show $ _x p)
        ++ ", y=" ++ (show $ _y p)
        ++ ", z=" ++ (show $ _z p)
        ++ ">"

load :: String -> IO [Position]
load filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Position]
parser = some $
  positionParser >>= \position -> spaces >> return position

positionParser :: Parser Position
positionParser = do
  reserved "<x="
  x <- number
  reserved ", y="
  y <- number
  reserved ", z="
  z <- number
  char '>'
  return $ Position x y z
