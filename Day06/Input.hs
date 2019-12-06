module Day06.Input where

import Common.Parser
import Common.ParserPlus

import Control.Applicative
import Data.List

getPlanet :: Orbit -> String
getPlanet (Orbit p _) = p

getSatellite :: Orbit -> String
getSatellite (Orbit _ s) = s

data Orbit = Orbit String String
  deriving (Show, Eq)

get :: String -> IO [Orbit]
get filename = do
  input <- readFile filename
  return $ runParser parser input

parser :: Parser [Orbit]
parser = some orbitParser

orbitParser :: Parser Orbit
orbitParser = do
  l <- line
  let (p,c) = break (')' ==) l
  return $ Orbit p (tail c)
