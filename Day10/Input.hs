module Day10.Input where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

type Asteroid = (Int, Int)

load :: String -> IO [Asteroid]
load filename = do
  input <- readFile filename
  let parsed = runParser parser input
  return $ getAsteroids parsed 0

getAsteroids :: [[Char]] -> Int -> [Asteroid]
getAsteroids []     _ = []
getAsteroids (r:rs) y =
  (getRowAsteroid y r 0) ++ getAsteroids rs (y+1)

getRowAsteroid :: Int -> [Char] -> Int -> [Asteroid]
getRowAsteroid _ []     _ = []
getRowAsteroid y (a:as) x
  | a == '#'  = (x,y) : getRowAsteroid y as (x+1)
  | otherwise = getRowAsteroid y as (x+1)

type Field = [[Char]]

parser :: Parser Field
parser = some $ rowParser

rowParser :: Parser [Char]
rowParser = do
  r <- some $ char '.' <|> char '#'
  spaces
  return $ r
