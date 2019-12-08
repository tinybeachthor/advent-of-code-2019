module Day08.Input where

import Common.Parser
import Common.ParserPlus

import Control.Applicative

type Layer = [[Int]]

load :: String -> Int -> Int -> IO [Layer]
load filename x y = do
  input <- readFile filename
  return $ runParser (parser x y) input

parser :: Int -> Int -> Parser [Layer]
parser x y = some $ layerParser x y

layerParser :: Int -> Int -> Parser Layer
layerParser x 0 = return []
layerParser x y = do
  r <- rowParser x
  rs <- layerParser x (y-1)
  spaces
  return $ r:rs

rowParser :: Int -> Parser [Int]
rowParser 0 = return []
rowParser x = do
  d <- (\c -> read [c]) <$> digit
  ds <- rowParser (x-1)
  return $ d:ds
