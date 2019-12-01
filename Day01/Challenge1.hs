import System.IO

import Common.Parser
import Common.ParserPlus

main :: IO ()
main = do
  input <- readFile "Day01/input"
  let masses = runParser parser input
  let fuels = calcFuel <$> masses
  let totalFuel = foldl (+) 0 fuels
  putStr . show $ totalFuel

parser :: Parser [Integer]
parser = chainl p []
  where
    p = do
      mass <- natural
      spaces
      return . pure $ mass

calcFuel :: Integer -> Integer
calcFuel mass = (quot mass 3) - 2
