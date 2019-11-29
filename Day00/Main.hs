import Common.Parser
import Common.ParserPlus

import Control.Applicative
import Control.Monad

parser :: Parser [Char]
parser = do
  c <- item
  spaces
  d <- digit
  spaces
  if c == 'a'
     then return [d]
     else return ['X']

multiParser :: Parser [Char]
multiParser = chainl parser []

main = do
  putStrLn $ show . runParser multiParser $ "a 1"
  putStrLn $ show . runParser multiParser $ "b1 a2"
  putStrLn $ show . runParser multiParser $ "b1 a2 a3 b 4 c5"
