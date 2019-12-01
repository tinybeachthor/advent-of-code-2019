import System.IO

import qualified Day01.Input

main :: IO ()
main = do
  masses <- Day01.Input.get "Day01/input"
  let fuels = calcFuel <$> masses
  let totalFuel = foldl (+) 0 fuels
  putStr . show $ totalFuel

calcFuel :: Integer -> Integer
calcFuel mass = (quot mass 3) - 2
