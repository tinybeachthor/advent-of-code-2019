import System.IO

import qualified Day01.Input

main :: IO ()
main = do
  masses <- Day01.Input.get "Day01/input"
  let fuels = calcFuel <$> masses
  let totalFuel = foldl (+) 0 fuels
  putStr . show $ totalFuel

calcFuel :: Integer -> Integer
calcFuel mass =
  let fuel = massToFuel mass
   in fuel + if fuel > 0
                then calcFuel fuel
                else 0

massToFuel :: Integer -> Integer
massToFuel mass =
  let fuel = (quot mass 3) - 2
   in if fuel > 0 then fuel else 0
