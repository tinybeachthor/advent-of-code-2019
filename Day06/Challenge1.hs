import System.IO

import Day06.Input

main :: IO ()
main = do
  raw <- get "Day06/input"
  -- putStrLn . show $ raw
  let tree = orbitTree raw "COM"
  -- putStrLn . show $ tree
  putStrLn . show . sum $ depths 0 tree

getInOrbit :: String -> [Orbit] -> [Orbit]
getInOrbit _ [] = []
getInOrbit p (o@(Orbit p' _):os) =
  if p == p'
     then o : getInOrbit p os
     else getInOrbit p os

data OrbitTree = OrbitTree String [OrbitTree]
  deriving Show

orbitTree :: [Orbit] -> String -> OrbitTree
orbitTree os s =
  let sats = getSatellite <$> getInOrbit s os
      cs = orbitTree os <$> sats
   in OrbitTree s $ cs

depths :: Int -> OrbitTree -> [Int]
depths d (OrbitTree _ cs) =
  let sats = concat $ (depths (d+1)) <$> cs
   in d:sats
