import System.IO

import Day06.Input

main :: IO ()
main = do
  raw <- get "Day06/input"
  -- putStrLn . show $ raw
  let tree = orbitTree raw "COM"
  -- putStrLn . show $ tree
  let paths = reverse <$> unwrap tree
  let santa = head $ filter (\(p:ps) -> p == "SAN") paths
  let you   = head $ filter (\(p:ps) -> p == "YOU") paths
  -- putStrLn . show $ santa
  -- putStrLn . show $ you
  let common = zip (reverse santa) (reverse you)
  let prefix = takeWhile (\(a,b) -> a == b) common
  let santa' = drop (length prefix) (reverse santa)
  let you' = drop (length prefix) (reverse you)
  putStrLn . show $ santa'
  putStrLn . show $ you'
  putStrLn . show $ length santa' + length you' - 2

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

unwrap :: OrbitTree -> [[String]]
unwrap (OrbitTree p []) = [[p]]
unwrap (OrbitTree p cs) = (p:) <$> (concat $ unwrap <$> cs)
