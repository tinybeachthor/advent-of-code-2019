import Day08.Input
  (Layer, load)

import Data.List

main = do
  layers <- load "Day08/input" 25 6
  putStrLn . show $ layers
  let maxLayer = mostZeros layers
  putStrLn . show $ maxLayer
  let ones = countElemsLayer maxLayer 1
  let twos = countElemsLayer maxLayer 2
  putStrLn . show $ ones * twos

mostZeros :: [Layer] -> Layer
mostZeros ls =
  let ls' = zip (countElems ls 0) ls
      ml  = minimumBy (\(a, _) (b, _) -> compare a b) ls'
   in snd ml

countElems :: [Layer] -> Int -> [Int]
countElems []   _ = []
countElems (l:ls) x = (countElemsLayer l x) : (countElems ls x)

countElemsLayer :: Layer -> Int -> Int
countElemsLayer []     _ = 0
countElemsLayer (r:rs) x =
  let c = length $ filter (\e -> e == x) r
    in c + countElemsLayer rs x
