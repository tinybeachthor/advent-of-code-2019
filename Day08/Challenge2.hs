import Day08.Input
  (load)

import Data.List

main = do
  layers <- load "Day08/input" 25 6
  -- printLayer $ head layers
  let image = foldl1 foldLayers layers
  printLayer $ image

printLayer :: [[Int]] -> IO ()
printLayer l = do { printRow l; return () }
  where
    printRow [] = return ()
    printRow (r:rs) = do
      putStrLn $ printPixel <$> r
      printRow rs

    printPixel 2 = ' '
    printPixel 1 = 'X'
    printPixel 0 = '.'
    printPixel _ = '?'

foldLayers :: [[Int]] -> [[Int]] -> [[Int]]
foldLayers a b = zipWith combineRows a b

combineRows :: [Int] -> [Int] -> [Int]
combineRows a b =
  let c = zip a b
   in (\(x,y) -> combinePixels x y) <$> c

combinePixels :: Int -> Int -> Int
combinePixels 2 b = b
combinePixels a b = a
