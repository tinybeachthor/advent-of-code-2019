import Day10.Input

import Data.Sort

main = do
  field <- load "Day10/input"
  let base = (22, 19)
  let others = filter (base /=) field
  let angled = (\a -> (angle base a, [a])) <$> others
  let ordered = sortOn fst angled
  putStrLn . show $ ordered
  let reduced = (sortOn $ euclidean base) <$> foldSame ordered
  putStrLn . show $ reduced
  let shot = zip [1,2 ..] (unwrap reduced)
  mapM_ (putStrLn . show) shot

unwrap :: [[Asteroid]] -> [Asteroid]
unwrap [] = []
unwrap as =
  let fulls = filter ([] /=) as
      splits = firstsAndRests fulls
      firsts = fst <$> splits
      rests = snd <$> splits
   in firsts ++ (unwrap rests)

firstsAndRests :: [[a]] -> [(a,[a])]
firstsAndRests [] = []
firstsAndRests xss = [(head xs, tail xs) | xs <- xss]

foldSame :: [(Double, [Asteroid])] -> [[Asteroid]]
foldSame [] = []
foldSame ((a,as):(b,bs):rest) =
  if a == b
     then foldSame $ (a, as ++ bs) : rest
     else as : (foldSame $ (b,bs) : rest)
foldSame (a:[]) = (snd a):[]

angle :: Asteroid -> Asteroid -> Double
angle (a, b) (x, y) =
  let u = fromIntegral $ x - a
      v = fromIntegral $ y - b
   in 0 - atan2 u v

euclidean :: (Int,Int) -> (Int,Int) -> Int
euclidean (a,b) (x,y) = (a-x)*(a-x) + (b-y)*(b-y)
