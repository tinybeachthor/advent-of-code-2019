import Day10.Input

import Data.Sort

main = do
  field <- load "Day10/input"
  let testSets = gen1toN field
  let visibleFrom = inLOS <$> testSets
  putStrLn . show $ visibleFrom

-- inLOS :: (Asteroid, [Asteroid]) -> (Asteroid, Int)
-- inLOS (a, as) =
--   let sorted = sortOn (euclidean a) as
--    in (a, countVisible a sorted)

-- removeHidden :: Asteroid -> Asteroid -> [Asteroid] -> [Asteroid]
-- removeHidden _ _ []     = []
-- removeHidden x a (b:bs) =
--   if intersect x a b
--      then removeHidden x a bs
--      else b : removeHidden x a bs

intersect :: Asteroid -> Asteroid -> Asteroid -> Bool
intersect (xx, xy) (ax, ay) (bx, by) =
  let (vx,vy) = (ax - xx, ay - xy)
      x = (fromIntegral (bx - xx)) / (fromIntegral vx)
      y = (fromIntegral (by - xy)) / (fromIntegral vy)
   in (x > 0 && y > 0 && x == y)
   || (vx == 0 && (bx - xx) == 0 && (ay - xy) * (by - xy) > 0)
   || (vy == 0 && (by - xy) == 0 && (ax - xx) * (bx - xx) > 0)

shadows :: Asteroid -> Asteroid -> [Asteroid] -> [Asteroid]
shadows _ _ [] = []
shadows x b (a:as) =
  if intersect x b a
     then a : shadows x b as
     else shadows x b as

-- Helpers

gen1toN :: [a] -> [(a,[a])]
gen1toN [] = []
gen1toN (a:as) = (a, as) : ((\(b, bs) -> (b, a:bs)) <$> (gen1toN as))

euclidean :: (Int,Int) -> (Int,Int) -> Int
euclidean (a,b) (x,y) = (a-x)*(a-x) + (b-y)*(b-y)
