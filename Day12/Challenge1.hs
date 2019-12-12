import Day12.Input
  (Position(..), load)

import Control.Monad

main = do
  positions <- load "Day12/input"
  let moons = initMoon <$> positions
  let nSteps = 1000
  moons' <- simulateSteps nSteps moons
  mapM (putStrLn . show) moons'
  let e = sum $ energy <$> moons'
  putStrLn . show $ e
  return ()

simulateSteps :: Int -> [Moon] -> IO [Moon]
simulateSteps 0 moons = return moons
simulateSteps n moons = do
  -- mapM (putStrLn . show) moons
  let moons' = step moons
  -- mapM (putStrLn . show) moons'
  simulateSteps (n-1) moons'

energy :: Moon -> Int
energy (Position x y z, Position u v w) =
  let pot = (abs x) + (abs y) + (abs z)
      kin = (abs u) + (abs v) + (abs w)
   in pot * kin

type Moon = (Position, Position)

zeroVelocity = Position 0 0 0

initMoon :: Position -> Moon
initMoon p = (p, zeroVelocity)

step :: [Moon] -> [Moon]
step ms =
  let eachWithRest = gen1toN ms
      ms' = (\(m,ms) -> applyGravity m ms) <$> eachWithRest
   in applyVelocity <$> ms'

applyGravity :: Moon -> [Moon] -> Moon
applyGravity x []     = x
applyGravity x (m:ms) = applyGravity (gravity x m) ms
  where

    gravity :: Moon -> Moon -> Moon
    gravity (p@(Position a b c), (Position u v w)) ((Position x y z), _) =
      let x' = u + velocityDiff a x
          y' = v + velocityDiff b y
          z' = w + velocityDiff c z
      in (p, Position x' y' z')

    velocityDiff :: Int -> Int -> Int
    velocityDiff a b = case compare a b of
                         GT -> -1
                         EQ ->  0
                         LT ->  1

applyVelocity :: Moon -> Moon
applyVelocity (Position x y z, vv@(Position u v w)) =
  (Position (x + u) (y + v) (z + w), vv)

-- Helpers

gen1toN :: [a] -> [(a,[a])]
gen1toN [] = []
gen1toN (a:as) = (a, as) : ((\(b, bs) -> (b, a:bs)) <$> (gen1toN as))
