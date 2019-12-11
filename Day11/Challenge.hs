import Day09.Input (get)
import Common.IntegerComputer
import Day09.Intcode (step)

import Data.Set (fromList)

main = do
  program <- get "Day11/input"
  -- putStrLn . show $ program
  let state = ComputerState (0, program) ([1], []) 0
  painted <- runRobot state [] (0,0) U
  -- putStrLn . show $ painted
  let positions = snd <$> painted
  putStrLn . show . length $ fromList positions

  -- get dimensions
  let maxX = maximum $ fst <$> positions
  let maxY = maximum $ snd <$> positions
  let minX = minimum $ fst <$> positions
  let minY = minimum $ snd <$> positions
  putStrLn . show $ (minX, maxX)
  putStrLn . show $ (minY, maxY)
  -- normalize
  let normalized = (\(c,(x,y)) -> (c,(x, y + (maxY-minY)))) <$> painted
  putStrLn . show $ normalized
  --render
  render (maxY-minY) (maxX-minX) normalized

render :: Integer -> Integer -> [Painted] -> IO ()
render (-1) _ ps = return ()
render   y  x ps = do
  putStrLn $ renderRow y x x ps
  render (y-1) x ps

renderRow :: Integer -> Integer -> Integer -> [Painted] -> String
renderRow _ (-1) maxX ps = []
renderRow y   x  maxX ps =
  let c = case getPositionColor ps (maxX - x,y) of
            1 -> '#'
            _ -> ' '
   in c : (renderRow y (x-1) maxX ps)

type Position = (Integer,Integer)
data Orientation = U | L | D | R
type Painted = (Integer, Position)

runRobot :: ComputerState -> [Painted] -> Position -> Orientation -> IO [Painted]
runRobot state painted p o = do
  (mColor, state')  <- runComputerM (interruptibleComputer step) state
  case mColor of
    Nothing    -> return painted
    Just color -> do
      (mTurn,  state'') <- runComputerM (interruptibleComputer step) state'
      case mTurn of
        Nothing   -> return painted
        Just turn -> do
          let painted' = (color, p) : painted
          let o' = if turn == 0 then turnLeft o else turnRight o
          let p' = oneForward p o'
          let state''' = appendInput state'' (getPositionColor painted' p')
          runRobot state''' painted' p' o'

getPositionColor :: [Painted] -> Position -> Integer
getPositionColor []         _ = 0
getPositionColor ((c,p):ps) x =
  if x == p then c else getPositionColor ps x

-- Helpers

turnLeft :: Orientation -> Orientation
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Orientation -> Orientation
turnRight U = R
turnRight L = U
turnRight D = L
turnRight R = D

oneForward :: Position -> Orientation -> Position
oneForward (x,y) U = (x,y+1)
oneForward (x,y) L = (x-1,y)
oneForward (x,y) D = (x,y-1)
oneForward (x,y) R = (x+1,y)

appendInput :: ComputerState -> Integer -> ComputerState
appendInput s a =
  let (i,o) = _io s
   in ComputerState (_memory s) (i ++ [a], o) (_base s)
