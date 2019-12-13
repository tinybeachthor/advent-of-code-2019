import Day09.Input (get)
import Common.IntegerComputer
import Day09.Intcode (step)

import Data.Set (fromList)

main = do
  program <- get "Day13/input"
  -- putStrLn . show $ program
  let state = ComputerState (0, program) ([], []) 0
  (_,  state') <- runComputerM (intComputer step) state
  -- putStrLn . show $ (snd . _io $ state')
  let objects = decodeOutput (snd . _io $ state')
  mapM (putStrLn . show) objects
  let blockPositions = fst <$> (filter (\(p,go) -> go == Block) objects)
  putStrLn . show . length $ fromList blockPositions

data Position = Position { _x :: Integer, _y :: Integer }
  deriving (Show, Eq, Ord)

data GameObject = Empty | Wall | Block | Paddle | Ball
  deriving (Show, Eq)

idToGameObject :: Integer -> GameObject
idToGameObject id = case id of
                      0 -> Empty
                      1 -> Wall
                      2 -> Block
                      3 -> Paddle
                      4 -> Ball
                      x -> error $ "Invalid object id : " ++ (show x)

decodeOutput :: [Integer] -> [(Position, GameObject)]
decodeOutput [] = []
decodeOutput (id:y:x:rest) =
  (Position x y, idToGameObject id) : (decodeOutput rest)
