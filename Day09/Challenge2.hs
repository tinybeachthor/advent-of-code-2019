import qualified Day09.Input (get)
import Common.IntegerComputer
import Day09.Intcode (step)

main = do
  code <- Day09.Input.get "Day09/input"
  let state = ComputerState (0, code) ([2], []) 0
  (_, state') <- runComputerM (intComputer step) state
  -- putStrLn . show $ state'
  let (i,o) = _io state'
  putStrLn . show $ o
