import qualified Day05.Input (get)
import Common.IntComputer
import Day05.Intcode (step)

main = do
  code <- Day05.Input.get "Day05/input"
  let state = ComputerState (0, code) ([5], [])
  (_, state') <- runComputerM (intComputer step) state
  let (i,o) = _io state'
  putStrLn . show $ o
