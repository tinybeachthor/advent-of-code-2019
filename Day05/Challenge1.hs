import qualified Day05.Input (get)
import Day05.Computer
  (ProgramCounter, Computer(..), MemoryInterface, stepIO)
import Day05.Intcode

main :: IO ()
main = do
  memory <- Day05.Input.get "Day05/input"
  putStrLn . show $ 0
  putStrLn . show $ memory
  final <- runIO [1] (intCodeMemory memory) 0
  putStrLn . show $ final

runIO :: [Int] -> MemoryInterface a -> ProgramCounter -> IO ([Int])
runIO input code pc = do
  let io = (input,[])
  (io', code', pc', end) <- stepIO code pc io
  if end then return (snd io') else do
    output <- runIO (fst io') code' pc'
    return $ (snd io') ++ output
