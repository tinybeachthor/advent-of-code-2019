import qualified Day05.Input (get)
import Common.IntComputer
import Day05.Intcode (step)

main = do
  code <- Day05.Input.get "Day07/input"
  -- output <- amps code [4,3,2,1,0] 0
  output <- maxAmps code
  putStrLn . show $ output

type Phase = Int
type Input = Int
type Output = Int

maxAmps :: [Int] -> IO Int
maxAmps code = do
  let r = [0,1,2,3,4]
  let inputs = [[a,b,c,d,e] | a<-r, b<-r, c<-r, d<-r, e<-r]
  let valid = filter (\i -> all (\r' -> r' `elem` i) r) inputs
  outputs <- mapM (\i -> amps code i 0) valid
  return $ maximum outputs

amps :: [Int] -> [Phase] -> Input -> IO Output
amps code (p:ps) input = do
  output <- amp code p input
  if ps == []
     then return output
     else amps code ps output

amp :: [Int] -> Phase -> Input -> IO Output
amp code phase input = do
  let state = ComputerState (0, code) ([phase,input], [])
  (_, state') <- runComputerM (intComputer step) state
  -- putStrLn . show $ state'
  let (i,o) = _io state'
  return $ head o
