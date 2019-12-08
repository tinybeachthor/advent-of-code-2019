import qualified Day05.Input (get)
import Common.IntComputer
import Day05.Intcode (step)

main = do
  code <- Day05.Input.get "Day07/input"
  output <- maxAmps code
  putStrLn . show $ output

type Phase = Int
type Input = Int
type Output = Int

maxAmps :: [Int] -> IO Int
maxAmps code = do
  let amps  = setupAmps code
  let valid = validSettings
  outputs <- mapM (runAmps amps) valid
  return $ maximum outputs

runAmps :: [ComputerState] -> [Phase] -> IO Int
runAmps cs ps = do
  let cs' = zipWith (\s p -> appendInput s p) cs ps
  loopAmps cs' 0

loopAmps :: [ComputerState] -> Input -> IO Int
loopAmps cs input = do
  (cs', output) <- singleRunAmps cs input
  case output of
    Nothing -> return $ lastOutputOfLast cs'
    Just a  -> loopAmps cs' a

singleRunAmps :: [ComputerState] -> Int -> IO ([ComputerState], Maybe Int)
singleRunAmps []     a = return $ ([], Just a)
singleRunAmps (c:cs) a = do
  let c' = appendInput c a
  (c'', o) <- runAmp c'
  case o of
    Nothing -> return (c'' : cs, Nothing)
    Just b  -> do
      (cs', b') <- singleRunAmps cs b
      return (c'' : cs', b')

runAmp :: ComputerState -> IO (ComputerState, Maybe Int)
runAmp cs = do
  (a, cs') <- runComputerM (interruptibleComputer step) cs
  return (cs', a)

setupAmps :: [Int] -> [ComputerState]
setupAmps code =
  let c = ComputerState (0, code) ([], [])
   in c : (setupAmps code)

appendInput :: ComputerState -> Int -> ComputerState
appendInput s a =
  let (i,o) = _io s
   in ComputerState (_memory s) (i++[a], o)

validSettings :: [[Int]]
validSettings =
  let r = [5..9]
      inputs = [[a,b,c,d,e] | a<-r, b<-r, c<-r, d<-r, e<-r]
   in filter (\i -> all (\r' -> r' `elem` i) r) inputs

lastOutputOfLast :: [ComputerState] -> Int
lastOutputOfLast [] = -1
lastOutputOfLast (c:[]) =
  let (i,o) = _io c
   in head o
lastOutputOfLast (c:cs) = lastOutputOfLast cs
