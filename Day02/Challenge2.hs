import System.IO

import qualified Day02.Input

main :: IO ()
main = do
  ops <- Day02.Input.get "Day02/input"
  let nvs = [(k,v) | k <- [0..99], v <- [0..99]]
  let inStates = (\f -> f ops) <$> (setNV <$> nvs)
  let outStates = (run 0) <$> inStates
  let results = zip ((flip get 0) <$> outStates) nvs
  let valid = filter (\(x, (k,v)) -> 19690720 == x) results
  putStrLn . show $ (head valid)

type OpCodes = [Integer]

setNV :: (Integer, Integer) -> OpCodes -> OpCodes
setNV (n,v) ops = update (update ops n 1) v 2

run :: Int -> OpCodes -> OpCodes
run i ops =
  let (ops', end) = step ops i
   in if end
         then ops'
         else run (i+4) ops'

step :: OpCodes -> Int -> (OpCodes, Bool)
step ops i = case get ops i of
  1 -> (doOp ops (+) i, False)
  2 -> (doOp ops (*) i, False)
  99 -> (ops, True)
  _ -> error "Unknown operation"

doOp :: OpCodes -> (Integer -> Integer -> Integer) -> Int -> OpCodes
doOp ops f i =
  let (aa,ab,r) = getNext3 ops i
      a = get ops (fromIntegral aa)
      b = get ops (fromIntegral ab)
   in update ops (f a b) r

getNext3 :: OpCodes -> Int -> (Integer, Integer, Integer)
getNext3 ops i =
  let a = get ops (i+1)
      b = get ops (i+2)
      c = get ops (i+3)
   in (a,b,c)

update :: OpCodes -> Integer -> Integer -> OpCodes
update ops v a =
  let (h, t) = splitAt (fromIntegral a) ops
   in h ++ [v] ++ (tail t)

get :: OpCodes -> Int -> Integer
get ops i = head . drop i $ ops
