import System.IO

import qualified Day02.Input

main :: IO ()
main = do
  ops <- Day02.Input.get "Day02/input"
  let outState = run (set1202 ops) 0
  putStrLn . show $ get outState 0

type OpCodes = [Integer]

set1202 :: OpCodes -> OpCodes
set1202 ops = update (update ops 12 1) 2 2

run :: OpCodes -> Int -> OpCodes
run ops i =
  let (ops', end) = step ops i
   in if end
         then ops'
         else run ops' (i+4)

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
