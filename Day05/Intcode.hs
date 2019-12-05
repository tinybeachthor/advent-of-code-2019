{-# LANGUAGE InstanceSigs #-}

module Day05.Intcode where

import Day05.Computer (MemoryInterface(MemoryInterface), InOut)

type Filename = String

data Mode = Position | Immediate
  deriving (Show)

modeRaw :: Mode -> Int
modeRaw Position  = 0
modeRaw Immediate = 1

modeParse :: Int -> Mode
modeParse 0 = Position
modeParse 1 = Immediate
modeParse x = error $ "INVALID MODE : " ++ show x

-- Param
data Param = Param { mode :: Mode, value :: Int }
  deriving (Show)

paramModeRaw :: Param -> Int
paramModeRaw = modeRaw . mode

-- Operation
data Op
  = Unknown Int
  | Exit
  | Add Param Param Param
  | Mul Param Param Param
  | Input Param
  | Output Param
  deriving (Show)

intCodeMemory :: [Int] -> MemoryInterface Op
intCodeMemory m = MemoryInterface m opLen opRaw parse execute

opLen :: Op -> Int
opLen (Unknown x) = 1
opLen (Exit)      = 1
opLen (Add a b r) = 4
opLen (Mul a b r) = 4
opLen (Input x)   = 2
opLen (Output x)  = 2

opRaw :: Op -> [Int]
opRaw (Unknown x) = x:[]
opRaw (Exit)      = 99:[]
opRaw (Add a b r) =
  let op = 1 + 100*(paramModeRaw a) + 1000*(paramModeRaw b) + 10000*(paramModeRaw r)
    in op:(value a):(value b):(value r):[]
opRaw (Mul a b r) =
  let op = 2 + 100*(paramModeRaw a) + 1000*(paramModeRaw b) + 10000*(paramModeRaw r)
    in op:(value a):(value b):(value r):[]
opRaw (Input x)   =
  let op = 3 + 100*(paramModeRaw x)
    in op:(value x):[]
opRaw (Output x)  =
  let op = 4 + 100*(paramModeRaw x)
    in op:(value x):[]

parse :: [Int] -> Op
parse [] = error "MEMORY EMPTY"
parse (x:xs) =
  let ('1':cm':bm':am':opStr) = show $ 100000 + x
      am = modeParse . read . pure $ am'
      bm = modeParse . read . pure $ bm'
      cm = modeParse . read . pure $ cm'
      op = read opStr
    in case op of
        1  -> let (a:b:r:rest) = xs
                in Add (Param am a) (Param bm b) (Param cm r)
        2  -> let (a:b:r:rest) = xs
                in Mul (Param am a) (Param bm b) (Param cm r)
        3  -> let (r:rest) = xs
                in Input (Param am r)
        4  -> let (r:rest) = xs
                in Output (Param am r)
        99 -> Exit
        _  -> Unknown x

execute :: (InOut, [Int]) -> Op -> (InOut, [Int], Bool)
execute (io, mem) (Exit)      = (io,mem,True)
execute (io, mem) (Add a b r) = (io,update mem r $ execOp mem (+) a b, False)
execute (io, mem) (Mul a b r) = (io,update mem r $ execOp mem (*) a b, False)
execute ((input,out),mem) (Input x)  =
  let mem' = update mem x (head input)
    in ((tail input,out),mem',False)
execute ((input,out),mem) (Output x) =
  let v = get (value x) mem
    in ((input,v:out),mem,False)
execute _ op = error $ "UNKNONW INSTRUCTION : " ++ show op

resolveParam :: [Int] -> Param -> Int
resolveParam mem p = case mode p of
                          Immediate -> value p
                          Position  -> get (value p) mem

execOp :: [Int] -> (Int -> Int -> Int) -> Param -> Param -> Int
execOp mem f a b =
  let a' = resolveParam mem a
      b' = resolveParam mem b
   in f a' b'

get :: Int -> [Int] -> Int
get i = head . drop i

update :: [Int] -> Param -> Int -> [Int]
update mem a v =
  let (h, t) = splitAt (value a) mem
   in h ++ [v] ++ (tail t)

