{-# LANGUAGE InstanceSigs #-}

module Day05.Intcode where

import Day05.Computer (MemoryInterface(MemoryInterface), ProgramCounter, InOut)

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
  | JT Param Param
  | JF Param Param
  | Less Param Param Param
  | Equals Param Param Param
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
opLen (JT c r)    = 3
opLen (JF c r)    = 3
opLen (Less a b r)   = 4
opLen (Equals a b r) = 4

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
opRaw (JT c r)  =
  let op = 5 + 100*(paramModeRaw c) + 1000*(paramModeRaw r)
    in op:(value c):(value r):[]
opRaw (JF c r)  =
  let op = 6 + 100*(paramModeRaw c) + 1000*(paramModeRaw r)
    in op:(value c):(value r):[]
opRaw (Less a b r)  =
  let op = 7 + 100*(paramModeRaw a) + 1000*(paramModeRaw b) + 10000*(paramModeRaw r)
    in op:(value a):(value b):(value r):[]
opRaw (Equals a b r)  =
  let op = 8 + 100*(paramModeRaw a) + 1000*(paramModeRaw b) + 10000*(paramModeRaw r)
    in op:(value a):(value b):(value r):[]

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
        5  -> let (c:r:rest) = xs
                in JT (Param am c) (Param bm r)
        6  -> let (c:r:rest) = xs
                in JF (Param am c) (Param bm r)
        7  -> let (a:b:r:rest) = xs
                in Less (Param am a) (Param bm b) (Param cm r)
        8  -> let (a:b:r:rest) = xs
                in Equals (Param am a) (Param bm b) (Param cm r)
        99 -> Exit
        _  -> Unknown x

execute :: (InOut, [Int], ProgramCounter) -> (InOut, [Int], ProgramCounter, Bool)
execute (io, mem, pc) =
  let op = parse $ drop pc mem
   in executeOp (io, mem) pc op

executeOp :: (InOut, [Int]) -> ProgramCounter -> Op -> (InOut, [Int], ProgramCounter, Bool)
executeOp (io, mem) pc op@(Exit)      = (io,mem, pc + opLen op, True)
executeOp (io, mem) pc op@(Add a b r) = (io,update mem r $ execOp mem (+) a b, pc + opLen op, False)
executeOp (io, mem) pc op@(Mul a b r) = (io,update mem r $ execOp mem (*) a b, pc + opLen op, False)
executeOp ((input,out),mem) pc op@(Input x)  =
  let mem' = update mem x (head input)
    in ((tail input,out),mem', pc + opLen op, False)
executeOp ((input,out),mem) pc op@(Output x) =
  let v = get (value x) mem
    in ((input,v:out),mem, pc + opLen op, False)
executeOp (io, mem) pc op@(JT c r) =
  let m = resolveParam mem c
      pc' = if m /= 0 then resolveParam mem r else pc + opLen op
   in (io, mem, pc', False)
executeOp (io, mem) pc op@(JF c r) =
  let m = resolveParam mem c
      pc' = if m == 0 then resolveParam mem r else pc + opLen op
   in (io, mem, pc', False)
executeOp (io, mem) pc op@(Less a b r) =
  let a' = resolveParam mem a
      b' = resolveParam mem b
   in (io,update mem r $ if a' < b' then 1 else 0, pc + opLen op, False)
executeOp (io, mem) pc op@(Equals a b r) =
  let a' = resolveParam mem a
      b' = resolveParam mem b
   in (io,update mem r $ if a' == b' then 1 else 0, pc + opLen op, False)
executeOp _ pc op = error $ "UNKNONW INSTRUCTION : " ++ show op

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
