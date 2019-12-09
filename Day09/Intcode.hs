{-# LANGUAGE InstanceSigs #-}

module Day09.Intcode where

import Common.IntegerComputer
  (ProgramCounter, InOut)

step :: (ProgramCounter, [Integer], InOut, Integer) -> (ProgramCounter, [Integer], InOut, Integer, Bool)
step (pc, mem, io, base) =
  let op = parse $ drop (fromIntegral pc) mem
      (io', mem', pc', base', end) = executeOp (io, mem) pc base op
   in (pc', mem', io', base', end)

-- Mode
data Mode = Position | Immediate | Relative
  deriving (Show)

modeRaw :: Mode -> Integer
modeRaw Position  = 0
modeRaw Immediate = 1
modeRaw Relative  = 2

modeParse :: Integer -> Mode
modeParse 0 = Position
modeParse 1 = Immediate
modeParse 2 = Relative
modeParse x = error $ "INVALID MODE : " ++ show x

-- Param
data Param = Param { mode :: Mode, value :: Integer }
  deriving (Show)

paramModeRaw :: Param -> Integer
paramModeRaw = modeRaw . mode

-- Operation
data Op
  = Unknown Integer
  | Exit
  | Add Param Param Param
  | Mul Param Param Param
  | Input Param
  | Output Param
  | JT Param Param
  | JF Param Param
  | Less Param Param Param
  | Equals Param Param Param
  | SetBase Param
  deriving (Show)

opLen :: Op -> Integer
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
opLen (SetBase a) = 2

opRaw :: Op -> [Integer]
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
opRaw (SetBase a)  =
  let op = 9 + 100*(paramModeRaw a)
    in op:(value a):[]

parse :: [Integer] -> Op
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
        9  -> let (r:rest) = xs
                in SetBase (Param am r)
        99 -> Exit
        _  -> Unknown x

executeOp :: (InOut, [Integer]) -> ProgramCounter -> Integer -> Op -> (InOut, [Integer], ProgramCounter, Integer, Bool)
executeOp (io, mem) pc base op@(Exit) =
  (io,mem, pc + opLen op, base, True)
executeOp (io, mem) pc base op@(Add a b r) =
  (io,update mem (resolveAddress base r) $ execFn mem (+) base a b, pc + opLen op, base, False)
executeOp (io, mem) pc base op@(Mul a b r) =
  (io,update mem (resolveAddress base r) $ execFn mem (*) base a b, pc + opLen op, base, False)
executeOp ((input,out),mem) pc base op@(Input x)  =
  let mem' = update mem (resolveAddress base x) (head input)
    in ((tail input,out),mem', pc + opLen op, base, False)
executeOp ((input,out),mem) pc base op@(Output x) =
  let v = resolveParam mem base x
    in ((input,v:out),mem, pc + opLen op, base, False)
executeOp (io, mem) pc base op@(JT c r) =
  let m = resolveParam mem base c
      pc' = if m /= 0 then resolveParam mem base r else pc + opLen op
   in (io, mem, pc', base, False)
executeOp (io, mem) pc base op@(JF c r) =
  let m = resolveParam mem base c
      pc' = if m == 0 then resolveParam mem base r else pc + opLen op
   in (io, mem, pc', base, False)
executeOp (io, mem) pc base op@(Less a b r) =
  let a' = resolveParam mem base a
      b' = resolveParam mem base b
   in (io,update mem (resolveAddress base r) $ if a' < b' then 1 else 0, pc + opLen op, base, False)
executeOp (io, mem) pc base op@(Equals a b r) =
  let a' = resolveParam mem base a
      b' = resolveParam mem base b
   in (io,update mem (resolveAddress base r) $ if a' == b' then 1 else 0, pc + opLen op, base, False)
executeOp (io, mem) pc base op@(SetBase a) =
  (io,mem, pc + opLen op, base + resolveParam mem base a, False)
executeOp _ _ _ op =
  error $ "UNKNONW INSTRUCTION : " ++ show op

-- Helpers
resolveParam :: [Integer] -> Integer -> Param -> Integer
resolveParam mem base p = case mode p of
                               Immediate -> value p
                               Position  -> get (value p) mem
                               Relative  -> get (base + value p) mem

resolveAddress :: Integer -> Param -> Integer
resolveAddress base p = case mode p of
                               Immediate -> value p
                               Position  -> value p
                               Relative  -> base + value p

execFn :: [Integer] -> (Integer -> Integer -> Integer) -> Integer -> Param -> Param -> Integer
execFn mem f base a b =
  let a' = resolveParam mem base a
      b' = resolveParam mem base b
   in f a' b'

get :: Integer -> [Integer] -> Integer
get i mem = case drop (fromIntegral i) mem of
              [] -> 0
              a:_ -> a

update :: [Integer] -> Integer -> Integer -> [Integer]
update mem a v =
  let (h, t) = splitAt (fromIntegral a) mem
   in case t of
        [] -> h ++ take (fromIntegral a - length mem) (repeat 0) ++ [v]
        _  -> h ++ [v] ++ (tail t)
