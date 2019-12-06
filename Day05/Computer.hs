module Day05.Computer where

import Data.List

-- ProgramCounter
type ProgramCounter = Int

type In = [Int]
type Out = [Int]
type InOut = (In, Out)

-- Memory
data MemoryInterface a = MemoryInterface {
  memory  :: [Int],

  -- Length of operation
  opLen   :: a -> Int,
  -- Raw memory representation
  opRaw   :: a -> [Int],
  -- Parse operations from raw
  parse   :: [Int] -> a,
  -- Execute single operation
  execute :: (InOut, [Int], ProgramCounter) -> (InOut, [Int], ProgramCounter, Bool)
}

updateMemory :: MemoryInterface a -> [Int] -> MemoryInterface a
updateMemory mi m' = MemoryInterface m' (opLen mi) (opRaw mi) (parse mi) (execute mi)

-- Computer
newtype Computer a =
  Computer { run :: (InOut, MemoryInterface a, ProgramCounter) -> (InOut, MemoryInterface a, ProgramCounter, Bool) }

segfault = error "SEGFAULT : Instruction segmentation error"

stepComputer :: Computer a -> MemoryInterface a -> ProgramCounter -> In -> (InOut, MemoryInterface a, ProgramCounter, Bool)
stepComputer c mem pc input = run c ((input,[]), mem, pc)

intComputer :: Computer a
intComputer = Computer executeComputer
  where
    executeComputer :: (InOut, MemoryInterface a, ProgramCounter) -> (InOut, MemoryInterface a, ProgramCounter, Bool)
    executeComputer (io, mi, pc) =
      let (io', m', pc', end) = execute mi (io, memory mi, pc)
       in (io', updateMemory mi m', pc', end)

stepIO :: MemoryInterface a -> ProgramCounter -> InOut -> IO (InOut, MemoryInterface a, ProgramCounter, Bool)
stepIO mem pc (input,output) = do
  let (io', mem', pc', end) = stepComputer intComputer mem pc input
  putStrLn . show $ pc'
  putStrLn . show $ memory mem'
  return $ (io', mem', pc', end)
