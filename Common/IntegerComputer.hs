{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Common.IntegerComputer where

import System.IO
import Control.Monad.State

type ProgramCounter = Integer
type InOut = ([Integer], [Integer])
type Memory = (ProgramCounter, [Integer])

data ComputerState = ComputerState
  { _memory :: Memory
  , _io     :: InOut
  , _base   :: Integer
  } deriving (Show, Eq)

newtype ComputerM a =
  ComputerM { unComputerM :: StateT ComputerState IO a }
  deriving (Functor, Applicative, Monad, MonadState ComputerState, MonadIO)

runComputerM :: ComputerM a -> ComputerState -> IO (a, ComputerState)
runComputerM c s = runStateT (unComputerM c) s

type Step = (ProgramCounter, [Integer], InOut, Integer)
         -> (ProgramCounter, [Integer], InOut, Integer, Bool)

interruptibleComputer :: Step -> ComputerM (Maybe Integer)
interruptibleComputer step = do
  state <- get
  let (pc, mem) = _memory state
  let io@(i,o) = _io state
  let base = _base state
  let (pc', mem', io'@(i',o'), base', end) = step (pc, mem, io, base)
  put $ ComputerState (pc', mem') io' base'
  if end
     then return Nothing
     else if o' /= o
             then return $ Just (head o')
             else interruptibleComputer step

intComputer :: Step -> ComputerM ()
intComputer step = do
  o <- interruptibleComputer step
  case o of
    Nothing -> return ()
    _ -> intComputer step
