{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Common.IntComputer where

import System.IO
import Control.Monad.State

type ProgramCounter = Int
type InOut = ([Int], [Int])
type Memory = (ProgramCounter, [Int])

data ComputerState = ComputerState
  { _memory :: Memory
  , _io     :: InOut
  } deriving (Show, Eq)

newtype ComputerM a =
  ComputerM { unComputerM :: StateT ComputerState IO a }
  deriving (Functor, Applicative, Monad, MonadState ComputerState, MonadIO)

runComputerM :: ComputerM a -> ComputerState -> IO (a, ComputerState)
runComputerM c s = runStateT (unComputerM c) s

type Step =
  (ProgramCounter, [Int], InOut) -> (ProgramCounter, [Int], InOut, Bool)

interruptibleComputer :: Step -> ComputerM (Maybe Int)
interruptibleComputer step = do
  state <- get
  let (pc, mem) = _memory state
  let io@(i,o) = _io state
  let (pc', mem', io'@(i',o'), end) = step (pc, mem, io)
  put $ ComputerState (pc', mem') io'
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
