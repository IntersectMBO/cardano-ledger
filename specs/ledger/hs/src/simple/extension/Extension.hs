module Extension where

import Ledger.Abstract

type State = [TxWits]

data Signal
  = NoSignal
  | TxSignal TxWits

-- | The empty ledger is a valid base state
baseStates :: [State]
baseStates = [[]]

peelState :: State -> (State, Signal)
peelState [] = ([], NoSignal)
peelState (x:xs) = (xs, TxSignal x)

data PredicateFailure
  = BadInputs
  | IncreasedTotalBalance
  | InsufficientWitnesses
  deriving (Eq, Show)
