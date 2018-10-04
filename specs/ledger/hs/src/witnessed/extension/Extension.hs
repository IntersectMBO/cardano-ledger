module Extension where

import qualified Data.Map.Strict  as Map
import qualified Extension.Simple as Simple
import           Ledger.Abstract

-- |The state associated with a 'Ledger'.
data State =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo        :: UTxO
  , peelState :: (State, Signal)
  } deriving (Show, Eq)

emptyState :: State
emptyState = LedgerState
  { getUtxo = UTxO Map.empty
  , peelState = (emptyState, NoSignal)
  }

data Signal
  = NoSignal
  | TxSignal TxWits
  deriving (Eq, Show)

-- | The empty ledger is a valid base state
baseStates :: [State]
baseStates = [emptyState]

data PredicateFailure
  = TxFailure Simple.PredicateFailure
  | InsufficientWitnesses
  deriving (Eq, Show)
