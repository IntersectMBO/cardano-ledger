{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UTXO transition system
module Byron.Spec.Ledger.STS.UTXO
  ( UTXO,
    UTxOEnv (UTxOEnv),
    UTxOState (UTxOState),
    UtxoPredicateFailure (..),
    PredicateFailure,
    utxo,
    utxo0,
    pps,
    reserves,
  )
where

import Byron.Spec.Ledger.Core (Lovelace, dom, range, (∪), (⊆), (⋪), (◁))
import Byron.Spec.Ledger.GlobalParams (lovelaceCap)
import Byron.Spec.Ledger.UTxO (Tx, UTxO, balance, body, pcMinFee, txins, txouts, unUTxO, value)
import Byron.Spec.Ledger.Update (PParams)
import Control.State.Transition
  ( Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    transitionRules,
    (?!),
  )
import Data.Data (Data, Typeable)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Test.Goblin (SeedGoblin (..))
import Test.Goblin.TH (deriveSeedGoblin)

data UTXO deriving (Data, Typeable)

data UTxOEnv = UTxOEnv
  { utxo0 :: UTxO,
    pps :: PParams
  }
  deriving (Eq, Show, Generic, NoThunks)

data UTxOState = UTxOState
  { utxo :: UTxO,
    reserves :: Lovelace
  }
  deriving (Eq, Show, Generic, NoThunks)

-- | These `PredicateFailure`s are all "throwable". The disjunction of the
--   rules' preconditions is not `True` - the `PredicateFailure`s represent
--   `False` cases.
data UtxoPredicateFailure
  = EmptyTxInputs
  | EmptyTxOutputs
  | FeeTooLow
  | IncreasedTotalBalance
  | InputsNotInUTxO
  | NonPositiveOutputs
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UTXO where
  type Environment UTXO = UTxOEnv
  type State UTXO = UTxOState
  type Signal UTXO = Tx
  type PredicateFailure UTXO = UtxoPredicateFailure

  initialRules =
    [ do
        IRC UTxOEnv {utxo0} <- judgmentContext
        return $
          UTxOState
            { utxo = utxo0,
              reserves = lovelaceCap - balance utxo0
            }
    ]
  transitionRules =
    [ do
        TRC
          ( UTxOEnv _ pps,
            UTxOState {utxo, reserves},
            tx
            ) <-
          judgmentContext

        let ins = txins $ body tx
            outs = txouts $ body tx

        ins ⊆ dom utxo ?! InputsNotInUTxO

        let fee = balance (ins ◁ utxo) - balance outs

        pcMinFee pps tx <= fee ?! FeeTooLow

        (not . null) ins ?! EmptyTxInputs

        (not . null . unUTxO) outs ?! EmptyTxOutputs

        let outputValues = fmap value $ Set.toList $ range outs
        all (0 <) outputValues ?! NonPositiveOutputs

        return $
          UTxOState
            { utxo = (ins ⋪ utxo) ∪ outs,
              reserves = reserves + fee
            }
    ]

--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''UTxOEnv
deriveSeedGoblin ''UTxOState
