{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Shelley.Spec.Ledger.STS.Upec where

import Cardano.Ledger.Shelley.Constraints (ShelleyBased, UsesAuxiliary, UsesScript, UsesTxBody, UsesValue)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PPUPState (..),
    esAccountState,
    esLState,
    _delegationState,
    _ppups,
    _utxoState,
    pattern DPState,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParamsUpdate, ProposedPPUpdates (..), updatePParams)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP, NewppEnv (..), NewppState (..))

-- | Update epoch change
data UPEC era

data UpecState era = UpecState
  { -- | Current protocol parameters.
    currentPp :: !(PParams era),
    -- | State of the protocol update transition system.
    ppupState :: !(PPUPState era)
  }
  deriving (Show)

data UpecPredicateFailure era
  = NewPpFailure (PredicateFailure (NEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (UpecPredicateFailure era)

instance
  ( UsesAuxiliary era,
    UsesTxBody era,
    UsesScript era,
    UsesValue era
  ) =>
  STS (UPEC era)
  where
  type State (UPEC era) = UpecState era
  type Signal (UPEC era) = ()
  type Environment (UPEC era) = EpochState era
  type BaseM (UPEC era) = ShelleyBase
  type PredicateFailure (UPEC era) = UpecPredicateFailure era
  initialRules = []
  transitionRules =
    [ do
        TRC
          ( EpochState
              { esAccountState = acnt,
                esLState = ls
              },
            UpecState pp ppupSt,
            _
            ) <-
          judgmentContext

        coreNodeQuorum <- liftSTS $ asks quorum

        let utxoSt = _utxoState ls
            DPState dstate pstate = _delegationState ls
            pup = proposals . _ppups $ utxoSt
            ppNew = votedValue pup pp (fromIntegral coreNodeQuorum)
        NewppState pp' ppupSt' <-
          trans @(NEWPP era) $
            TRC (NewppEnv dstate pstate utxoSt acnt, NewppState pp ppupSt, ppNew)
        pure $
          UpecState pp' ppupSt'
    ]

-- | If at least @n@ nodes voted to change __the same__ protocol parameters to
-- __the same__ values, return the given protocol parameters updated to these
-- values. Here @n@ is the quorum needed.
votedValue ::
  ProposedPPUpdates era ->
  -- | Protocol parameters to which the change will be applied.
  PParams era ->
  -- | Quorum needed to change the protocol parameters.
  Int ->
  Maybe (PParams era)
votedValue (ProposedPPUpdates pup) pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map (PParamsUpdate era) Int)
          pup
      consensus = Map.filter (>= quorumN) votes
   in case length consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

instance ShelleyBased era => Embed (NEWPP era) (UPEC era) where
  wrapFailed = NewPpFailure
