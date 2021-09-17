{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Shelley.Spec.Ledger.STS.Upec where

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, StrictMaybe)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.Constraints
  ( ShelleyBased,
    UsesAuxiliary,
    UsesPParams (mergePPUpdates),
    UsesScript,
    UsesTxBody,
    UsesValue,
  )
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Default.Class (Default)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PPUPState (..),
    UpecState (..),
    esAccountState,
    esLState,
    _delegationState,
    _ppups,
    _utxoState,
    pattern DPState,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (ProposedPPUpdates (..), ProtVer)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP, NewppEnv (..), NewppState (..))

-- | Update epoch change
data UPEC era

newtype UpecPredicateFailure era
  = NewPpFailure (PredicateFailure (NEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (UpecPredicateFailure era)

instance
  ( UsesAuxiliary era,
    UsesTxBody era,
    UsesScript era,
    UsesValue era,
    UsesPParams era,
    Default (Core.PParams era),
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (Core.PParamsDelta era) (StrictMaybe ProtVer)
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
  forall era.
  UsesPParams era =>
  ProposedPPUpdates era ->
  -- | Protocol parameters to which the change will be applied.
  Core.PParams era ->
  -- | Quorum needed to change the protocol parameters.
  Int ->
  Maybe (Core.PParams era)
votedValue (ProposedPPUpdates pup) pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map (Core.PParamsDelta era) Int)
          pup
      consensus = Map.filter (>= quorumN) votes
   in case length consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        1 ->
          (Just . mergePPUpdates (Proxy @era) pps . fst . head . Map.toList)
            consensus
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

instance
  (ShelleyBased era, STS (NEWPP era)) =>
  Embed (NEWPP era) (UPEC era)
  where
  wrapFailed = NewPpFailure
