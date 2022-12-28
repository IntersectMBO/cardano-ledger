{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Cardano.Ledger.Shelley.Rules.Upec (
  ShelleyUPEC,
  ShelleyUpecPredFailure (..),
  votedValue,
)
where

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyUPEC)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  PPUPState (..),
  UTxOState (utxosPpups),
  UpecState (..),
  esLState,
  lsDPState,
  lsUTxOState,
  pattern DPState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.Rules.Newpp (NewppEnv (..), ShelleyNEWPP, ShelleyNewppState (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Default.Class (Default)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype ShelleyUpecPredFailure era
  = NewPpFailure (PredicateFailure (ShelleyNEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (ShelleyUpecPredFailure era)

instance
  ( EraPParams era
  , Default (PParams era)
  , State (EraRule "PPUP" era) ~ PPUPState era
  ) =>
  STS (ShelleyUPEC era)
  where
  type State (ShelleyUPEC era) = UpecState era
  type Signal (ShelleyUPEC era) = ()
  type Environment (ShelleyUPEC era) = EpochState era
  type BaseM (ShelleyUPEC era) = ShelleyBase
  type PredicateFailure (ShelleyUPEC era) = ShelleyUpecPredFailure era
  initialRules = []
  transitionRules =
    [ do
        TRC
          ( EpochState
              { esLState = ls
              }
            , UpecState pp ppupSt
            , _
            ) <-
          judgmentContext

        coreNodeQuorum <- liftSTS $ asks quorum

        let utxoSt = lsUTxOState ls
            DPState dstate pstate = lsDPState ls
            pup = proposals . utxosPpups $ utxoSt
            ppNew = votedValue pup pp (fromIntegral coreNodeQuorum)
        NewppState pp' ppupSt' <-
          trans @(ShelleyNEWPP era) $
            TRC (NewppEnv dstate pstate utxoSt, NewppState pp ppupSt, ppNew)
        pure $
          UpecState pp' ppupSt'
    ]

-- | If at least @n@ nodes voted to change __the same__ protocol parameters to
-- __the same__ values, return the given protocol parameters updated to these
-- values. Here @n@ is the quorum needed.
votedValue ::
  forall era.
  EraPParams era =>
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
        1 ->
          (Just . applyPPUpdates pps . fst . head . Map.toList)
            consensus
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

instance
  (Era era, STS (ShelleyNEWPP era)) =>
  Embed (ShelleyNEWPP era) (ShelleyUPEC era)
  where
  wrapFailed = NewPpFailure
