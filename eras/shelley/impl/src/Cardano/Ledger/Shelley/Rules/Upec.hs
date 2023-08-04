{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Epoch change registration.
--
-- The rules of this module determine how the update subsystem of the ledger
-- handles the epoch transitions.
module Cardano.Ledger.Shelley.Rules.Upec (
  ShelleyUPEC,
  UpecState (..),
  ShelleyUpecPredFailure (..),
  votedValue,
) where

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyUPEC)
import Cardano.Ledger.Shelley.Gov
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  UTxOState (..),
  esLState,
  lsCertState,
  lsUTxOState,
  pattern CertState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.Rules.Newpp (
  NewppEnv (..),
  ShelleyNEWPP,
  ShelleyNewppState (..),
 )
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

data UpecState era = UpecState
  { currentPp :: !(PParams era)
  -- ^ Current protocol parameters.
  , ppupState :: !(ShelleyGovState era)
  -- ^ State of the protocol update transition system.
  }

deriving stock instance
  (Show (PParams era), Show (PParamsUpdate era)) =>
  Show (UpecState era)

newtype ShelleyUpecPredFailure era
  = NewPpFailure (PredicateFailure (ShelleyNEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (ShelleyUpecPredFailure era)

instance
  ( EraPParams era
  , Default (PParams era)
  , GovState era ~ ShelleyGovState era
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
            CertState _ pstate dstate = lsCertState ls
            pup = proposals . utxosGovState $ utxoSt
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
  let incrGov vote gov = 1 + Map.findWithDefault 0 vote gov
      votes =
        Map.foldr
          (\vote gov -> Map.insert vote (incrGov vote gov) gov)
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
