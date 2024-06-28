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
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState.Types (
  EraLedgerState,
  HasLedgerState (hlsCertStateL, hlsUTxOStateL),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.Rules.Newpp (
  NewppEnv (..),
  ShelleyNEWPP,
  ShelleyNewppPredFailure,
  ShelleyNewppState (..),
 )
import Control.DeepSeq (NFData)
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
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data UpecState era = UpecState
  { usCurPParams :: !(PParams era)
  -- ^ Current protocol parameters.
  , usGovState :: !(ShelleyGovState era)
  -- ^ State of the protocol update transition system.
  }

deriving stock instance
  (Show (PParams era), Show (PParamsUpdate era)) =>
  Show (UpecState era)

newtype ShelleyUpecPredFailure era
  = NewPpFailure (PredicateFailure (ShelleyNEWPP era))
  deriving (Eq, Show, Generic)

instance NoThunks (ShelleyUpecPredFailure era)

instance NFData (ShelleyNewppPredFailure era) => NFData (ShelleyUpecPredFailure era)

instance
  ( EraGov era
  , Default (PParams era)
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  , HasLedgerState era
  ) =>
  STS (ShelleyUPEC era)
  where
  type State (ShelleyUPEC era) = UpecState era
  type Signal (ShelleyUPEC era) = ()
  type Environment (ShelleyUPEC era) = EraLedgerState era
  type BaseM (ShelleyUPEC era) = ShelleyBase
  type PredicateFailure (ShelleyUPEC era) = ShelleyUpecPredFailure era
  initialRules = []
  transitionRules =
    [ do
        TRC
          ( ls
            , UpecState pp ppupState
            , _
            ) <-
          judgmentContext

        coreNodeQuorum <- liftSTS $ asks quorum

        let utxoState = ls ^. hlsUTxOStateL
            pup = sgsCurProposals ppupState
            ppNew = votedValue pup pp (fromIntegral coreNodeQuorum)
        NewppState pp' ppupState' <-
          trans @(ShelleyNEWPP era) $
            TRC (NewppEnv (ls ^. hlsCertStateL) utxoState, NewppState pp ppupState, ppNew)
        pure $! UpecState pp' ppupState'
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
votedValue (ProposedPPUpdates pppu) pp quorumN =
  let votes =
        Map.foldr
          (\vote -> Map.insertWith (+) vote 1)
          (Map.empty :: Map (PParamsUpdate era) Int)
          pppu
      consensus = Map.filter (>= quorumN) votes
   in case Map.keys consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        [ppu] -> Just $ applyPPUpdates pp ppu
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

instance
  (Era era, STS (ShelleyNEWPP era)) =>
  Embed (ShelleyNEWPP era) (ShelleyUPEC era)
  where
  wrapFailed = NewPpFailure
