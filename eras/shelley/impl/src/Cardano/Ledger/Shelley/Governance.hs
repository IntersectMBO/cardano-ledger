{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Governance (
  EraGovernance (..),
  ShelleyPPUPState (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders (Decode (..), decode, (<!))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates, emptyPPPUpdates)
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default (..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

class
  ( EraPParams era
  , Eq (GovernanceState era)
  , Show (GovernanceState era)
  , NoThunks (GovernanceState era)
  , NFData (GovernanceState era)
  , ToCBOR (GovernanceState era)
  , FromCBOR (GovernanceState era)
  , Default (GovernanceState era)
  ) =>
  EraGovernance era
  where
  type GovernanceState era = (r :: Type) | r -> era

  emptyGovernanceState :: GovernanceState era

  getProposedPPUpdates :: GovernanceState era -> Maybe (ProposedPPUpdates era)
  getProposedPPUpdates _ = Nothing

instance ToExpr (PParamsUpdate era) => ToExpr (ShelleyPPUPState era)

instance Crypto c => EraGovernance (ShelleyEra c) where
  type GovernanceState (ShelleyEra c) = ShelleyPPUPState (ShelleyEra c)
  emptyGovernanceState = ShelleyPPUPState emptyPPPUpdates emptyPPPUpdates

  getProposedPPUpdates = Just . proposals

data ShelleyPPUPState era = ShelleyPPUPState
  { proposals :: !(ProposedPPUpdates era)
  , futureProposals :: !(ProposedPPUpdates era)
  }
  deriving (Generic)

deriving instance Show (PParamsUpdate era) => Show (ShelleyPPUPState era)

deriving instance Eq (PParamsUpdate era) => Eq (ShelleyPPUPState era)

instance NFData (PParamsUpdate era) => NFData (ShelleyPPUPState era)

instance NoThunks (PParamsUpdate era) => NoThunks (ShelleyPPUPState era)

instance (Era era, ToCBOR (PParamsUpdate era)) => ToCBOR (ShelleyPPUPState era) where
  toCBOR (ShelleyPPUPState ppup fppup) =
    encodeListLen 2 <> toCBOR ppup <> toCBOR fppup

instance
  (Era era, FromCBOR (PParamsUpdate era)) =>
  FromCBOR (ShelleyPPUPState era)
  where
  fromCBOR =
    decode $
      RecD ShelleyPPUPState
        <! From
        <! From

instance Default (ShelleyPPUPState era) where
  def = ShelleyPPUPState emptyPPPUpdates emptyPPPUpdates
