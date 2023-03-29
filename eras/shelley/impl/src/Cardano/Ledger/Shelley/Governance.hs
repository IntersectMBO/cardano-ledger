{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Governance (
  EraGovernance (..),
  ShelleyPPUPState (..),
  Diff (ShelleyPPUPState'),
) where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), decode, (<!))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), emptyPPPUpdates)
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Incremental (ILC (..), ($$))
import Data.Kind (Type)
import Data.Map (Map)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

class
  ( EraPParams era
  , Eq (GovernanceState era)
  , Show (GovernanceState era)
  , NoThunks (GovernanceState era)
  , NFData (GovernanceState era)
  , EncCBOR (GovernanceState era)
  , DecCBOR (GovernanceState era)
  , ToCBOR (GovernanceState era)
  , FromCBOR (GovernanceState era)
  , Default (GovernanceState era)
  , ToJSON (GovernanceState era)
  ) =>
  EraGovernance era
  where
  type GovernanceState era = (r :: Type) | r -> era

  -- | Construct empty governance state
  emptyGovernanceState :: GovernanceState era
  emptyGovernanceState = def

  -- | Returns `Nothing` for all eras starting with Conway, otherwise returns proposed
  -- pparams updates
  getProposedPPUpdates :: GovernanceState era -> Maybe (ProposedPPUpdates era)
  getProposedPPUpdates _ = Nothing

instance ToExpr (PParamsUpdate era) => ToExpr (ShelleyPPUPState era)

instance Crypto c => EraGovernance (ShelleyEra c) where
  type GovernanceState (ShelleyEra c) = ShelleyPPUPState (ShelleyEra c)

  getProposedPPUpdates = Just . proposals

data ShelleyPPUPState era = ShelleyPPUPState
  { proposals :: !(ProposedPPUpdates era)
  , futureProposals :: !(ProposedPPUpdates era)
  }
  deriving (Generic)

instance ILC (ShelleyPPUPState era) where
  data Diff (ShelleyPPUPState era) = ShelleyPPUPState'
    { diffProposals :: !(Diff (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era)))
    , diffFutureProposals :: !(Diff (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era)))
    }
  applyDiff ShelleyPPUPState {..} ShelleyPPUPState' {..} =
    ShelleyPPUPState
      { proposals = ProposedPPUpdates (unProposedPPUpdates proposals $$ diffProposals)
      , futureProposals = ProposedPPUpdates (unProposedPPUpdates futureProposals $$ diffProposals)
      }
  extend x y =
    ShelleyPPUPState'
      { diffProposals = extend (diffProposals x) (diffProposals y)
      , diffFutureProposals = extend (diffFutureProposals x) (diffFutureProposals y)
      }
  zero = ShelleyPPUPState' zero zero
  totalDiff _ = ShelleyPPUPState' zero zero

deriving instance Show (PParamsUpdate era) => Show (Diff (ShelleyPPUPState era))

deriving instance Eq (PParamsUpdate era) => Eq (Diff (ShelleyPPUPState era))

deriving instance Show (PParamsUpdate era) => Show (ShelleyPPUPState era)

deriving instance Eq (PParamsUpdate era) => Eq (ShelleyPPUPState era)

instance NFData (PParamsUpdate era) => NFData (ShelleyPPUPState era)

instance NoThunks (PParamsUpdate era) => NoThunks (ShelleyPPUPState era)

instance (Era era, EncCBOR (PParamsUpdate era)) => EncCBOR (ShelleyPPUPState era) where
  encCBOR (ShelleyPPUPState ppup fppup) =
    encodeListLen 2 <> encCBOR ppup <> encCBOR fppup

instance
  (Era era, DecCBOR (PParamsUpdate era)) =>
  DecCBOR (ShelleyPPUPState era)
  where
  decCBOR =
    decode $
      RecD ShelleyPPUPState
        <! From
        <! From

instance (Era era, EncCBOR (PParamsUpdate era)) => ToCBOR (ShelleyPPUPState era) where
  toCBOR = toEraCBOR @era

instance (Era era, DecCBOR (PParamsUpdate era)) => FromCBOR (ShelleyPPUPState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => ToJSON (ShelleyPPUPState era) where
  toJSON = object . toPPUPStatePairs
  toEncoding = pairs . mconcat . toPPUPStatePairs

toPPUPStatePairs :: (KeyValue a, EraPParams era) => ShelleyPPUPState era -> [a]
toPPUPStatePairs ShelleyPPUPState {..} =
  [ "proposals" .= proposals
  , "futureProposals" .= futureProposals
  ]

instance Default (ShelleyPPUPState era) where
  def = ShelleyPPUPState emptyPPPUpdates emptyPPPUpdates
