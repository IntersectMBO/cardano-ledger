{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Governance (
  EraGov (..),
  ShelleyGovState (..),
  emptyShelleyGovState,
  FuturePParams (..),
  solidifyFuturePParams,
  knownFuturePParams,
  nextEpochPParams,
  nextEpochUpdatedPParams,
  -- Lens
  curPParamsShelleyGovStateL,
  prevPParamsShelleyGovStateL,
  futurePParamsShelleyGovStateL,
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (..),
  EncCBOR (encCBOR),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates, emptyPPPUpdates)
import Cardano.Ledger.State
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), (.=))
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks (..))

instance EraGov ShelleyEra where
  type GovState ShelleyEra = ShelleyGovState ShelleyEra

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateL = futurePParamsShelleyGovStateL

  obligationGovState = const mempty -- No GovState obigations in ShelleyEra

data ShelleyGovState era = ShelleyGovState
  { sgsCurProposals :: !(ProposedPPUpdates era)
  , sgsFutureProposals :: !(ProposedPPUpdates era)
  , sgsCurPParams :: !(PParams era)
  , sgsPrevPParams :: !(PParams era)
  , sgsFuturePParams :: !(FuturePParams era)
  -- ^ Prediction of parameter changes that might happen on the epoch boundary.
  }
  deriving (Generic)
  deriving (ToJSON) via KeyValuePairs (ShelleyGovState era)

curPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
curPParamsShelleyGovStateL = lens sgsCurPParams (\sps x -> sps {sgsCurPParams = x})

prevPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
prevPParamsShelleyGovStateL = lens sgsPrevPParams (\sps x -> sps {sgsPrevPParams = x})

futurePParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (FuturePParams era)
futurePParamsShelleyGovStateL =
  lens sgsFuturePParams (\sps x -> sps {sgsFuturePParams = x})

deriving instance
  ( Show (PParamsUpdate era)
  , Show (PParams era)
  ) =>
  Show (ShelleyGovState era)

deriving instance
  ( Eq (PParamsUpdate era)
  , Eq (PParams era)
  ) =>
  Eq (ShelleyGovState era)

instance
  ( NFData (PParamsUpdate era)
  , NFData (PParams era)
  ) =>
  NFData (ShelleyGovState era)

instance
  ( NoThunks (PParamsUpdate era)
  , NoThunks (PParams era)
  ) =>
  NoThunks (ShelleyGovState era)

instance
  ( Era era
  , EncCBOR (PParamsUpdate era)
  , EncCBOR (PParams era)
  ) =>
  EncCBOR (ShelleyGovState era)
  where
  encCBOR (ShelleyGovState ppup fppup pp ppp fpp) =
    encode $
      Rec ShelleyGovState
        !> To ppup
        !> To fppup
        !> To pp
        !> To ppp
        !> To fpp

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (PParams era)
  ) =>
  DecShareCBOR (ShelleyGovState era)
  where
  type
    Share (ShelleyGovState era) =
      ( Interns (Credential Staking)
      , Interns (KeyHash StakePool)
      , Interns (Credential DRepRole)
      , Interns (Credential HotCommitteeRole)
      )
  decShareCBOR _ =
    decode $
      RecD ShelleyGovState
        <! From
        <! From
        <! From
        <! From
        <! From

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (PParams era)
  ) =>
  DecCBOR (ShelleyGovState era)
  where
  decCBOR = decNoShareCBOR

instance
  ( Era era
  , EncCBOR (PParamsUpdate era)
  , EncCBOR (PParams era)
  ) =>
  ToCBOR (ShelleyGovState era)
  where
  toCBOR = toEraCBOR @era

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (PParams era)
  ) =>
  FromCBOR (ShelleyGovState era)
  where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => ToKeyValuePairs (ShelleyGovState era) where
  toKeyValuePairs ShelleyGovState {..} =
    [ "proposals" .= sgsCurProposals
    , "futureProposals" .= sgsFutureProposals
    , "curPParams" .= sgsCurPParams
    , "prevPParams" .= sgsPrevPParams
    ]

instance EraPParams era => Default (ShelleyGovState era) where
  def = emptyShelleyGovState

emptyShelleyGovState :: EraPParams era => ShelleyGovState era
emptyShelleyGovState =
  ShelleyGovState
    emptyPPPUpdates
    emptyPPPUpdates
    emptyPParams
    emptyPParams
    NoPParamsUpdate
