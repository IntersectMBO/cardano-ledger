{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Shelley.Governance (
  EraGov (..),
  ShelleyGovState (..),
  emptyShelleyGovState,
  -- Lens
  proposalsL,
  futureProposalsL,
  curPParamsShelleyGovStateL,
  prevPParamsShelleyGovStateL,
  futurePParamsShelleyGovStateL,

  -- * Deprecations
  proposals,
  futureProposals,
  sgovPp,
  sgovPrevPp,
) where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (..),
  EncCBOR (encCBOR),
  FromCBOR (..),
  ToCBOR (..),
  decNoShareCBOR,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.CertState (Obligations)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates, emptyPPPUpdates)
import Control.DeepSeq (NFData)
import Data.Aeson (
  KeyValue,
  ToJSON (..),
  object,
  pairs,
  (.=),
 )
import Data.Default.Class (Default (..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens)
import NoThunks.Class (NoThunks (..))

class
  ( EraPParams era
  , Eq (GovState era)
  , Show (GovState era)
  , NoThunks (GovState era)
  , NFData (GovState era)
  , EncCBOR (GovState era)
  , DecCBOR (GovState era)
  , DecShareCBOR (GovState era)
  , ToCBOR (GovState era)
  , FromCBOR (GovState era)
  , Default (GovState era)
  , ToJSON (GovState era)
  ) =>
  EraGov era
  where
  type GovState era = (r :: Type) | r -> era

  -- | Construct empty governance state
  emptyGovState :: GovState era
  emptyGovState = def

  -- | Returns `Nothing` for all eras starting with Conway, otherwise returns proposed
  -- pparams updates
  getProposedPPUpdates :: GovState era -> Maybe (ProposedPPUpdates era)
  getProposedPPUpdates _ = Nothing

  -- | Lens for accessing current protocol parameters
  curPParamsGovStateL :: Lens' (GovState era) (PParams era)

  -- | Lens for accessing the previous protocol parameters
  prevPParamsGovStateL :: Lens' (GovState era) (PParams era)

  -- | Getter for accessing the future protocol parameters.
  --
  -- This getter is only reliable and efficient 2 stability before the end of the
  -- epoch. Depending on the era, if called earlier in the epoch, it will either produce
  -- unreliable results or getting those results will be somewhat costly.
  --
  -- Whenever called at the earliest two stability before the end of the epoch, then the
  -- results will be 100% reliable and they will contain either a `Just` value with the
  -- new `PParams`, when there was an update proposed and `Nothing` whenever PParams will
  -- reamin unchanged at the next epoch boundary.
  futurePParamsGovStateG :: SimpleGetter (GovState era) (Maybe (PParams era))

  obligationGovState :: GovState era -> Obligations

instance Crypto c => EraGov (ShelleyEra c) where
  type GovState (ShelleyEra c) = ShelleyGovState (ShelleyEra c)

  getProposedPPUpdates = Just . sgsCurProposals

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateG = futurePParamsShelleyGovStateL

  obligationGovState = const mempty -- No GovState obigations in ShelleyEra

data ShelleyGovState era = ShelleyGovState
  { sgsCurProposals :: !(ProposedPPUpdates era)
  , sgsFutureProposals :: !(ProposedPPUpdates era)
  , sgsCurPParams :: !(PParams era)
  , sgsPrevPParams :: !(PParams era)
  , sgsFuturePParams :: Maybe (PParams era)
  -- ^ Prediction of any parameter changes that might happen on the epoch boundary. The
  -- field is lazy on purpose, since we need to compute this field only towards the
  -- end of the epoch.
  }
  deriving (Generic)

proposals :: ShelleyGovState era -> ProposedPPUpdates era
proposals = sgsCurProposals
{-# DEPRECATED proposals "In favor of `sgsCurProposals`" #-}
futureProposals :: ShelleyGovState era -> ProposedPPUpdates era
futureProposals = sgsFutureProposals
{-# DEPRECATED futureProposals "In favor of `sgsFutureProposals`" #-}
sgovPp :: ShelleyGovState era -> PParams era
sgovPp = sgsCurPParams
{-# DEPRECATED sgovPp "In favor of `sgsCurPParams`" #-}
sgovPrevPp :: ShelleyGovState era -> PParams era
sgovPrevPp = sgsPrevPParams
{-# DEPRECATED sgovPrevPp "In favor of `sgsPrevPParams`" #-}

proposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
proposalsL = lens sgsCurProposals (\sgov x -> sgov {sgsCurProposals = x})

futureProposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
futureProposalsL = lens sgsFutureProposals (\sgov x -> sgov {sgsFutureProposals = x})

curPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
curPParamsShelleyGovStateL = lens sgsCurPParams (\sps x -> sps {sgsCurPParams = x})

prevPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
prevPParamsShelleyGovStateL = lens sgsPrevPParams (\sps x -> sps {sgsPrevPParams = x})

futurePParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (Maybe (PParams era))
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

instance EraPParams era => ToJSON (ShelleyGovState era) where
  toJSON = object . toPPUPStatePairs
  toEncoding = pairs . mconcat . toPPUPStatePairs

toPPUPStatePairs :: (KeyValue e a, EraPParams era) => ShelleyGovState era -> [a]
toPPUPStatePairs ShelleyGovState {..} =
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
    Nothing
