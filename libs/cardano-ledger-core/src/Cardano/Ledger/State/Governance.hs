{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.State.Governance (
  EraGov (..),
  FuturePParams (..),
  solidifyFuturePParams,
  nextEpochPParams,
  nextEpochUpdatedPParams,
  knownFuturePParams,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe, maybeToStrictMaybe)
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (..),
  EncCBOR (encCBOR),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State.CertState (Obligations)
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Kind (Type)
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro (Lens', (^.))
import NoThunks.Class (AllowThunk (..), NoThunks (..))

class
  ( EraPParams era
  , Eq (GovState era)
  , Show (GovState era)
  , NoThunks (GovState era)
  , NFData (GovState era)
  , EncCBOR (GovState era)
  , DecCBOR (GovState era)
  , DecShareCBOR (GovState era)
  , Share (GovState era)
      ~ ( Interns (Credential 'Staking)
        , Interns (KeyHash 'StakePool)
        , Interns (Credential 'DRepRole)
        , Interns (Credential 'HotCommitteeRole)
        )
  , ToCBOR (GovState era)
  , FromCBOR (GovState era)
  , ToJSON (GovState era)
  ) =>
  EraGov era
  where
  type GovState era = (r :: Type) | r -> era

  -- | Construct empty governance state
  emptyGovState :: GovState era

  -- | Lens for accessing current protocol parameters
  curPParamsGovStateL :: Lens' (GovState era) (PParams era)

  -- | Lens for accessing the previous protocol parameters
  prevPParamsGovStateL :: Lens' (GovState era) (PParams era)

  -- | Lens for accessing the future protocol parameters.
  --
  -- This lens will produce `DefinitePParamsUpdate` whenever we are absolutely sure that
  -- the new PParams will be updated. Which means there will be no chance of a
  -- `DefinitePParamsUpdate` value until we are past the point of no return, which is 2
  -- stability windows before the end of the epoch. This lens is mostly intended for
  -- ledger usage and `nextEpochUpdatedPParams` should be used instead whenever definite
  -- results are desired.
  futurePParamsGovStateL :: Lens' (GovState era) (FuturePParams era)

  obligationGovState :: GovState era -> Obligations

data FuturePParams era
  = -- | This indicates that there is definitely not going to be an update to PParams
    -- expected at the next epoch boundary.
    NoPParamsUpdate
  | -- | This case specifies the PParams that will be adopted at the next epoch boundary.
    DefinitePParamsUpdate !(PParams era)
  | -- | With this case there is no guarantee that these will be the new PParams, users
    -- should not rely on this value to be computed efficiently and should use
    -- `nextEpochPParams` instead. The field is lazy on purpose, since we truly need to
    -- compute this field only towards the end of the epoch, which is done by
    -- `solidifyFuturePParams` two stability windows before the end of the epoch.
    PotentialPParamsUpdate (Maybe (PParams era))
  deriving (Generic)

instance ToJSON (PParams era) => ToJSON (FuturePParams era)

-- | Return new PParams only when it is known that there was an update proposed and it is
-- guaranteed to be applied
knownFuturePParams :: FuturePParams era -> Maybe (PParams era)
knownFuturePParams = \case
  DefinitePParamsUpdate pp -> Just pp
  _ -> Nothing

-- | This function is guaranteed to produce `PParams` that will be adopted at the next
-- epoch boundary, whenever this function is applied to the `GovState` that was produced
-- by ledger at any point that is two stability windows before the end of the epoch. If
-- you need to know if there were actual changes to those PParams then use
-- `nextEpochUpdatedPParams` instead.
nextEpochPParams :: EraGov era => GovState era -> PParams era
nextEpochPParams govState =
  fromSMaybe (govState ^. curPParamsGovStateL) $ nextEpochUpdatedPParams govState

-- | This function is guaranteed to return updated PParams when it is called during the
-- last two stability windows of the epoch and there were proposals to update PParams that
-- all relevant parties reached consensus on. In other words whenever there is a definite
-- update to PParams coming on the epoch boundary those PParams will be returned,
-- otherwise it will return `Nothing`. This function is inexpensive and can be invoked at
-- any time without danger of forcing some suspended computation.
nextEpochUpdatedPParams :: EraGov era => GovState era -> StrictMaybe (PParams era)
nextEpochUpdatedPParams govState =
  maybeToStrictMaybe $ knownFuturePParams (govState ^. futurePParamsGovStateL)

solidifyFuturePParams :: FuturePParams era -> FuturePParams era
solidifyFuturePParams = \case
  -- Here we convert a potential to a definite update:
  PotentialPParamsUpdate Nothing -> NoPParamsUpdate
  PotentialPParamsUpdate (Just pp) -> DefinitePParamsUpdate pp
  fpp -> fpp

deriving stock instance Eq (PParams era) => Eq (FuturePParams era)

deriving stock instance Show (PParams era) => Show (FuturePParams era)

deriving via AllowThunk (FuturePParams era) instance NoThunks (FuturePParams era)

instance (Typeable era, EncCBOR (PParams era)) => EncCBOR (FuturePParams era) where
  encCBOR =
    encode . \case
      NoPParamsUpdate -> Sum NoPParamsUpdate 0
      DefinitePParamsUpdate pp -> Sum DefinitePParamsUpdate 1 !> To pp
      PotentialPParamsUpdate pp -> Sum PotentialPParamsUpdate 2 !> To pp

instance (Typeable era, DecCBOR (PParams era)) => DecCBOR (FuturePParams era) where
  decCBOR = decode . Summands "FuturePParams" $ \case
    0 -> SumD NoPParamsUpdate
    1 -> SumD DefinitePParamsUpdate <! From
    2 -> SumD PotentialPParamsUpdate <! From
    k -> Invalid k

instance NFData (PParams era) => NFData (FuturePParams era) where
  rnf = \case
    NoPParamsUpdate -> ()
    PotentialPParamsUpdate pp -> rnf pp
    DefinitePParamsUpdate pp -> rnf pp
