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
  EraGovernance (..),
  ShelleyGovState (..),
  Constitution (..),
  ConstitutionData (..),
  -- Lens
  proposalsL,
  futureProposalsL,
  curPParamsShelleyGovStateL,
  prevPParamsShelleyGovStateL,
  constitutionHashL,
  constitutionScriptL,
) where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  FromCBOR (..),
  ToCBOR (..),
  decodeNullStrictMaybe,
  encodeNullStrictMaybe,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates, emptyPPPUpdates)
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks (..))

newtype ConstitutionData = ConstitutionData ByteString

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

  -- | Returns `Nothing` for all era preceding Conway, otherwise returns the hash of the constitution
  getConstitutionHash :: GovernanceState era -> Maybe (SafeHash (EraCrypto era) ConstitutionData)
  getConstitutionHash = const Nothing

  -- | Lens for accessing current protocol parameters
  curPParamsGovStateL :: Lens' (GovernanceState era) (PParams era)

  -- | Lens for accessing the previous protocol parameters
  prevPParamsGovStateL :: Lens' (GovernanceState era) (PParams era)

instance
  ( ToExpr (PParamsUpdate era)
  , ToExpr (PParams era)
  ) =>
  ToExpr (ShelleyGovState era)

instance Crypto c => EraGovernance (ShelleyEra c) where
  type GovernanceState (ShelleyEra c) = ShelleyGovState (ShelleyEra c)

  getProposedPPUpdates = Just . proposals

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

data ShelleyGovState era = ShelleyGovState
  { proposals :: !(ProposedPPUpdates era)
  , futureProposals :: !(ProposedPPUpdates era)
  , sgovPp :: !(PParams era)
  , sgovPrevPp :: !(PParams era)
  }
  deriving (Generic)

proposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
proposalsL = lens proposals (\sgov x -> sgov {proposals = x})

futureProposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
futureProposalsL = lens futureProposals (\sgov x -> sgov {futureProposals = x})

curPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
curPParamsShelleyGovStateL = lens sgovPp (\sps x -> sps {sgovPp = x})

prevPParamsShelleyGovStateL :: Lens' (ShelleyGovState era) (PParams era)
prevPParamsShelleyGovStateL = lens sgovPrevPp (\sps x -> sps {sgovPrevPp = x})

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
  encCBOR (ShelleyGovState ppup fppup pp ppp) =
    encode $
      Rec ShelleyGovState
        !> To ppup
        !> To fppup
        !> To pp
        !> To ppp

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (PParams era)
  ) =>
  DecCBOR (ShelleyGovState era)
  where
  decCBOR =
    decode $
      RecD ShelleyGovState
        <! From
        <! From
        <! From
        <! From

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

toPPUPStatePairs :: (KeyValue a, EraPParams era) => ShelleyGovState era -> [a]
toPPUPStatePairs ShelleyGovState {..} =
  [ "proposals" .= proposals
  , "futureProposals" .= futureProposals
  , "curPParams" .= sgovPp
  , "prevPParams" .= sgovPrevPp
  ]

instance EraPParams era => Default (ShelleyGovState era) where
  def =
    ShelleyGovState
      emptyPPPUpdates
      emptyPPPUpdates
      emptyPParams
      emptyPParams

data Constitution era = Constitution
  { constitutionHash :: !(SafeHash (EraCrypto era) ConstitutionData)
  , constitutionScript :: !(StrictMaybe (ScriptHash (EraCrypto era)))
  }
  deriving (Generic)

constitutionHashL :: Lens' (Constitution era) (SafeHash (EraCrypto era) ConstitutionData)
constitutionHashL = lens constitutionHash (\x y -> x {constitutionHash = y})

constitutionScriptL :: Lens' (Constitution era) (StrictMaybe (ScriptHash (EraCrypto era)))
constitutionScriptL = lens constitutionScript (\x y -> x {constitutionScript = y})

instance Era era => ToJSON (Constitution era) where
  toJSON = object . toConstitutionPairs
  toEncoding = pairs . mconcat . toConstitutionPairs

toConstitutionPairs :: (KeyValue a, Era era) => Constitution era -> [a]
toConstitutionPairs c@(Constitution _ _) =
  let Constitution {..} = c
   in ["constitutionHash" .= constitutionHash]
        <> ["constitutionScript" .= cScript | SJust cScript <- [constitutionScript]]

deriving instance Eq (Constitution era)

deriving instance Show (Constitution era)

instance Era era => Default (Constitution era) where
  def = Constitution def def

instance Era era => DecCBOR (Constitution era) where
  decCBOR =
    decode $
      RecD Constitution
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance Era era => EncCBOR (Constitution era) where
  encCBOR Constitution {..} =
    encode $
      Rec (Constitution @era)
        !> To constitutionHash
        !> E (encodeNullStrictMaybe encCBOR) constitutionScript

instance Era era => ToCBOR (Constitution era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (Constitution era) where
  fromCBOR = fromEraCBOR @era

instance Era era => NFData (Constitution era)

instance Era era => NoThunks (Constitution era)
