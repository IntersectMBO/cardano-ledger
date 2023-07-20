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
  ShelleyPPUPState (..),
  constitutionHashL,
  constitutionScriptL,
  Constitution (..),
  ConstitutionData (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  FromCBOR (..),
  ToCBOR (..),
  decodeNullStrictMaybe,
  encodeListLen,
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

instance ToExpr (PParamsUpdate era) => ToExpr (ShelleyPPUPState era)

instance Crypto c => EraGovernance (ShelleyEra c) where
  type GovernanceState (ShelleyEra c) = ShelleyPPUPState (ShelleyEra c)

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
