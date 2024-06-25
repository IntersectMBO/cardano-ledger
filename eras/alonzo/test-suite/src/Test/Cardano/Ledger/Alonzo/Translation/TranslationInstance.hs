{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  deserializeTranslationInstances,
  VersionedTxInfo (..),
) where

import Cardano.Ledger.Plutus.Language (Language (..))

import Cardano.Ledger.Core as Core

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecoderError,
  EncCBOR (..),
  decodeFullAnnotator,
  decodeList,
  fromPlainDecoder,
  fromPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<*!),
 )
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Codec.Serialise as Cborg (Serialise (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4

data VersionedTxInfo
  = TxInfoPV1 PV1.TxInfo
  | TxInfoPV2 PV2.TxInfo
  | TxInfoPV3 PV3.TxInfo
  | TxInfoPV4 PV4.TxInfo
  deriving (Show, Eq, Generic)

-- | Represents arguments passed to `alonzoTxInfo` along with the produced result.
data TranslationInstance era = TranslationInstance
  { tiPparams :: PParams era
  , tiLanguage :: Language
  , tiUtxo :: UTxO era
  , tiTx :: Core.Tx era
  , tiResult :: VersionedTxInfo
  }
  deriving (Generic)

deriving instance
  (Era era, Eq (PParams era), Eq (UTxO era), Eq (Core.Tx era)) => Eq (TranslationInstance era)
deriving instance
  (Era era, Show (PParams era), Show (UTxO era), Show (Core.Tx era)) => Show (TranslationInstance era)

instance Cborg.Serialise PV1.DCert
instance Cborg.Serialise PV1.TxInInfo
instance Cborg.Serialise PV1.TxInfo
instance Cborg.Serialise PV1.TxOut
instance Cborg.Serialise PV2.ScriptPurpose
instance Cborg.Serialise PV2.TxId
instance Cborg.Serialise PV2.TxOutRef
instance Cborg.Serialise PV2.TxInInfo
instance Cborg.Serialise PV2.TxInfo
instance Cborg.Serialise PV3.Address
instance Cborg.Serialise PV3.BuiltinData
instance Cborg.Serialise PV3.ChangedParameters
instance Cborg.Serialise PV3.ColdCommitteeCredential
instance Cborg.Serialise PV3.Committee
instance Cborg.Serialise PV3.Constitution
instance Cborg.Serialise PV3.Credential
instance Cborg.Serialise PV3.CurrencySymbol
instance Cborg.Serialise PV3.DRep
instance Cborg.Serialise PV3.DRepCredential
instance Cborg.Serialise PV3.DatumHash
instance Cborg.Serialise PV3.Delegatee
instance Cborg.Serialise PV3.GovernanceAction
instance Cborg.Serialise PV3.GovernanceActionId
instance Cborg.Serialise PV3.HotCommitteeCredential
instance Cborg.Serialise PV3.Lovelace
instance Cborg.Serialise PV3.OutputDatum
instance Cborg.Serialise PV3.POSIXTime
instance Cborg.Serialise PV3.ProposalProcedure
instance Cborg.Serialise PV3.ProtocolVersion
instance Cborg.Serialise PV3.PubKeyHash
instance Cborg.Serialise PV3.ScriptHash
instance Cborg.Serialise PV3.ScriptPurpose
instance Cborg.Serialise PV3.StakingCredential
instance Cborg.Serialise PV3.TokenName
instance Cborg.Serialise PV3.TxCert
instance Cborg.Serialise PV3.TxId
instance Cborg.Serialise PV3.TxInInfo
instance Cborg.Serialise PV3.TxInfo
instance Cborg.Serialise PV3.TxOut
instance Cborg.Serialise PV3.TxOutRef
instance Cborg.Serialise PV3.Value
instance Cborg.Serialise PV3.Vote
instance Cborg.Serialise PV3.Voter
instance (Cborg.Serialise k, Cborg.Serialise v) => Cborg.Serialise (PV3.Map k v)
instance Cborg.Serialise a => Cborg.Serialise (PV3.Extended a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.Interval a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.LowerBound a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.UpperBound a)
instance Cborg.Serialise PV3.Rational
instance Cborg.Serialise PV4.ChangedParameters
instance Cborg.Serialise PV4.ColdCommitteeCredential
instance Cborg.Serialise PV4.Committee
instance Cborg.Serialise PV4.Constitution
instance Cborg.Serialise PV4.DRep
instance Cborg.Serialise PV4.DRepCredential
instance Cborg.Serialise PV4.Delegatee
instance Cborg.Serialise PV4.GovernanceAction
instance Cborg.Serialise PV4.GovernanceActionId
instance Cborg.Serialise PV4.HotCommitteeCredential
instance Cborg.Serialise PV4.ProposalProcedure
instance Cborg.Serialise PV4.ProtocolVersion
instance Cborg.Serialise PV4.ScriptPurpose
instance Cborg.Serialise PV4.TxCert
instance Cborg.Serialise PV4.TxId
instance Cborg.Serialise PV4.TxInInfo
instance Cborg.Serialise PV4.TxInfo
instance Cborg.Serialise PV4.TxOutRef
instance Cborg.Serialise PV4.Vote
instance Cborg.Serialise PV4.Voter

instance Cborg.Serialise VersionedTxInfo

instance EncCBOR VersionedTxInfo where
  encCBOR = fromPlainEncoding . Cborg.encode

instance DecCBOR VersionedTxInfo where
  decCBOR = fromPlainDecoder Cborg.decode

instance
  ( Typeable era
  , EncCBOR (PParams era)
  , EncCBOR (UTxO era)
  , EncCBOR (Core.Tx era)
  ) =>
  EncCBOR (TranslationInstance era)
  where
  encCBOR (TranslationInstance pp l u tx r) =
    encode $
      Rec TranslationInstance
        !> To pp
        !> To l
        !> To u
        !> To tx
        !> To r

instance
  ( Typeable era
  , DecCBOR (PParams era)
  , DecCBOR (UTxO era)
  , DecCBOR (Annotator (Core.Tx era))
  ) =>
  DecCBOR (Annotator (TranslationInstance era))
  where
  decCBOR =
    decode $
      Ann (RecD TranslationInstance)
        <*! Ann From
        <*! Ann From
        <*! Ann From
        <*! From
        <*! Ann From

deserializeTranslationInstances ::
  forall era.
  ( Era era
  , DecCBOR (PParams era)
  , DecCBOR (UTxO era)
  , DecCBOR (Annotator (Core.Tx era))
  ) =>
  BSL.ByteString ->
  Either DecoderError [TranslationInstance era]
deserializeTranslationInstances = decodeFullAnnotator (eraProtVerHigh @era) "Translations" decList
  where
    decList = sequence <$> decodeList decCBOR
