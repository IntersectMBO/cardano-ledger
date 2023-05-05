{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.TranslationInstance (
  TranslationInstance (..),
  translationInstances,
  epochInfo,
  systemStart,
  deserializeTranslationInstances,
) where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Language (Language (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)

import Test.QuickCheck (
  Gen,
  arbitrary,
  generate,
  suchThat,
  vectorOf,
 )

import Cardano.Ledger.Core
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Ledger.Alonzo.TxInfo (VersionedTxInfo, alonzoTxInfo)
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V3 as PV3

-- | Represents arguments passed to `alonzoTxInfo` along with the produced result.
data TranslationInstance = TranslationInstance
  { tiPparams :: PParams Alonzo
  , tiUtxo :: UTxO Alonzo
  , tiTx :: AlonzoTx Alonzo
  , tiResult :: VersionedTxInfo
  }
  deriving (Show, Eq, Generic)

translationInstances :: Int -> IO [TranslationInstance]
translationInstances size =
  generate $ vectorOf size genTranslationInstance

genTranslationInstance :: Gen TranslationInstance
genTranslationInstance = do
  pp <- genAlonzoPParams @(EraCrypto Alonzo) defaultConstants
  utxo <- arbitrary :: Gen (UTxO Alonzo)
  tx <- validTx
  let fullUtxo = utxoWithTx tx utxo
  let vtxInfoE = alonzoTxInfo pp PlutusV1 epochInfo systemStart fullUtxo tx
  let vtxInfo = either (error . show) id vtxInfoE
  pure $ TranslationInstance pp fullUtxo tx vtxInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

validTx :: Gen (AlonzoTx Alonzo)
validTx = suchThat (arbitrary :: Gen (AlonzoTx Alonzo)) $ \tx ->
  let txBody = tx ^. bodyTxL
      txIns = txBody ^. inputsTxBodyL
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in (Set.size txIns > 0 && length txOuts >= Set.size txIns)

utxoWithTx :: AlonzoTx Alonzo -> UTxO Alonzo -> UTxO Alonzo
utxoWithTx tx utxo =
  let txBody = tx ^. bodyTxL
      txIns = Set.toList $ txBody ^. inputsTxBodyL
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in utxo <> UTxO (Map.fromList $ txIns `zip` txOuts)

instance Cborg.Serialise PV1.TxInfo
instance Cborg.Serialise PV1.TxInInfo
instance Cborg.Serialise PV1.TxOut
instance Cborg.Serialise PV3.POSIXTime
instance Cborg.Serialise a => Cborg.Serialise (PV3.Extended a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.LowerBound a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.UpperBound a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.Interval a)
instance Cborg.Serialise PV3.Address
instance Cborg.Serialise PV3.Credential
instance Cborg.Serialise PV3.CurrencySymbol
instance Cborg.Serialise PV3.DCert
instance Cborg.Serialise PV3.TxOutRef
instance Cborg.Serialise PV3.TxId
instance Cborg.Serialise PV3.Value
instance Cborg.Serialise PV3.PubKeyHash
instance (Cborg.Serialise k, Cborg.Serialise v) => Cborg.Serialise (PV3.Map k v)
instance Cborg.Serialise PV3.TokenName
instance Cborg.Serialise PV3.TxInInfo
instance Cborg.Serialise PV3.DatumHash
instance Cborg.Serialise PV3.StakingCredential
instance Cborg.Serialise PV3.ScriptHash
instance Cborg.Serialise PV3.TxOut
instance Cborg.Serialise PV3.OutputDatum
instance Cborg.Serialise PV3.ScriptPurpose
instance Cborg.Serialise PV3.TxInfo
instance Cborg.Serialise VersionedTxInfo

instance EncCBOR VersionedTxInfo where
  encCBOR = fromPlainEncoding . Cborg.encode

instance DecCBOR VersionedTxInfo where
  decCBOR = fromPlainDecoder Cborg.decode

instance EncCBOR TranslationInstance where
  encCBOR (TranslationInstance pp u tx r) =
    encode $
      Rec TranslationInstance
        !> To pp
        !> To u
        !> To tx
        !> To r

instance DecCBOR (Annotator TranslationInstance) where
  decCBOR =
    decode $
      Ann (RecD TranslationInstance)
        <*! Ann From
        <*! Ann From
        <*! From
        <*! Ann From

deserializeTranslationInstances :: BSL.ByteString -> Either DecoderError [TranslationInstance]
deserializeTranslationInstances = decodeFullAnnotator (eraProtVerHigh @Alonzo) "Translations" decList
  where
    decList = sequence <$> decodeList decCBOR
