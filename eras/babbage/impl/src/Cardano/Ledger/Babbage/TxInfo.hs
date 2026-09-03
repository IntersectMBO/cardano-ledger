{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (..),
  transReferenceScript,
  transTxOutV1,
  transTxOutV2,
  transTxInInfoV1,
  transTxInInfoV2,
  transRedeemer,
  transTxRedeemers,
  toPlutusV2Args,
  transRedeemerPointerV2V3,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusScriptPurpose,
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  SupportedPlutusRunnable (..),
  lookupTxInfoResultImpossible,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  AlonzoContextError (..),
  toLegacyPlutusArgs,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem)
import Cardano.Ledger.Alonzo.Tx (Data)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, getSpendingDatum)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.BaseTypes (
  Inject (..),
  StrictMaybe (..),
  isSJust,
  kindObjectValue,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  PlutusArgs (..),
  SLanguage (..),
  decodePlutusRunnable,
 )
import Cardano.Ledger.Plutus.TxInfo (
  TxOutSource (..),
  transAddr,
  transCoinToValue,
  transDataHash,
  transScriptHash,
  transTxIn,
  txOutSourceToText,
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxIn (..), txInToText)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Aeson (ToJSON (..), (.=), pattern String)
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2

transReferenceScript ::
  forall era.
  EraScript era =>
  StrictMaybe (Script era) ->
  Maybe PV2.ScriptHash
transReferenceScript SNothing = Nothing
transReferenceScript (SJust s) = Just . transScriptHash . hashScript @era $ s

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Babbage features, return Left.
transTxOutV1 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  TxOutSource ->
  TxOut era ->
  Either (ContextError era) PV1.TxOut
transTxOutV1 txOutSource txOut = do
  when (isSJust (txOut ^. referenceScriptTxOutL)) $ do
    Left $ inject $ ReferenceScriptsNotSupported @era txOutSource
  when (isSJust (txOut ^. dataTxOutL)) $ do
    Left $ inject $ InlineDatumsNotSupported @era txOutSource
  case Alonzo.transTxOut txOut of
    Nothing -> Left $ inject $ ByronTxOutInContext @era txOutSource
    Just plutusTxOut -> Right plutusTxOut

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
transTxOutV2 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  TxOutSource ->
  TxOut era ->
  Either (ContextError era) PV2.TxOut
transTxOutV2 txOutSource txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = transReferenceScript $ txOut ^. referenceScriptTxOutL
      datum =
        case txOut ^. datumTxOutF of
          NoDatum -> PV2.NoOutputDatum
          DatumHash dh -> PV2.OutputDatumHash $ transDataHash dh
          Datum binaryData ->
            PV2.OutputDatum
              . PV2.Datum
              . PV2.dataToBuiltinData
              . getPlutusData
              . binaryDataToData
              $ binaryData
  case transAddr (txOut ^. addrTxOutL) of
    Nothing -> Left $ inject $ ByronTxOutInContext @era txOutSource
    Just addr ->
      Right (PV2.TxOut addr (Alonzo.transValue val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
transTxInInfoV1 ::
  forall era.
  ( Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
transTxInInfoV2 ::
  forall era.
  ( Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV2.TxInInfo
transTxInInfoV2 utxo txIn = do
  txOut <- Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV2.TxInInfo (transTxIn txIn) plutusTxOut)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPointerV2V3 ::
  forall proxy l era.
  ( EraTx era
  , AlonzoEraTxBody era
  , EraPlutusTxInfo l era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  LedgerTxInfo era ->
  (PlutusPurpose AsIx era, (Data era, ExUnits)) ->
  Either (ContextError era) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPointerV2V3 proxy lti@LedgerTxInfo {ltiTx} (ptr, (d, _)) =
  case redeemerPointerInverse (ltiTx ^. bodyTxL) ptr of
    SNothing -> Left $ inject $ RedeemerPointerPointsToNothing ptr
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy lti sp
      Right (plutusScriptPurpose, transRedeemer d)

-- | Translate all `Redeemers` from within a `Tx` into a Map from a `PlutusScriptPurpose`
-- to a `PV2.Redeemer`
transTxRedeemers ::
  ( EraPlutusTxInfo l era
  , AlonzoEraTxBody era
  , EraTx era
  , AlonzoEraTxWits era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  LedgerTxInfo era ->
  Either (ContextError era) (PV2.Map (PlutusScriptPurpose l) PV2.Redeemer)
transTxRedeemers proxy lti@LedgerTxInfo {ltiTx} =
  PV2.unsafeFromList
    <$> mapM
      (transRedeemerPointerV2V3 proxy lti)
      (Map.toList $ ltiTx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)

instance EraPlutusContext BabbageEra where
  type ContextError BabbageEra = BabbageContextError BabbageEra
  data TxInfoResult BabbageEra
    = BabbageTxInfoResult -- Fields must be kept lazy
        (PlutusTxInfoResult 'PlutusV1 BabbageEra)
        (PlutusTxInfoResult 'PlutusV2 BabbageEra)

  mkSupportedLanguage = \case
    PlutusV1 -> Just $ SupportedLanguage SPlutusV1
    PlutusV2 -> Just $ SupportedLanguage SPlutusV2
    _lang -> Nothing

  mkSupportedPlutusRunnable v = \case
    BabbagePlutusV1 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p
    BabbagePlutusV2 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p

  mkTxInfoResult lti =
    BabbageTxInfoResult
      (toPlutusTxInfo SPlutusV1 lti)
      (toPlutusTxInfo SPlutusV2 lti)

  lookupTxInfoResult SPlutusV1 (BabbageTxInfoResult tirPlutusV1 _) = tirPlutusV1
  lookupTxInfoResult SPlutusV2 (BabbageTxInfoResult _ tirPlutusV2) = tirPlutusV2
  lookupTxInfoResult slang _ = lookupTxInfoResultImpossible slang

data BabbageContextError era
  = AlonzoContextError (AlonzoContextError era)
  | ByronTxOutInContext TxOutSource
  | RedeemerPointerPointsToNothing (PlutusPurpose AsIx era)
  | InlineDatumsNotSupported TxOutSource
  | ReferenceScriptsNotSupported TxOutSource
  | ReferenceInputsNotSupported (Set.Set TxIn)
  deriving (Generic)

deriving instance
  (Eq (AlonzoContextError era), Eq (PlutusPurpose AsIx era)) =>
  Eq (BabbageContextError era)

deriving instance
  (Ord (AlonzoContextError era), Ord (PlutusPurpose AsIx era)) =>
  Ord (BabbageContextError era)

deriving instance
  (Show (AlonzoContextError era), Show (PlutusPurpose AsIx era)) =>
  Show (BabbageContextError era)

instance
  ( Era era
  , NFData (TxCert era)
  , NFData (PlutusPurpose AsIx era)
  , NFData (PlutusPurpose AsItem era)
  ) =>
  NFData (BabbageContextError era)

instance Inject (AlonzoContextError era) (BabbageContextError era) where
  inject = AlonzoContextError

instance
  ( Era era
  , EncCBOR (TxCert era)
  , EncCBOR (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsItem era)
  ) =>
  EncCBOR (BabbageContextError era)
  where
  encCBOR = \case
    ByronTxOutInContext txOutSource ->
      encode $ Sum (ByronTxOutInContext @era) 0 !> To txOutSource
    AlonzoContextError (TranslationLogicMissingInput txIn) ->
      encode $ Sum (TranslationLogicMissingInput @era) 1 !> To txIn
    RedeemerPointerPointsToNothing ptr ->
      encode $ Sum RedeemerPointerPointsToNothing 2 !> To ptr
    InlineDatumsNotSupported txOutSource ->
      encode $ Sum (InlineDatumsNotSupported @era) 4 !> To txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      encode $ Sum (ReferenceScriptsNotSupported @era) 5 !> To txOutSource
    ReferenceInputsNotSupported txIns ->
      encode $ Sum (ReferenceInputsNotSupported @era) 6 !> To txIns
    AlonzoContextError (TimeTranslationPastHorizon err) ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err
    AlonzoContextError alonzoError ->
      encode $ Sum AlonzoContextError 8 !> To alonzoError

instance (Era era, DecCBOR (PlutusPurpose AsIx era)) => DecCBOR (BabbageContextError era) where
  decCBOR = decode $ Summands "ContextError" $ \case
    0 -> SumD ByronTxOutInContext <! From
    1 -> SumD (AlonzoContextError . TranslationLogicMissingInput) <! From
    2 -> SumD RedeemerPointerPointsToNothing <! From
    4 -> SumD InlineDatumsNotSupported <! From
    5 -> SumD ReferenceScriptsNotSupported <! From
    6 -> SumD ReferenceInputsNotSupported <! From
    7 -> SumD (AlonzoContextError . TimeTranslationPastHorizon) <! From
    8 -> SumD AlonzoContextError <! From
    n -> Invalid n

instance
  ( ToJSON (TxCert era)
  , ToJSON (PlutusPurpose AsIx era)
  , ToJSON (PlutusPurpose AsItem era)
  ) =>
  ToJSON (BabbageContextError era)
  where
  toJSON = \case
    AlonzoContextError err -> toJSON err
    ByronTxOutInContext txOutSource ->
      String $ "Byron UTxO being created or spent: " <> txOutSourceToText txOutSource
    RedeemerPointerPointsToNothing ptr ->
      kindObjectValue "RedeemerPointerPointsToNothing" ["ptr" .= toJSON ptr]
    InlineDatumsNotSupported txOutSource ->
      String $ "Inline datums not supported, output source: " <> txOutSourceToText txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      String $ "Reference scripts not supported, output source: " <> txOutSourceToText txOutSource
    ReferenceInputsNotSupported txIns ->
      String $
        "Reference inputs not supported: "
          <> T.intercalate ", " (map txInToText (Set.toList txIns))

instance EraPlutusTxInfo 'PlutusV1 BabbageEra where
  toPlutusTxCert _ _ = Alonzo.transTxCert

  toPlutusScriptPurpose proxy lti = Alonzo.transPlutusPurpose proxy (ltiProtVer lti)

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    PlutusTxInfoResult $ withTopTxLevelOnly ltiTx $ \tx -> do
      let
        txBody = tx ^. bodyTxL
        refInputs = txBody ^. referenceInputsTxBodyL
      unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)

      timeRange <-
        Alonzo.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputs <- mapM (transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
      outputs <-
        zipWithM
          (transTxOutV1 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      -- It is important for memoization for `txInfo` to be a let binding
      let
        txInfo =
          PV1.TxInfo
            { PV1.txInfoInputs = inputs
            , PV1.txInfoOutputs = outputs
            , PV1.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
            , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
            , PV1.txInfoDCert = txCerts
            , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
            , PV1.txInfoValidRange = timeRange
            , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
            , PV1.txInfoData = Alonzo.transTxWitsDatums (tx ^. witsTxL)
            , PV1.txInfoId = Alonzo.transTxBodyId txBody
            }
      Right $ \_ -> Right txInfo

  toPlutusArgs = Alonzo.toPlutusV1Args

  toPlutusTxInInfo _ = transTxInInfoV1

instance EraPlutusTxInfo 'PlutusV2 BabbageEra where
  toPlutusTxCert _ _ = Alonzo.transTxCert

  toPlutusScriptPurpose proxy lti = Alonzo.transPlutusPurpose proxy (ltiProtVer lti)

  toPlutusTxInfo proxy lti@LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    PlutusTxInfoResult $ withTopTxLevelOnly ltiTx $ \tx -> do
      let txBody = tx ^. bodyTxL
      timeRange <-
        Alonzo.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
      refInputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
      outputs <-
        zipWithM
          (transTxOutV2 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      plutusRedeemers <- transTxRedeemers proxy lti
      -- It is important for memoization for `txInfo` to be a let binding
      let
        txInfo =
          PV2.TxInfo
            { PV2.txInfoInputs = inputs
            , PV2.txInfoOutputs = outputs
            , PV2.txInfoReferenceInputs = refInputs
            , PV2.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
            , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
            , PV2.txInfoDCert = txCerts
            , PV2.txInfoWdrl = PV2.unsafeFromList $ Alonzo.transTxBodyWithdrawals txBody
            , PV2.txInfoValidRange = timeRange
            , PV2.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
            , PV2.txInfoRedeemers = plutusRedeemers
            , PV2.txInfoData = PV2.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
            , PV2.txInfoId = Alonzo.transTxBodyId txBody
            }
      Right $ \_ -> Right txInfo

  toPlutusArgs = toPlutusV2Args

  toPlutusTxInInfo _ = transTxInInfoV2

toPlutusV2Args ::
  ( AlonzoEraUTxO era
  , EraPlutusTxInfo 'PlutusV2 era
  ) =>
  proxy 'PlutusV2 ->
  LedgerTxInfo era ->
  PV2.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV2)
toPlutusV2Args proxy lti@LedgerTxInfo {..} txInfo scriptPurpose redeemerData =
  PlutusV2Args
    <$> toLegacyPlutusArgs
      proxy
      lti
      (PV2.ScriptContext txInfo)
      scriptPurpose
      maybeSpendingDatum
      redeemerData
  where
    maybeSpendingDatum =
      getSpendingDatum ltiUTxO ltiTx $ hoistPlutusPurpose toAsItem scriptPurpose
