{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxInfo (
  ContextError (..),
  transReferenceScript,
  transTxOutV1,
  transTxOutV2,
  transTxInInfoV1,
  transTxInInfoV2,
  transTxRedeemers,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  PlutusScriptPurpose,
  mkPlutusLanguageContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (ContextError (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Tx (Data, rdptrInv)
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  RdmrPtr,
  unRedeemers,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.Babbage.TxBody (
  AllegraEraTxBody (..),
  AlonzoEraTxOut (..),
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  MaryEraTxBody (..),
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (
  TxOutSource (..),
  transAddr,
  transCoin,
  transDataHash,
  transScriptHash,
  transTxIn,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)
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
  forall era a.
  ( Inject (ContextError (BabbageEra (EraCrypto era))) a
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  TxOutSource (EraCrypto era) ->
  TxOut era ->
  Either a PV1.TxOut
transTxOutV1 txOutSource txOut = do
  when (isSJust (txOut ^. referenceScriptTxOutL)) $
    Left $
      inject $
        ReferenceScriptsNotSupported txOutSource
  when (isSJust (txOut ^. dataTxOutL)) $
    Left $
      inject $
        InlineDatumsNotSupported txOutSource
  case Alonzo.transTxOut txOut of
    Nothing -> Left $ inject $ ByronTxOutInContext txOutSource
    Just plutusTxOut -> Right plutusTxOut

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
transTxOutV2 ::
  forall era a.
  ( Inject (ContextError (BabbageEra (EraCrypto era))) a
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  TxOutSource (EraCrypto era) ->
  TxOut era ->
  Either a PV2.TxOut
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
    Nothing -> Left $ inject $ ByronTxOutInContext txOutSource
    Just addr ->
      Right (PV2.TxOut addr (Alonzo.transValue val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
transTxInInfoV1 ::
  ( Inject (ContextError (AlonzoEra (EraCrypto era))) a
  , Inject (ContextError (BabbageEra (EraCrypto era))) a
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either a PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
transTxInInfoV2 ::
  ( Inject (ContextError (AlonzoEra (EraCrypto era))) a
  , Inject (ContextError (BabbageEra (EraCrypto era))) a
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either a PV2.TxInInfo
transTxInInfoV2 utxo txIn = do
  txOut <- Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV2.TxInInfo (transTxIn txIn) plutusTxOut)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  forall proxy l era.
  ( EraPlutusTxInfo l era
  , MaryEraTxBody era
  , Inject (ContextError (BabbageEra (EraCrypto era))) (ContextError era)
  ) =>
  proxy l ->
  TxBody era ->
  (RdmrPtr, (Data era, ExUnits)) ->
  Either (ContextError era) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPtr proxy txBody (ptr, (d, _)) =
  case rdptrInv txBody ptr of
    SNothing -> Left $ inject $ RdmrPtrPointsToNothing @(EraCrypto era) ptr
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy sp
      Right (plutusScriptPurpose, transRedeemer d)

-- | Translate all `Redeemers` from within a `Tx` into a Map from a `PlutusScriptPurpose`
-- to a `PV2.Redeemer`
transTxRedeemers ::
  ( EraPlutusTxInfo l era
  , MaryEraTxBody era
  , EraTx era
  , AlonzoEraTxWits era
  , Inject (ContextError (BabbageEra (EraCrypto era))) (ContextError era)
  ) =>
  proxy l ->
  Tx era ->
  Either (ContextError era) (PV2.Map (PlutusScriptPurpose l) PV2.Redeemer)
transTxRedeemers proxy tx =
  PV2.fromList
    <$> mapM
      (transRedeemerPtr proxy (tx ^. bodyTxL))
      (Map.toList (unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL))

instance Crypto c => EraPlutusContext (BabbageEra c) where
  data ContextError (BabbageEra c)
    = AlonzoContextError !(ContextError (AlonzoEra c))
    | ByronTxOutInContext !(TxOutSource c)
    | RdmrPtrPointsToNothing !RdmrPtr
    | InlineDatumsNotSupported !(TxOutSource c)
    | ReferenceScriptsNotSupported !(TxOutSource c)
    | ReferenceInputsNotSupported !(Set.Set (TxIn c))
    deriving (Eq, Show, Generic)

  mkPlutusScriptContext = \case
    BabbagePlutusV1 p -> mkPlutusLanguageContext p
    BabbagePlutusV2 p -> mkPlutusLanguageContext p

instance NoThunks (ContextError (BabbageEra c))

instance Crypto c => NFData (ContextError (BabbageEra c))

instance Inject (ContextError (BabbageEra c)) (ContextError (BabbageEra c))

instance Inject (ContextError (AlonzoEra c)) (ContextError (BabbageEra c)) where
  inject = AlonzoContextError

instance Crypto c => EncCBOR (ContextError (BabbageEra c)) where
  encCBOR = \case
    ByronTxOutInContext txOutSource ->
      encode $ Sum ByronTxOutInContext 0 !> To txOutSource
    AlonzoContextError (TranslationLogicMissingInput txIn) ->
      encode $ Sum TranslationLogicMissingInput 1 !> To txIn
    RdmrPtrPointsToNothing ptr ->
      encode $ Sum RdmrPtrPointsToNothing 2 !> To ptr
    InlineDatumsNotSupported txOutSource ->
      encode $ Sum InlineDatumsNotSupported 4 !> To txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      encode $ Sum ReferenceScriptsNotSupported 5 !> To txOutSource
    ReferenceInputsNotSupported txIns ->
      encode $ Sum ReferenceInputsNotSupported 6 !> To txIns
    AlonzoContextError (TimeTranslationPastHorizon err) ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err

instance Crypto c => DecCBOR (ContextError (BabbageEra c)) where
  decCBOR = decode $ Summands "ContextError" $ \case
    0 -> SumD ByronTxOutInContext <! From
    1 -> SumD (AlonzoContextError . TranslationLogicMissingInput) <! From
    2 -> SumD RdmrPtrPointsToNothing <! From
    4 -> SumD InlineDatumsNotSupported <! From
    5 -> SumD ReferenceScriptsNotSupported <! From
    6 -> SumD ReferenceInputsNotSupported <! From
    7 -> SumD (AlonzoContextError . TimeTranslationPastHorizon) <! From
    n -> Invalid n

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    let refInputs = txBody ^. referenceInputsTxBodyL
    unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)

    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV1 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txCerts
        , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV1.txInfoData = Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV1.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- transTxRedeemers proxy tx
    pure
      PV2.TxInfo
        { PV2.txInfoInputs = inputs
        , PV2.txInfoOutputs = outputs
        , PV2.txInfoReferenceInputs = refInputs
        , PV2.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV2.txInfoDCert = txCerts
        , PV2.txInfoWdrl = PV2.fromList $ Alonzo.transTxBodyWithdrawals txBody
        , PV2.txInfoValidRange = timeRange
        , PV2.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV2.txInfoRedeemers = plutusRedeemers
        , PV2.txInfoData = PV2.fromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV2.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV2.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose
