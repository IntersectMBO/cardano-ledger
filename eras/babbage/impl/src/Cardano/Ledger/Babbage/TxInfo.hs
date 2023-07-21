{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxInfo where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Alonzo.Tx (Data, rdptrInv)
import Cardano.Ledger.Alonzo.TxInfo (
  EraPlutusContext,
  PlutusTxCert (..),
  TranslationError (..),
  TxOutSource (..),
  VersionedTxInfo (..),
  transShelleyTxCert,
  unTxCertV1,
  unTxCertV2,
 )
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  RdmrPtr,
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (
  AllegraEraTxBody (..),
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  MaryEraTxBody (..),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.Monad (unless, when, zipWithM)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import PlutusLedgerApi.V1.Contexts ()
import qualified PlutusLedgerApi.V2 as PV2

transScriptHash :: ScriptHash c -> PV2.ScriptHash
transScriptHash (ScriptHash h) = PV2.ScriptHash (PV2.toBuiltin (hashToBytes h))

transReferenceScript ::
  forall era.
  EraScript era =>
  StrictMaybe (Script era) ->
  Maybe PV2.ScriptHash
transReferenceScript SNothing = Nothing
transReferenceScript (SJust s) = Just . transScriptHash . hashScript @era $ s

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Babbage features, return Left.
txInfoOutV1 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TxOutSource (EraCrypto era) ->
  TxOut era ->
  Either (TranslationError (EraCrypto era)) PV1.TxOut
txInfoOutV1 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = txOut ^. referenceScriptTxOutL
  when (isSJust referenceScript) $ Left $ ReferenceScriptsNotSupported os
  datahash <-
    case txOut ^. datumTxOutF of
      NoDatum -> Right SNothing
      DatumHash dh -> Right $ SJust dh
      Datum _ -> Left $ InlineDatumsNotSupported os
  addr <-
    case Alonzo.transTxOutAddr txOut of
      Nothing -> Left (ByronTxOutInContext os)
      Just addr -> Right addr
  Right (PV1.TxOut addr (Alonzo.transValue @(EraCrypto era) val) (Alonzo.transDataHash datahash))

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
txInfoOutV2 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TxOutSource (EraCrypto era) ->
  TxOut era ->
  Either (TranslationError (EraCrypto era)) PV2.TxOut
txInfoOutV2 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = transReferenceScript @era $ txOut ^. referenceScriptTxOutL
      datum =
        case txOut ^. datumTxOutF of
          NoDatum -> PV2.NoOutputDatum
          DatumHash dh -> PV2.OutputDatumHash $ Alonzo.transDataHash' dh
          Datum binaryData ->
            PV2.OutputDatum
              . PV2.Datum
              . PV2.dataToBuiltinData
              . getPlutusData
              . binaryDataToData
              $ binaryData
  case Alonzo.transTxOutAddr txOut of
    Nothing -> Left (ByronTxOutInContext os)
    Just ad ->
      Right (PV2.TxOut ad (Alonzo.transValue @(EraCrypto era) val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV1 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (TranslationError (EraCrypto era)) PV1.TxInInfo
txInfoInV1 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV1 (TxOutFromInput txin) txout
      Right (PV1.TxInInfo (Alonzo.txInfoIn' txin) out)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV2 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (TranslationError (EraCrypto era)) PV2.TxInInfo
txInfoInV2 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV2 (TxOutFromInput txin) txout
      Right (PV2.TxInInfo (Alonzo.txInfoIn' txin) out)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  ( MaryEraTxBody era
  , Alonzo.EraPlutusContext 'PlutusV1 era
  ) =>
  TxBody era ->
  (RdmrPtr, (Data era, ExUnits)) ->
  Either (TranslationError (EraCrypto era)) (PV2.ScriptPurpose, PV2.Redeemer)
transRedeemerPtr txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left (RdmrPtrPointsToNothing ptr)
    SJust sp -> Right (Alonzo.transScriptPurpose sp, transRedeemer d)

instance Crypto c => EraPlutusContext 'PlutusV1 (BabbageEra c) where
  transTxCert = TxCertPlutusV1 . transShelleyTxCert

instance Crypto c => EraPlutusContext 'PlutusV2 (BabbageEra c) where
  transTxCert = TxCertPlutusV2 . transShelleyTxCert

babbageTxInfo ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , Alonzo.EraPlutusContext 'PlutusV1 era
  , Alonzo.EraPlutusContext 'PlutusV2 era
  ) =>
  PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
babbageTxInfo pp lang ei sysS utxo tx = do
  timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
  case lang of
    PlutusV1 -> babbageTxInfoV1 timeRange tx utxo
    PlutusV2 -> babbageTxInfoV2 timeRange tx utxo
    _ -> Left $ LanguageNotSupported lang
  where
    interval = tx ^. bodyTxL ^. vldtTxBodyL

babbageTxInfoV1 ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PV1.POSIXTimeRange ->
  Tx era ->
  UTxO era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
babbageTxInfoV1 timeRange tx utxo = do
  let refInputs = txBody ^. referenceInputsTxBodyL
  unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)
  inputs <- mapM (txInfoInV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
  outputs <-
    zipWithM
      (txInfoOutV1 . TxOutFromOutput)
      [minBound ..]
      (foldr (:) [] outs)
  pure . TxInfoPV1 $
    PV1.TxInfo
      { PV1.txInfoInputs = inputs
      , PV1.txInfoOutputs = outputs
      , PV1.txInfoFee = Alonzo.transValue (inject @(MaryValue (EraCrypto era)) fee)
      , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
      , PV1.txInfoDCert = toList $ fmap (unTxCertV1 . Alonzo.transTxCert) (txBody ^. certsTxBodyL)
      , PV1.txInfoWdrl = Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
      , PV1.txInfoValidRange = timeRange
      , PV1.txInfoSignatories =
          map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
      , PV1.txInfoData = map Alonzo.transDataPair datpairs
      , PV1.txInfoId = PV1.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
      }
  where
    txBody = tx ^. bodyTxL
    witnesses = tx ^. witsTxL
    outs = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)

babbageTxInfoV2 ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusContext 'PlutusV2 era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PV2.POSIXTimeRange ->
  Tx era ->
  UTxO era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
babbageTxInfoV2 timeRange tx utxo = do
  inputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
  refInputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
  outputs <-
    zipWithM
      (txInfoOutV2 . TxOutFromOutput)
      [minBound ..]
      (foldr (:) [] outs)
  rdmrs' <- mapM (transRedeemerPtr txBody) rdmrs
  pure . TxInfoPV2 $
    PV2.TxInfo
      { PV2.txInfoInputs = inputs
      , PV2.txInfoOutputs = outputs
      , PV2.txInfoReferenceInputs = refInputs
      , PV2.txInfoFee = Alonzo.transValue (inject @(MaryValue (EraCrypto era)) fee)
      , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
      , PV2.txInfoDCert = toList $ fmap (unTxCertV2 . Alonzo.transTxCert) (txBody ^. certsTxBodyL)
      , PV2.txInfoWdrl = PV2.fromList $ Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
      , PV2.txInfoValidRange = timeRange
      , PV2.txInfoSignatories =
          map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
      , PV2.txInfoRedeemers = PV2.fromList rdmrs'
      , PV2.txInfoData = PV2.fromList $ map Alonzo.transDataPair datpairs
      , PV2.txInfoId = PV2.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
      }
  where
    txBody = tx ^. bodyTxL
    witnesses = tx ^. witsTxL
    outs = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)
    rdmrs = Map.toList (unRedeemers $ witnesses ^. rdmrsTxWitsL)
