{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Babbage.TxInfo where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (Data, rdptrInv)
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (getTxOutDatum),
    TranslationError (..),
    TxOutSource (..),
    VersionedTxInfo (..),
  )
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness (..), unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..), isSJust)
import Cardano.Ledger.Core as Core (PParams, Script, Tx, TxBody, TxOut, Value)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), Wdrl (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.Monad (unless, when, zipWithM)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as PV1
import Plutus.V1.Ledger.Contexts ()
import qualified Plutus.V2.Ledger.Api as PV2

transScriptHash :: ScriptHash c -> PV2.ScriptHash
transScriptHash (ScriptHash h) = PV2.ScriptHash (PV2.toBuiltin (hashToBytes h))

transReferenceScript ::
  forall era.
  ValidateScript era =>
  StrictMaybe (Core.Script era) ->
  Maybe PV2.ScriptHash
transReferenceScript SNothing = Nothing
transReferenceScript (SJust s) = Just . transScriptHash . hashScript @era $ s

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Babbage features, return Left.
txInfoOutV1 ::
  forall era.
  ( Era era,
    ExtendedUTxO era,
    ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  TxOutSource (Crypto era) ->
  Core.TxOut era ->
  Either (TranslationError (Crypto era)) PV1.TxOut
txInfoOutV1 os txout = do
  let val = getField @"value" txout
      referenceScript = getField @"referenceScript" txout
  when (isSJust referenceScript) $ Left $ ReferenceScriptsNotSupported os
  datahash <-
    case getTxOutDatum txout of
      NoDatum -> Right SNothing
      DatumHash dh -> Right $ SJust dh
      Datum _ -> Left $ InlineDatumsNotSupported os
  addr <-
    case Alonzo.transTxOutAddr txout of
      Nothing -> Left (ByronTxOutInContext os)
      Just addr -> Right addr
  Right (PV1.TxOut addr (Alonzo.transValue @(Crypto era) val) (Alonzo.transDataHash datahash))

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
txInfoOutV2 ::
  forall era.
  ( Era era,
    ExtendedUTxO era,
    ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  TxOutSource (Crypto era) ->
  Core.TxOut era ->
  Either (TranslationError (Crypto era)) PV2.TxOut
txInfoOutV2 os txout = do
  let val = getField @"value" txout
      referenceScript = transReferenceScript @era $ getField @"referenceScript" txout
      datum =
        case getTxOutDatum txout of
          NoDatum -> PV2.NoOutputDatum
          DatumHash dh -> PV2.OutputDatumHash $ Alonzo.transDataHash' dh
          Datum binaryData ->
            PV2.OutputDatum . PV2.Datum
              . PV2.dataToBuiltinData
              . getPlutusData
              . binaryDataToData
              $ binaryData
  case Alonzo.transTxOutAddr txout of
    Nothing -> Left (ByronTxOutInContext os)
    Just ad ->
      Right (PV2.TxOut ad (Alonzo.transValue @(Crypto era) val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV1 ::
  forall era.
  ( ValidateScript era,
    ExtendedUTxO era,
    Value era ~ Mary.Value (Crypto era),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Either (TranslationError (Crypto era)) PV1.TxInInfo
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
  ( ValidateScript era,
    ExtendedUTxO era,
    Value era ~ Mary.Value (Crypto era),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Either (TranslationError (Crypto era)) PV2.TxInInfo
txInfoInV2 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV2 (TxOutFromInput txin) txout
      Right (PV2.TxInInfo (Alonzo.txInfoIn' txin) out)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  Core.TxBody era ->
  (RdmrPtr, (Data era, ExUnits)) ->
  Either (TranslationError (Crypto era)) (PV2.ScriptPurpose, PV2.Redeemer)
transRedeemerPtr txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left (RdmrPtrPointsToNothing ptr)
    SJust sp -> Right (Alonzo.transScriptPurpose sp, transRedeemer d)

babbageTxInfo ::
  forall era.
  ( Era era,
    ExtendedUTxO era,
    ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "wits" (Core.Tx era) (TxWitness era),
    HasField "referenceScript" (TxOut era) (StrictMaybe (Core.Script era)),
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "referenceInputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "mint" (Core.TxBody era) (Mary.Value (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval
  ) =>
  Core.PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Core.Tx era ->
  Either (TranslationError (Crypto era)) VersionedTxInfo
babbageTxInfo pp lang ei sysS utxo tx = do
  timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
  case lang of
    PlutusV1 -> do
      let refInputs = getField @"referenceInputs" tbody
      unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)
      inputs <- mapM (txInfoInV1 utxo) (Set.toList (getField @"inputs" tbody))
      outputs <-
        zipWithM
          (\txIx -> txInfoOutV1 (TxOutFromOutput txIx))
          [minBound ..]
          (foldr (:) [] outs)
      pure . TxInfoPV1 $
        PV1.TxInfo
          { PV1.txInfoInputs = inputs,
            PV1.txInfoOutputs = outputs,
            PV1.txInfoFee = Alonzo.transValue (inject @(Mary.Value (Crypto era)) fee),
            PV1.txInfoMint = Alonzo.transValue forge,
            PV1.txInfoDCert = foldr (\c ans -> Alonzo.transDCert c : ans) [] (getField @"certs" tbody),
            PV1.txInfoWdrl = Map.toList (Alonzo.transWdrl (getField @"wdrls" tbody)),
            PV1.txInfoValidRange = timeRange,
            PV1.txInfoSignatories = map Alonzo.transKeyHash (Set.toList (getField @"reqSignerHashes" tbody)),
            PV1.txInfoData = map Alonzo.transDataPair datpairs,
            PV1.txInfoId = PV1.TxId (Alonzo.transSafeHash (hashAnnotated @(Crypto era) tbody))
          }
    PlutusV2 -> do
      inputs <- mapM (txInfoInV2 utxo) (Set.toList (getField @"inputs" tbody))
      refInputs <- mapM (txInfoInV2 utxo) (Set.toList (getField @"referenceInputs" tbody))
      outputs <-
        zipWithM
          (\txIx -> txInfoOutV2 (TxOutFromOutput txIx))
          [minBound ..]
          (foldr (:) [] outs)
      rdmrs' <- mapM (transRedeemerPtr tbody) rdmrs
      pure . TxInfoPV2 $
        PV2.TxInfo
          { PV2.txInfoInputs = inputs,
            PV2.txInfoOutputs = outputs,
            PV2.txInfoReferenceInputs = refInputs,
            PV2.txInfoFee = Alonzo.transValue (inject @(Mary.Value (Crypto era)) fee),
            PV2.txInfoMint = Alonzo.transValue forge,
            PV2.txInfoDCert = foldr (\c ans -> Alonzo.transDCert c : ans) [] (getField @"certs" tbody),
            PV2.txInfoWdrl = PV2.fromList $ Map.toList (Alonzo.transWdrl (getField @"wdrls" tbody)),
            PV2.txInfoValidRange = timeRange,
            PV2.txInfoSignatories = map Alonzo.transKeyHash (Set.toList (getField @"reqSignerHashes" tbody)),
            PV2.txInfoRedeemers = PV2.fromList rdmrs',
            PV2.txInfoData = PV2.fromList $ map Alonzo.transDataPair datpairs,
            PV2.txInfoId = PV2.TxId (Alonzo.transSafeHash (hashAnnotated @(Crypto era) tbody))
          }
  where
    tbody :: Core.TxBody era
    tbody = getField @"body" tx
    witnesses = getField @"wits" tx
    outs = getField @"outputs" tbody
    fee = getField @"txfee" tbody
    forge = getField @"mint" tbody
    interval = getField @"vldt" tbody

    datpairs = Map.toList (unTxDats $ txdats' witnesses)
    rdmrs = Map.toList (unRedeemers $ txrdmrs' witnesses)
