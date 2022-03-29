{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Babbage.TxInfo where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (Data, DataHash, rdptrInv)
import Cardano.Ledger.Alonzo.TxInfo (TranslationError (..), VersionedTxInfo (..))
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness (..), unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.Core as Core (PParams, Script, Tx, TxBody, TxOut, Value)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentData)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad (unless)
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as PV1
import Plutus.V1.Ledger.Contexts ()
import qualified Plutus.V2.Ledger.Api as PV2

transScriptHash :: ScriptHash c -> PV2.ScriptHash
transScriptHash (ScriptHash h) = PV2.ScriptHash (PV2.toBuiltin (hashToBytes h))

transReferenceScript :: forall era. ValidateScript era => StrictMaybe (Core.Script era) -> Maybe PV2.ScriptHash
transReferenceScript SNothing = Nothing
transReferenceScript (SJust s) = Just . transScriptHash . hashScript @era $ s

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Babbage features, return Left.
txInfoOutV1 ::
  forall era.
  ( Era era,
    ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  Core.TxOut era ->
  Either TranslationError PV1.TxOut
txInfoOutV1 txout =
  let val = getField @"value" txout
      datahash = getField @"datahash" txout
      inlineDatum = getField @"datum" txout
      referenceScript = transReferenceScript @era $ getField @"referenceScript" txout
   in case (Alonzo.transTxOutAddr txout, inlineDatum, referenceScript) of
        (Nothing, _, _) -> Left ByronOutputInContext
        (_, SJust _, _) -> Left InlineDatumsNotSupported
        (_, _, Just _) -> Left ReferenceScriptsNotSupported
        (Just ad, SNothing, Nothing) ->
          Right (PV1.TxOut ad (Alonzo.transValue @(Crypto era) val) (Alonzo.transDataHash datahash))

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
txInfoOutV2 ::
  forall era.
  ( Era era,
    ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  Core.TxOut era ->
  Either TranslationError PV2.TxOut
txInfoOutV2 txout =
  let val = getField @"value" txout
      d = case (getField @"datahash" txout, getField @"datum" txout) of
        (SNothing, SNothing) -> Right PV2.NoOutputDatum
        (SJust dh, SNothing) -> Right . PV2.OutputDatumHash $ Alonzo.transDataHash' dh
        (SNothing, SJust d') ->
          Right . PV2.OutputDatum . PV2.Datum . PV2.dataToBuiltinData . getPlutusData $ d'
        (SJust _, SJust _) -> Left TranslationLogicErrorDoubleDatum
      referenceScript = transReferenceScript @era $ getField @"referenceScript" txout
   in case (Alonzo.transTxOutAddr txout, d) of
        (_, Left e) -> Left e
        (Nothing, _) -> Left ByronOutputInContext
        (Just ad, Right d') ->
          Right (PV2.TxOut ad (Alonzo.transValue @(Crypto era) val) d' referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV1 ::
  forall era.
  ( ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Either TranslationError PV1.TxInInfo
txInfoInV1 (UTxO mp) txin =
  case SplitMap.lookup txin mp of
    Nothing -> Left TranslationLogicErrorInput
    Just txout -> do
      out <- txInfoOutV1 txout
      Right (PV1.TxInInfo (Alonzo.txInfoIn' txin) out)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV2 ::
  forall era.
  ( ValidateScript era,
    Value era ~ Mary.Value (Crypto era),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Either TranslationError PV2.TxInInfo
txInfoInV2 (UTxO mp) txin =
  case SplitMap.lookup txin mp of
    Nothing -> Left TranslationLogicErrorInput
    Just txout -> do
      out <- txInfoOutV2 txout
      Right (PV2.TxInInfo (Alonzo.txInfoIn' txin) out)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  (Core.TxBody era) ->
  (RdmrPtr, (Data era, ExUnits)) ->
  Either TranslationError (PV2.ScriptPurpose, PV2.Redeemer)
transRedeemerPtr txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left TranslationLogicErrorRedeemer
    SJust sp -> Right (Alonzo.transScriptPurpose sp, transRedeemer d)

babbageTxInfo ::
  forall era m.
  ( Era era,
    ValidateScript era,
    Monad m,
    Value era ~ Mary.Value (Crypto era),
    HasField "wits" (Core.Tx era) (TxWitness era),
    HasField "datahash" (TxOut era) (StrictMaybe (SafeHash (Crypto era) EraIndependentData)),
    HasField "datum" (TxOut era) (StrictMaybe (Data era)),
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
  EpochInfo m ->
  SystemStart ->
  UTxO era ->
  Core.Tx era ->
  m (Either TranslationError VersionedTxInfo)
babbageTxInfo pp lang ei sysS utxo tx = do
  timeRange <- Alonzo.transVITime pp ei sysS interval
  pure $
    case lang of
      PlutusV1 -> do
        unless (Set.null $ getField @"referenceInputs" tbody) (Left ReferenceInputsNotSupported)
        inputs <- mapM (txInfoInV1 utxo) (Set.toList (getField @"inputs" tbody))
        outputs <- mapM txInfoOutV1 (foldr (:) [] outs)
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
        outputs <- mapM txInfoOutV2 (foldr (:) [] outs)
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
