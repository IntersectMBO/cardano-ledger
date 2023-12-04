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

module Cardano.Ledger.Babbage.TxInfo where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  mkPlutusLanguageContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Tx (Data, rdptrInv)
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  RdmrPtr,
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.Babbage.TxBody (
  AllegraEraTxBody (..),
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  MaryEraTxBody (..),
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
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
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (PlutusScriptPurpose, transCoin, transScriptHash)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
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
txInfoOutV1 ::
  forall c.
  Crypto c =>
  TxOutSource c ->
  TxOut (BabbageEra c) ->
  Either (ContextError (BabbageEra c)) PV1.TxOut
txInfoOutV1 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = txOut ^. referenceScriptTxOutL
  when (isSJust referenceScript) $ Left $ ReferenceScriptsNotSupported os
  dataHash <-
    case txOut ^. datumTxOutF of
      NoDatum -> Right Nothing
      DatumHash dh -> Right $ Just $ Alonzo.transDataHash dh
      Datum _ -> Left $ InlineDatumsNotSupported os
  addr <-
    case Alonzo.transTxOutAddr txOut of
      Nothing -> Left (ByronTxOutInContext os)
      Just addr -> Right addr
  Right (PV1.TxOut addr (Alonzo.transValue @c val) dataHash)

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
txInfoOutV2 ::
  forall c.
  Crypto c =>
  TxOutSource c ->
  TxOut (BabbageEra c) ->
  Either (ContextError (BabbageEra c)) PV2.TxOut
txInfoOutV2 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = transReferenceScript $ txOut ^. referenceScriptTxOutL
      datum =
        case txOut ^. datumTxOutF of
          NoDatum -> PV2.NoOutputDatum
          DatumHash dh -> PV2.OutputDatumHash $ Alonzo.transDataHash dh
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
      Right (PV2.TxOut ad (Alonzo.transValue @c val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV1 ::
  Crypto c =>
  UTxO (BabbageEra c) ->
  TxIn c ->
  Either (ContextError (BabbageEra c)) PV1.TxInInfo
txInfoInV1 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV1 (TxOutFromInput txin) txout
      Right (PV1.TxInInfo (Alonzo.txInfoIn' txin) out)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV2 ::
  Crypto c =>
  UTxO (BabbageEra c) ->
  TxIn c ->
  Either (ContextError (BabbageEra c)) PV2.TxInInfo
txInfoInV2 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV2 (TxOutFromInput txin) txout
      Right (PV2.TxInInfo (Alonzo.txInfoIn' txin) out)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  EraPlutusTxInfo l (BabbageEra c) =>
  proxy l ->
  TxBody (BabbageEra c) ->
  (RdmrPtr, (Data (BabbageEra c), ExUnits)) ->
  Either (ContextError (BabbageEra c)) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPtr proxy txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left (RdmrPtrPointsToNothing ptr)
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy sp
      Right (plutusScriptPurpose, transRedeemer d)

instance Crypto c => EraPlutusContext (BabbageEra c) where
  data ContextError (BabbageEra c)
    = ByronTxOutInContext !(TxOutSource c)
    | TranslationLogicMissingInput !(TxIn c)
    | RdmrPtrPointsToNothing !RdmrPtr
    | InlineDatumsNotSupported !(TxOutSource c)
    | ReferenceScriptsNotSupported !(TxOutSource c)
    | ReferenceInputsNotSupported !(Set.Set (TxIn c))
    | TimeTranslationPastHorizon !Text
    deriving (Eq, Show, Generic)

  mkPlutusScriptContext = \case
    BabbagePlutusV1 p -> mkPlutusLanguageContext p
    BabbagePlutusV2 p -> mkPlutusLanguageContext p

instance NoThunks (ContextError (BabbageEra c))

instance Crypto c => NFData (ContextError (BabbageEra c))

instance Crypto c => EncCBOR (ContextError (BabbageEra c)) where
  encCBOR = \case
    ByronTxOutInContext txOutSource ->
      encode $ Sum ByronTxOutInContext 0 !> To txOutSource
    TranslationLogicMissingInput txIn ->
      encode $ Sum TranslationLogicMissingInput 1 !> To txIn
    RdmrPtrPointsToNothing ptr ->
      encode $ Sum RdmrPtrPointsToNothing 2 !> To ptr
    InlineDatumsNotSupported txOutSource ->
      encode $ Sum InlineDatumsNotSupported 4 !> To txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      encode $ Sum ReferenceScriptsNotSupported 5 !> To txOutSource
    ReferenceInputsNotSupported txIns ->
      encode $ Sum ReferenceInputsNotSupported 6 !> To txIns
    TimeTranslationPastHorizon err ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err

instance Crypto c => DecCBOR (ContextError (BabbageEra c)) where
  decCBOR = decode $ Summands "ContextError" $ \case
    0 -> SumD ByronTxOutInContext <! From
    1 -> SumD TranslationLogicMissingInput <! From
    2 -> SumD RdmrPtrPointsToNothing <! From
    4 -> SumD InlineDatumsNotSupported <! From
    5 -> SumD ReferenceScriptsNotSupported <! From
    6 -> SumD ReferenceInputsNotSupported <! From
    7 -> SumD TimeTranslationPastHorizon <! From
    n -> Invalid n

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo _proxy pp ei sysS utxo tx = do
    let refInputs = txBody ^. referenceInputsTxBodyL
        interval = tx ^. bodyTxL . vldtTxBodyL
    timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
    unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)
    inputs <- mapM (txInfoInV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    outputs <-
      zipWithM
        (txInfoOutV1 . TxOutFromOutput)
        [minBound ..]
        (foldr (:) [] outs)
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = toList $ fmap (Alonzo.transTxCert) (txBody ^. certsTxBodyL)
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
      datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp ei sysS utxo tx = do
    let interval = tx ^. bodyTxL . vldtTxBodyL
    timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
    inputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (txInfoOutV2 . TxOutFromOutput)
        [minBound ..]
        (foldr (:) [] outs)
    rdmrs' <- mapM (transRedeemerPtr proxy txBody) rdmrs
    pure
      PV2.TxInfo
        { PV2.txInfoInputs = inputs
        , PV2.txInfoOutputs = outputs
        , PV2.txInfoReferenceInputs = refInputs
        , PV2.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV2.txInfoDCert = toList $ fmap (Alonzo.transTxCert) (txBody ^. certsTxBodyL)
        , PV2.txInfoWdrl =
            PV2.fromList $ Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
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
      datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)
      rdmrs = Map.toList (unRedeemers $ witnesses ^. rdmrsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV2.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose
