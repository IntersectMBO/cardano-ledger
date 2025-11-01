{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
  transTxRedeemers,
  transRedeemer,
  toPlutusV2Args,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusScriptPurpose,
  PlutusTxInfo,
  SupportedLanguage (..),
  lookupTxInfoResultImpossible,
  toPlutusWithContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  AlonzoContextError (..),
  toLegacyPlutusArgs,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem)
import Cardano.Ledger.Alonzo.Tx (Data)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.BaseTypes (
  Inject (..),
  ProtVer,
  StrictMaybe (..),
  isSJust,
  kindObject,
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
import Cardano.Ledger.Plutus.Language (Language (..), PlutusArgs (..), SLanguage (..))
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
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
#if __GLASGOW_HASKELL__ < 914
import Data.Aeson (ToJSON (..), (.=), pattern String)
#else
import Data.Aeson (ToJSON (..), (.=), data String)
#endif
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
transTxInInfoV2 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV2.TxInInfo
transTxInInfoV2 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV2.TxInInfo (transTxIn txIn) plutusTxOut)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  forall proxy l era t.
  ( EraPlutusTxInfo l era
  , AlonzoEraTxBody era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  ProtVer ->
  TxBody t era ->
  (PlutusPurpose AsIx era, (Data era, ExUnits)) ->
  Either (ContextError era) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPtr proxy pv txBody (ptr, (d, _)) =
  case redeemerPointerInverse txBody ptr of
    SNothing -> Left $ inject $ RedeemerPointerPointsToNothing ptr
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy pv sp
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
  ProtVer ->
  Tx t era ->
  Either (ContextError era) (PV2.Map (PlutusScriptPurpose l) PV2.Redeemer)
transTxRedeemers proxy pv tx =
  PV2.unsafeFromList
    <$> mapM
      (transRedeemerPtr proxy pv $ tx ^. bodyTxL)
      (Map.toList $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)

instance EraPlutusContext BabbageEra where
  type ContextError BabbageEra = BabbageContextError BabbageEra
  data TxInfoResult BabbageEra
    = BabbageTxInfoResult -- Fields must be kept lazy
        (Either (ContextError BabbageEra) (PlutusTxInfo 'PlutusV1))
        (Either (ContextError BabbageEra) (PlutusTxInfo 'PlutusV2))

  mkSupportedLanguage = \case
    PlutusV1 -> Just $ SupportedLanguage SPlutusV1
    PlutusV2 -> Just $ SupportedLanguage SPlutusV2
    _lang -> Nothing

  mkTxInfoResult lti = BabbageTxInfoResult (toPlutusTxInfo SPlutusV1 lti) (toPlutusTxInfo SPlutusV2 lti)

  lookupTxInfoResult SPlutusV1 (BabbageTxInfoResult tirPlutusV1 _) = tirPlutusV1
  lookupTxInfoResult SPlutusV2 (BabbageTxInfoResult _ tirPlutusV2) = tirPlutusV2
  lookupTxInfoResult slang _ = lookupTxInfoResultImpossible slang

  mkPlutusWithContext = \case
    BabbagePlutusV1 p -> toPlutusWithContext $ Left p
    BabbagePlutusV2 p -> toPlutusWithContext $ Left p

data BabbageContextError era
  = AlonzoContextError !(AlonzoContextError era)
  | ByronTxOutInContext !TxOutSource
  | RedeemerPointerPointsToNothing !(PlutusPurpose AsIx era)
  | InlineDatumsNotSupported !TxOutSource
  | ReferenceScriptsNotSupported !TxOutSource
  | ReferenceInputsNotSupported !(Set.Set TxIn)
  deriving (Generic)

deriving instance
  (
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  Eq (AlonzoContextError era), 
#endif
  Eq (PlutusPurpose AsIx era)) =>
  Eq (BabbageContextError era)

deriving instance
  (
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  Show (AlonzoContextError era),
#endif
  Show (PlutusPurpose AsIx era)) =>
  Show (BabbageContextError era)

instance NoThunks (PlutusPurpose AsIx era) => NoThunks (BabbageContextError era)

instance (Era era, NFData (PlutusPurpose AsIx era)) => NFData (BabbageContextError era)

instance Inject (AlonzoContextError era) (BabbageContextError era) where
  inject = AlonzoContextError

instance (Era era, EncCBOR (PlutusPurpose AsIx era)) => EncCBOR (BabbageContextError era) where
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

instance (Era era, DecCBOR (PlutusPurpose AsIx era)) => DecCBOR (BabbageContextError era) where
  decCBOR = decode $ Summands "ContextError" $ \case
    0 -> SumD ByronTxOutInContext <! From
    1 -> SumD (AlonzoContextError . TranslationLogicMissingInput) <! From
    2 -> SumD RedeemerPointerPointsToNothing <! From
    4 -> SumD InlineDatumsNotSupported <! From
    5 -> SumD ReferenceScriptsNotSupported <! From
    6 -> SumD ReferenceInputsNotSupported <! From
    7 -> SumD (AlonzoContextError . TimeTranslationPastHorizon) <! From
    n -> Invalid n

instance ToJSON (PlutusPurpose AsIx era) => ToJSON (BabbageContextError era) where
  toJSON = \case
    AlonzoContextError err -> toJSON err
    ByronTxOutInContext txOutSource ->
      String $ "Byron UTxO being created or spent: " <> txOutSourceToText txOutSource
    RedeemerPointerPointsToNothing ptr ->
      kindObject "RedeemerPointerPointsToNothing" ["ptr" .= toJSON ptr]
    InlineDatumsNotSupported txOutSource ->
      String $ "Inline datums not supported, output source: " <> txOutSourceToText txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      String $ "Reference scripts not supported, output source: " <> txOutSourceToText txOutSource
    ReferenceInputsNotSupported txIns ->
      String $
        "Reference inputs not supported: "
          <> T.intercalate ", " (map txInToText (Set.toList txIns))

instance EraPlutusTxInfo 'PlutusV1 BabbageEra where
  toPlutusTxCert _ _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose proxy pv = Alonzo.transPlutusPurpose proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    let refInputs = txBody ^. referenceInputsTxBodyL
    unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)

    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV1 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txCerts
        , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV1.txInfoData = Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV1.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = Alonzo.toPlutusV1Args

  toPlutusTxInInfo _ = transTxInInfoV1

instance EraPlutusTxInfo 'PlutusV2 BabbageEra where
  toPlutusTxCert _ _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose proxy pv = Alonzo.transPlutusPurpose proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    plutusRedeemers <- transTxRedeemers proxy ltiProtVer ltiTx
    pure
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
        , PV2.txInfoData = PV2.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV2.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = toPlutusV2Args

  toPlutusTxInInfo _ = transTxInInfoV2

toPlutusV2Args ::
  EraPlutusTxInfo 'PlutusV2 era =>
  proxy 'PlutusV2 ->
  ProtVer ->
  PV2.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Maybe (Data era) ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV2)
toPlutusV2Args proxy pv txInfo scriptPurpose maybeSpendingData redeemerData =
  PlutusV2Args
    <$> toLegacyPlutusArgs proxy pv (PV2.ScriptContext txInfo) scriptPurpose maybeSpendingData redeemerData
