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
  toPlutusWithContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  AlonzoContextError (..),
  toLegacyPlutusArgs,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem)
import Cardano.Ledger.Alonzo.Tx (Data)
import Cardano.Ledger.Alonzo.TxWits (unRedeemers)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts (PlutusScript (..))
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..), isSJust, kindObject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..), PlutusArgs (..))
import Cardano.Ledger.Plutus.TxInfo (
  TxOutSource (..),
  transAddr,
  transCoinToValue,
  transDataHash,
  transScriptHash,
  transTxIn,
  txOutSourceToText,
 )
import Cardano.Ledger.TxIn (TxIn (..), txInToText)
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Aeson (ToJSON (..), (.=), pattern String)
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
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  TxOutSource (EraCrypto era) ->
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
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  TxOutSource (EraCrypto era) ->
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
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (ContextError era) PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
transTxInInfoV2 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (ContextError era) PV2.TxInInfo
transTxInInfoV2 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV2.TxInInfo (transTxIn txIn) plutusTxOut)

transRedeemer :: Data era -> PV2.Redeemer
transRedeemer = PV2.Redeemer . PV2.dataToBuiltinData . getPlutusData

transRedeemerPtr ::
  forall proxy l era.
  ( EraPlutusTxInfo l era
  , AlonzoEraTxBody era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  TxBody era ->
  (PlutusPurpose AsIx era, (Data era, ExUnits)) ->
  Either (ContextError era) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPtr proxy txBody (ptr, (d, _)) =
  case redeemerPointerInverse txBody ptr of
    SNothing -> Left $ inject $ RedeemerPointerPointsToNothing ptr
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy sp
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
  Tx era ->
  Either (ContextError era) (PV2.Map (PlutusScriptPurpose l) PV2.Redeemer)
transTxRedeemers proxy tx =
  PV2.unsafeFromList
    <$> mapM
      (transRedeemerPtr proxy (tx ^. bodyTxL))
      (Map.toList (unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL))

instance Crypto c => EraPlutusContext (BabbageEra c) where
  type ContextError (BabbageEra c) = BabbageContextError (BabbageEra c)

  mkPlutusWithContext = \case
    BabbagePlutusV1 p -> toPlutusWithContext $ Left p
    BabbagePlutusV2 p -> toPlutusWithContext $ Left p

data BabbageContextError era
  = AlonzoContextError !(AlonzoContextError era)
  | ByronTxOutInContext !(TxOutSource (EraCrypto era))
  | RedeemerPointerPointsToNothing !(PlutusPurpose AsIx era)
  | InlineDatumsNotSupported !(TxOutSource (EraCrypto era))
  | ReferenceScriptsNotSupported !(TxOutSource (EraCrypto era))
  | ReferenceInputsNotSupported !(Set.Set (TxIn (EraCrypto era)))
  deriving (Generic)

deriving instance
  (Eq (AlonzoContextError era), Eq (PlutusPurpose AsIx era)) =>
  Eq (BabbageContextError era)

deriving instance
  (Show (AlonzoContextError era), Show (PlutusPurpose AsIx era)) =>
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

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose proxy = Alonzo.transPlutusPurpose proxy . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    let refInputs = txBody ^. referenceInputsTxBodyL
    unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)

    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiProtVer ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
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

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (BabbageEra c) where
  toPlutusTxCert _ = pure . Alonzo.transTxCert

  toPlutusScriptPurpose proxy = Alonzo.transPlutusPurpose proxy . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiProtVer ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- transTxRedeemers proxy ltiTx
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

toPlutusV2Args ::
  EraPlutusTxInfo 'PlutusV2 era =>
  proxy 'PlutusV2 ->
  PV2.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Maybe (Data era) ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV2)
toPlutusV2Args proxy txInfo scriptPurpose maybeSpendingData redeemerData =
  PlutusV2Args
    <$> toLegacyPlutusArgs proxy (PV2.ScriptContext txInfo) scriptPurpose maybeSpendingData redeemerData
