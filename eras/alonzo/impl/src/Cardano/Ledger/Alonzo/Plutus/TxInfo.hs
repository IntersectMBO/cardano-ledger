{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Plutus.TxInfo (
  TxOutSource (..),
  TranslationError (..),
  transProtocolVersion,
  validScript,
  transDataHash,
  transDataHash',
  transKeyHash,
  transSafeHash,
  transHash,
  txInfoId,
  transStakeReference,
  transCred,
  transAddr,
  transTxOutAddr,
  slotToPOSIXTime,
  transVITime,
  txInfoIn',
  txInfoIn,
  txInfoOut,
  transPolicyID,
  transAssetName,
  transMultiAsset,
  transMintValue,
  transValue,
  transWithdrawals,
  getWitVKeyHash,
  transDataPair,
  transExUnits,
  exBudgetToExUnits,
  transScriptPurpose,
  VersionedTxInfo (..),
  ExtendedUTxO (..),
  alonzoTxInfo,
  valContext,
  ScriptFailure (..),
  ScriptResult (..),
  scriptPass,
  scriptFail,
  PlutusDebugLang (..),
  PlutusDebug (..),
  PlutusData (..),
  PlutusError (..),
  PlutusDebugInfo (..),
  EraPlutusContext (..),
  PlutusWithContext (..),
  alonzoTransTxCert,
  PlutusTxCert (..),
  unTxCertV1,
  unTxCertV2,
  unTxCertV3,
  debugPlutus,
  runPlutusScript,
  runPlutusScriptWithLogs,
  deserialiseAndEvaluateScript,
  explainPlutusEvaluationError,
  languages,
)
where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), validScript)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..), txdats')
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  mintTxBodyL,
  vldtTxBodyL,
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, RdmrPtr, unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), inject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Core as Core hiding (TranslationError)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Evaluate
import Cardano.Ledger.Plutus.Language (Language (..), Plutus (..))
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (getScriptsProvided), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Data.ByteString.Short as SBS (fromShort)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import PlutusLedgerApi.V1.Contexts ()
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- | NOTE: class 'TranslateEra' defines an associated type with the same name. Not to be confused.
data TranslationError c
  = ByronTxOutInContext !(TxOutSource c)
  | TranslationLogicMissingInput !(TxIn c)
  | RdmrPtrPointsToNothing !RdmrPtr
  | LanguageNotSupported !Language
  | InlineDatumsNotSupported !(TxOutSource c)
  | ReferenceScriptsNotSupported !(TxOutSource c)
  | ReferenceInputsNotSupported !(Set (TxIn c))
  | TimeTranslationPastHorizon !Text
  deriving (Eq, Show, Generic, NoThunks)

instance Crypto c => EncCBOR (TranslationError c) where
  encCBOR = \case
    ByronTxOutInContext txOutSource ->
      encode $ Sum ByronTxOutInContext 0 !> To txOutSource
    TranslationLogicMissingInput txIn ->
      encode $ Sum TranslationLogicMissingInput 1 !> To txIn
    RdmrPtrPointsToNothing ptr ->
      encode $ Sum RdmrPtrPointsToNothing 2 !> To ptr
    LanguageNotSupported lang ->
      encode $ Sum LanguageNotSupported 3 !> To lang
    InlineDatumsNotSupported txOutSource ->
      encode $ Sum InlineDatumsNotSupported 4 !> To txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      encode $ Sum ReferenceScriptsNotSupported 5 !> To txOutSource
    ReferenceInputsNotSupported txIns ->
      encode $ Sum ReferenceInputsNotSupported 6 !> To txIns
    TimeTranslationPastHorizon err ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err

instance Crypto c => DecCBOR (TranslationError c) where
  decCBOR = decode (Summands "TranslationError" dec)
    where
      dec 0 = SumD ByronTxOutInContext <! From
      dec 1 = SumD TranslationLogicMissingInput <! From
      dec 2 = SumD RdmrPtrPointsToNothing <! From
      dec 3 = SumD LanguageNotSupported <! From
      dec 4 = SumD InlineDatumsNotSupported <! From
      dec 5 = SumD ReferenceScriptsNotSupported <! From
      dec 6 = SumD ReferenceInputsNotSupported <! From
      dec 7 = SumD TimeTranslationPastHorizon <! From
      dec n = Invalid n

-- | translate a validity interval to POSIX time
transVITime ::
  EraPParams era =>
  PParams era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  ValidityInterval ->
  Either Text PV1.POSIXTimeRange
transVITime _ _ _ (ValidityInterval SNothing SNothing) = pure PV1.always
transVITime _ ei sysS (ValidityInterval (SJust i) SNothing) = do
  t <- slotToPOSIXTime ei sysS i
  pure $ PV1.from t
transVITime pp ei sysS (ValidityInterval SNothing (SJust i)) = do
  t <- slotToPOSIXTime ei sysS i
  pure $
    if HardForks.translateUpperBoundForPlutusScripts (pp ^. ppProtocolVersionL)
      then
        PV1.Interval
          (PV1.LowerBound PV1.NegInf True)
          (PV1.strictUpperBound t)
      else PV1.to t
transVITime _ ei sysS (ValidityInterval (SJust i) (SJust j)) = do
  t1 <- slotToPOSIXTime ei sysS i
  t2 <- slotToPOSIXTime ei sysS j
  pure $
    PV1.Interval
      (PV1.lowerBound t1)
      (PV1.strictUpperBound t2)

-- ========================================
-- translate TxIn and TxOut

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it and return
--   (Just translation). If does not exist in the UTxO, return Nothing.
txInfoIn ::
  (AlonzoEraTxOut era, Value era ~ MaryValue (EraCrypto era)) =>
  TxIn (EraCrypto era) ->
  TxOut era ->
  Maybe PV1.TxInInfo
txInfoIn txIn txOut = do
  let val = transValue (txOut ^. valueTxOutL)
      dataHash = case txOut ^. dataHashTxOutL of
        SNothing -> Nothing
        SJust safeHash -> Just (PV1.DatumHash (transSafeHash safeHash))
  addr <- transTxOutAddr txOut
  pure $ PV1.TxInInfo (txInfoIn' txIn) (PV1.TxOut addr val dataHash)

-- | Given a TxOut, translate it and return (Just transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Nothing
--   I.e. don't include Bootstrap Addresses in the answer.
txInfoOut ::
  (AlonzoEraTxOut era, Value era ~ MaryValue (EraCrypto era)) =>
  TxOut era ->
  Maybe PV1.TxOut
txInfoOut txOut = do
  let val = txOut ^. valueTxOutL
      dataHash = txOut ^. dataHashTxOutL
  addr <- transTxOutAddr txOut
  pure (PV1.TxOut addr (transValue val) (transDataHash dataHash))

-- ==================================
-- translate Values

transPolicyID :: PolicyID c -> PV1.CurrencySymbol
transPolicyID (PolicyID (ScriptHash x)) = PV1.CurrencySymbol (PV1.toBuiltin (hashToBytes x))

transAssetName :: AssetName -> PV1.TokenName
transAssetName (AssetName bs) = PV1.TokenName (PV1.toBuiltin (SBS.fromShort bs))

transMultiAsset :: MultiAsset c -> PV1.Value
transMultiAsset ma = transMultiAssetInternal ma mempty

transMultiAssetInternal :: MultiAsset c -> PV1.Value -> PV1.Value
transMultiAssetInternal (MultiAsset m) initAcc = Map.foldlWithKey' accum1 initAcc m
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          PV1.unionWith
            (+)
            ans2
            (PV1.singleton (transPolicyID sym) (transAssetName tok) quantity)

-- | Hysterical raisins:
--
-- Previously transaction body contained a mint field with MaryValue instead of a
-- MultiAsset, which has changed since then to just MultiAsset (because minting ADA
-- makes no sense). However, if we don't preserve previous translation, scripts that
-- previously succeeded will fail.
transMintValue :: MultiAsset c -> PV1.Value
transMintValue m = transMultiAssetInternal m justZeroAda
  where
    justZeroAda = PV1.singleton PV1.adaSymbol PV1.adaToken 0

transValue :: MaryValue c -> PV1.Value
transValue (MaryValue (Coin n) m) = justAda <> transMultiAsset m
  where
    justAda = PV1.singleton PV1.adaSymbol PV1.adaToken n

-- =============================================
-- translate fields like TxCert, Withdrawals, and similar

instance Crypto c => EraPlutusContext 'PlutusV1 (AlonzoEra c) where
  transTxCert = TxCertPlutusV1 . alonzoTransTxCert

alonzoTransTxCert :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> PV1.DCert
alonzoTransTxCert = \case
  RegTxCert stakeCred ->
    PV1.DCertDelegRegKey (PV1.StakingHash (transCred stakeCred))
  UnRegTxCert stakeCred ->
    PV1.DCertDelegDeRegKey (PV1.StakingHash (transCred stakeCred))
  DelegStakeTxCert stakeCred keyHash ->
    PV1.DCertDelegDelegate (PV1.StakingHash (transCred stakeCred)) (transKeyHash keyHash)
  RegPoolTxCert (PoolParams {ppId, ppVrf}) ->
    PV1.DCertPoolRegister (transKeyHash ppId) (PV1.PubKeyHash (PV1.toBuiltin (transHash ppVrf)))
  RetirePoolTxCert poolId (EpochNo i) ->
    PV1.DCertPoolRetire (transKeyHash poolId) (fromIntegral i)
  GenesisDelegTxCert {} -> PV1.DCertGenesis
  MirTxCert {} -> PV1.DCertMir

-- ===================================
-- translate Script Purpose

transScriptPurpose ::
  EraPlutusContext 'PlutusV1 era =>
  ScriptPurpose era ->
  PV1.ScriptPurpose
transScriptPurpose (Minting policyid) = PV1.Minting (transPolicyID policyid)
transScriptPurpose (Spending txin) = PV1.Spending (txInfoIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) =
  PV1.Rewarding (PV1.StakingHash (transCred cred))
-- TODO Add support for PV3
transScriptPurpose (Certifying dcert) = PV1.Certifying . unTxCertV1 $ transTxCert dcert

-- | Where we keep functions that differ from Era to Era but which
--   deal with the extra things in the TxOut (Scripts, DataHash, Datum, etc)
class ExtendedUTxO era where
  -- Compute a Digest of the current transaction to pass to the script
  --    This is the major component of the valContext function.
  txInfo ::
    PParams era ->
    Language ->
    EpochInfo (Either Text) ->
    SystemStart ->
    UTxO era ->
    Tx era ->
    Either (TranslationError (EraCrypto era)) VersionedTxInfo

  txscripts ::
    UTxO era ->
    Tx era ->
    Map.Map (ScriptHash (EraCrypto era)) (Script era)
  default txscripts ::
    EraUTxO era =>
    UTxO era ->
    Tx era ->
    Map.Map (ScriptHash (EraCrypto era)) (Script era)
  txscripts utxo = unScriptsProvided . getScriptsProvided utxo

  getAllowedSupplimentalDataHashes ::
    TxBody era ->
    UTxO era ->
    Set (DataHash (EraCrypto era))
  default getAllowedSupplimentalDataHashes ::
    AlonzoEraUTxO era =>
    TxBody era ->
    UTxO era ->
    Set (DataHash (EraCrypto era))
  getAllowedSupplimentalDataHashes txBody utxo = getSupplementalDataHashes utxo txBody

  getDatum ::
    Tx era ->
    UTxO era ->
    ScriptPurpose era ->
    Maybe (Data era)
  default getDatum :: AlonzoEraUTxO era => Tx era -> UTxO era -> ScriptPurpose era -> Maybe (Data era)
  getDatum tx utxo = getSpendingDatum utxo tx

{-# DEPRECATED txscripts "In favor of `getScriptsProvided`" #-}
{-# DEPRECATED getAllowedSupplimentalDataHashes "In favor of `getSupplementalDataHashes`" #-}
{-# DEPRECATED getDatum "In favor of `getDatumForSpending`" #-}

alonzoTxInfo ::
  forall era.
  ( EraTx era
  , AlonzoEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , TxWits era ~ AlonzoTxWits era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
alonzoTxInfo pp lang ei sysS utxo tx = do
  timeRange <- left TimeTranslationPastHorizon $ transVITime pp ei sysS interval
  -- We need to do this as a separate step
  let lookupTxOut txIn =
        case Map.lookup txIn (unUTxO utxo) of
          Nothing -> Left $ TranslationLogicMissingInput txIn
          Just txOut -> Right (txIn, txOut)
  txIns <- mapM lookupTxOut (Set.toList (txBody ^. inputsTxBodyL))
  case lang of
    PlutusV1 ->
      Right . TxInfoPV1 $
        PV1.TxInfo
          { PV1.txInfoInputs = mapMaybe (uncurry txInfoIn) txIns
          , PV1.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] txOuts)
          , PV1.txInfoFee = transValue (inject @Coin @(MaryValue (EraCrypto era)) fee)
          , PV1.txInfoMint = transMintValue (txBody ^. mintTxBodyL)
          , PV1.txInfoDCert = toList $ fmap (unTxCertV1 . transTxCert) (txBody ^. certsTxBodyL)
          , PV1.txInfoWdrl = Map.toList (transWithdrawals (txBody ^. withdrawalsTxBodyL))
          , PV1.txInfoValidRange = timeRange
          , PV1.txInfoSignatories = map transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
          , PV1.txInfoData = map transDataPair datpairs
          , PV1.txInfoId = PV1.TxId (transSafeHash (hashAnnotated txBody))
          }
    _ -> Left $ LanguageNotSupported lang
  where
    txBody :: TxBody era
    txBody = tx ^. bodyTxL
    txWits :: AlonzoTxWits era
    txWits = tx ^. witsTxL
    txOuts = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    interval = txBody ^. vldtTxBodyL

    datpairs = Map.toList (unTxDats $ txdats' txWits)

-- | valContext pairs transaction data with a script purpose.
--   See figure 22 of the Alonzo specification.
valContext ::
  EraPlutusContext 'PlutusV1 era =>
  VersionedTxInfo ->
  ScriptPurpose era ->
  Data era
valContext (TxInfoPV1 txinfo) sp =
  Data (PV1.toData (PV1.ScriptContext txinfo (transScriptPurpose sp)))
valContext (TxInfoPV2 txinfo) sp =
  Data (PV2.toData (PV2.ScriptContext txinfo (transScriptPurpose sp)))
valContext (TxInfoPV3 txinfo) _sp =
  -- FIXME: add support for PlutusV3
  Data (PV3.toData (PV3.ScriptContext txinfo (error "Unimplemented")))

-- | Compute the Set of Languages in an era, where 'AlonzoScripts' are used
languages ::
  forall era.
  ( ExtendedUTxO era
  , Script era ~ AlonzoScript era
  ) =>
  Tx era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
  Set Language
languages tx utxo sNeeded = Map.foldl' accum Set.empty allscripts
  where
    allscripts = Map.restrictKeys (txscripts @era utxo tx) sNeeded
    accum ans (TimelockScript _) = ans
    accum ans (PlutusScript (Plutus l _)) = Set.insert l ans
