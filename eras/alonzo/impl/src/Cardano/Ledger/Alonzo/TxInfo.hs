{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

module Cardano.Ledger.Alonzo.TxInfo (
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
  transStakeCred,
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
  explainPlutusEvaluationError,
  languages,
  runPLCScript,
  -- DEPRECATED
  explainPlutusFailure,
  validPlutusdata,
  getTxOutDatum,
  transShelleyTxCert,
)
where

-- =============================================

import Cardano.Crypto.Hash.Class (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  ExUnits (..),
  decodeCostModelFailHard,
  encodeCostModel,
  getEvaluationContext,
  transProtocolVersion,
  validScript,
 )
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), Datum, getPlutusData)
import Cardano.Ledger.Alonzo.Tx (CostModel, ScriptPurpose (..), txdats')
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  mintTxBodyL,
  vldtTxBodyL,
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, RdmrPtr, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..), TxIx, certIxToInt, txIxToInt)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Version,
  decodeFull',
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Binary.Decoding (decodeRecordSum)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core as Core hiding (TranslationError)
import Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), hashKey)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Language (
  BinaryPlutus (..),
  IsLanguage (..),
  Language (..),
  Plutus (..),
  SLanguage (..),
  fromSLanguage,
  withSLanguage,
 )
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash, hashAnnotated)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.Monad (when)
import Control.Monad.Trans.Fail
import Data.ByteString as BS (ByteString, length)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short as SBS (ShortByteString, fromShort)
import qualified Data.ByteString.UTF8 as BSU
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F (asum)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.SatInt (SatInt, fromSatInt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import PlutusLedgerApi.V1.Contexts ()
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prettyprinter (Pretty (..))

data PlutusWithContext era = PlutusWithContext
  { pwcScript :: !Plutus
  , pwcDatums :: ![Data era]
  , pwcExUnits :: !ExUnits
  , pwcCostModel :: !CostModel
  }
  deriving (Eq)

deriving instance Era era => Show (PlutusWithContext era)

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

-- | A transaction output can be translated because it is a newly created output,
-- or because it is the output which is connected to a transaction input being spent.
data TxOutSource c
  = TxOutFromInput !(TxIn c)
  | TxOutFromOutput !TxIx
  deriving (Eq, Show, Generic, NoThunks)

instance Crypto c => EncCBOR (TxOutSource c) where
  encCBOR = \case
    TxOutFromInput txIn -> encode $ Sum TxOutFromInput 0 !> To txIn
    TxOutFromOutput txIx -> encode $ Sum TxOutFromOutput 1 !> To txIx

instance Crypto c => DecCBOR (TxOutSource c) where
  decCBOR = decode (Summands "TxOutSource" dec)
    where
      dec 0 = SumD TxOutFromInput <! From
      dec 1 = SumD TxOutFromOutput <! From
      dec n = Invalid n

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

transDataHash :: StrictMaybe (DataHash c) -> Maybe PV1.DatumHash
transDataHash (SJust safe) = Just (transDataHash' safe)
transDataHash SNothing = Nothing

transDataHash' :: DataHash c -> PV1.DatumHash
transDataHash' safe = PV1.DatumHash (transSafeHash safe)

transKeyHash :: KeyHash d c -> PV1.PubKeyHash
transKeyHash (KeyHash h) = PV1.PubKeyHash (PV1.toBuiltin (hashToBytes h))

transSafeHash :: SafeHash c i -> PV1.BuiltinByteString
transSafeHash = PV1.toBuiltin . hashToBytes . extractHash

transHash :: Hash h a -> BS.ByteString
transHash = hashToBytes

txInfoId :: TxId c -> PV1.TxId
txInfoId (TxId safe) = PV1.TxId (transSafeHash safe)

transStakeCred :: Credential kr c -> PV1.Credential
transStakeCred (ScriptHashObj (ScriptHash sh)) =
  PV1.ScriptCredential (PV1.ScriptHash (PV1.toBuiltin (hashToBytes sh)))
transStakeCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))
{-# DEPRECATED transStakeCred "Infavor of identical `transCred`" #-}

transStakeReference :: StakeReference c -> Maybe PV1.StakingCredential
transStakeReference (StakeRefBase cred) = Just (PV1.StakingHash (transCred cred))
transStakeReference (StakeRefPtr (Ptr (SlotNo slot) txIx certIx)) =
  let !txIxInteger = toInteger (txIxToInt txIx)
      !certIxInteger = toInteger (certIxToInt certIx)
   in Just (PV1.StakingPtr (fromIntegral slot) txIxInteger certIxInteger)
transStakeReference StakeRefNull = Nothing

transCred :: Credential kr c -> PV1.Credential
transCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))
transCred (ScriptHashObj (ScriptHash sh)) =
  PV1.ScriptCredential (PV1.ScriptHash (PV1.toBuiltin (hashToBytes sh)))

transAddr :: Addr c -> Maybe PV1.Address
transAddr (Addr _net object stake) = Just (PV1.Address (transCred object) (transStakeReference stake))
transAddr (AddrBootstrap _bootaddr) = Nothing

transTxOutAddr :: EraTxOut era => TxOut era -> Maybe PV1.Address
transTxOutAddr txOut = do
  -- filter out Byron addresses without uncompacting them
  case txOut ^. bootAddrTxOutF of
    Just _ -> Nothing
    -- The presence of a Byron address is caught above in the Just case
    Nothing -> transAddr (txOut ^. addrTxOutL)

slotToPOSIXTime ::
  EpochInfo (Either Text) ->
  SystemStart ->
  SlotNo ->
  Either Text PV1.POSIXTime
slotToPOSIXTime ei sysS s = do
  PV1.POSIXTime . (truncate . (* 1000)) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    <$> epochInfoSlotToUTCTime ei sysS s

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

txInfoIn' :: TxIn c -> PV1.TxOutRef
txInfoIn' (TxIn txid txIx) = PV1.TxOutRef (txInfoId txid) (toInteger (txIxToInt txIx))

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
transValue (MaryValue n m) = justAda <> transMultiAsset m
  where
    justAda = PV1.singleton PV1.adaSymbol PV1.adaToken n

-- =============================================
-- translate fields like TxCert, Withdrawals, and similar

data PlutusTxCert (l :: Language) where
  TxCertPlutusV1 :: PV1.DCert -> PlutusTxCert 'PlutusV1
  TxCertPlutusV2 :: PV2.DCert -> PlutusTxCert 'PlutusV2
  TxCertPlutusV3 :: PV3.TxCert -> PlutusTxCert 'PlutusV3

unTxCertV1 :: PlutusTxCert 'PlutusV1 -> PV1.DCert
unTxCertV1 (TxCertPlutusV1 x) = x

unTxCertV2 :: PlutusTxCert 'PlutusV2 -> PV2.DCert
unTxCertV2 (TxCertPlutusV2 x) = x

unTxCertV3 :: PlutusTxCert 'PlutusV3 -> PV3.TxCert
unTxCertV3 (TxCertPlutusV3 x) = x

class Era era => EraPlutusContext (l :: Language) era where
  transTxCert :: TxCert era -> PlutusTxCert l

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

transShelleyTxCert ::
  (ShelleyEraTxCert era, ProtVerAtMost era 8, TxCert era ~ ShelleyTxCert era) =>
  ShelleyTxCert era ->
  PV1.DCert
transShelleyTxCert = alonzoTransTxCert
{-# DEPRECATED transShelleyTxCert "In favor of `alonzoTransTxCert`" #-}

transWithdrawals :: Withdrawals c -> Map.Map PV1.StakingCredential Integer
transWithdrawals (Withdrawals mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) =
      Map.insert (PV1.StakingHash (transCred cred)) n ans

getWitVKeyHash :: (Crypto c, Typeable kr) => WitVKey kr c -> PV1.PubKeyHash
getWitVKeyHash =
  PV1.PubKeyHash
    . PV1.toBuiltin
    . hashToBytes
    . (\(KeyHash x) -> x)
    . hashKey
    . (\(WitVKey x _) -> x)

transDataPair :: (DataHash c, Data era) -> (PV1.DatumHash, PV1.Datum)
transDataPair (x, y) = (transDataHash' x, PV1.Datum (PV1.dataToBuiltinData (getPlutusData y)))

transExUnits :: ExUnits -> PV1.ExBudget
transExUnits (ExUnits mem steps) =
  PV1.ExBudget (PV1.ExCPU (fromIntegral steps)) (PV1.ExMemory (fromIntegral mem))

exBudgetToExUnits :: PV1.ExBudget -> Maybe ExUnits
exBudgetToExUnits (PV1.ExBudget (PV1.ExCPU steps) (PV1.ExMemory memory)) =
  ExUnits
    <$> safeFromSatInt memory
    <*> safeFromSatInt steps
  where
    safeFromSatInt :: SatInt -> Maybe Natural
    safeFromSatInt i
      | i >= 0 = Just . fromInteger $ fromSatInt i
      | otherwise = Nothing

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

data VersionedTxInfo
  = TxInfoPV1 PV1.TxInfo
  | TxInfoPV2 PV2.TxInfo
  | TxInfoPV3 PV3.TxInfo
  deriving (Show, Eq, Generic)

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

  getAllowedSupplimentalDataHashes ::
    TxBody era ->
    UTxO era ->
    Set (DataHash (EraCrypto era))

  getDatum ::
    Tx era ->
    UTxO era ->
    ScriptPurpose era ->
    Maybe (Data era)

getTxOutDatum :: AlonzoEraTxOut era => TxOut era -> Datum era
getTxOutDatum txOut = txOut ^. datumTxOutF
{-# DEPRECATED getTxOutDatum "In favor of `datumTxOutF`" #-}

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
          , PV1.txInfoFee = transValue (inject @(MaryValue (EraCrypto era)) fee)
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

data ScriptFailure = PlutusSF Text PlutusDebug
  deriving (Show, Generic)

data ScriptResult
  = Passes [PlutusDebug]
  | Fails [PlutusDebug] (NonEmpty ScriptFailure)
  deriving (Generic)

scriptPass :: PlutusDebug -> ScriptResult
scriptPass pd = Passes [pd]

scriptFail :: ScriptFailure -> ScriptResult
scriptFail pd = Fails [] (pure pd)

instance Semigroup ScriptResult where
  (Passes ps) <> (Passes qs) = Passes (ps <> qs)
  (Passes ps) <> (Fails qs xs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Passes qs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Fails qs ys) = Fails (ps <> qs) (xs <> ys)

instance Monoid ScriptResult where
  mempty = Passes mempty

newtype PlutusData = PlutusData {unPlutusData :: [PV1.Data]}
  deriving (Eq)

instance EncCBOR PlutusData where
  encCBOR (PlutusData d) = encCBOR d

instance DecCBOR PlutusData where
  decCBOR = PlutusData <$> decCBOR

data PlutusDebugLang (l :: Language) where
  PlutusDebugLang ::
    { pdSLanguage :: SLanguage l
    , pdCostModel :: CostModel
    , pdExUnits :: ExUnits
    , pdPlutusScript :: BinaryPlutus
    , pdPlutusData :: PlutusData
    , pdProtVer :: ProtVer
    } ->
    PlutusDebugLang l

-- | There is dummy Show instance for PlutusDebugLang intentionally, because it is too
-- expensive and it will be too tempting to use it incorrectly. If needed for
-- testing use 'StandaloneDeriving', otherwise define an efficient way to display
-- this info.
instance Show (PlutusDebugLang l) where
  show _ = "PlutusDebug Omitted"

deriving instance Eq (SLanguage l) => Eq (PlutusDebugLang l)

deriving instance Generic (PlutusDebugLang l)

instance (IsLanguage l, EncCBOR (SLanguage l)) => EncCBOR (PlutusDebugLang l) where
  encCBOR (PlutusDebugLang slang costModel exUnits sbs pData protVer) =
    encode $
      Sum (PlutusDebugLang slang) (fromIntegral (fromEnum (fromSLanguage slang)))
        !> E encodeCostModel costModel
        !> To exUnits
        !> To sbs
        !> To pData
        !> To protVer

instance IsLanguage l => DecCBOR (PlutusDebugLang l) where
  decCBOR = decodeRecordSum "PlutusDebugLang" $ \tag -> do
    let slang = isLanguage @l
        lang = fromSLanguage slang
    when (fromEnum lang /= fromIntegral tag) $ fail $ "Unexpected language: " <> show tag
    costModel <- decodeCostModelFailHard lang
    exUnits <- decCBOR
    sbs <- decCBOR
    pData <- decCBOR
    protVer <- decCBOR
    pure (6, PlutusDebugLang slang costModel exUnits sbs pData protVer)

data PlutusDebug where
  PlutusDebug :: IsLanguage l => PlutusDebugLang l -> PlutusDebug

deriving instance Show PlutusDebug

instance EncCBOR PlutusDebug where
  encCBOR (PlutusDebug pdbg) = encCBOR pdbg

data PlutusError
  = PlutusErrorV1 PV1.EvaluationError
  | PlutusErrorV2 PV2.EvaluationError
  | PlutusErrorV3 PV3.EvaluationError
  deriving (Show)

data PlutusDebugInfo
  = DebugSuccess PV1.ExBudget -- NOTE: PV1.ExBudget == PV2.ExBudget, hence this works
  | DebugCannotDecode String
  | DebugInfo [Text] PlutusError PlutusDebug
  | DebugBadHex String
  deriving (Show)

debugPlutus ::
  Version ->
  String ->
  PlutusDebugInfo
debugPlutus version db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      let plutusDebugLangDecoder ::
            forall l. IsLanguage l => Proxy l -> Fail String PlutusDebug
          plutusDebugLangDecoder _ =
            FailT $
              pure $
                either (Left . pure . show) (Right . PlutusDebug) $
                  decodeFull' @(PlutusDebugLang l) version bs
          plutusDebugDecoder =
            F.asum
              [ plutusDebugLangDecoder (Proxy @'PlutusV1)
              , plutusDebugLangDecoder (Proxy @'PlutusV2)
              , plutusDebugLangDecoder (Proxy @'PlutusV3)
              ]
       in case runFail plutusDebugDecoder of
            Left e -> DebugCannotDecode e
            Right pd@(PlutusDebug (PlutusDebugLang sl costModel exUnits binaryScript pData protVer)) ->
              let pv = transProtocolVersion protVer
                  v = PV1.Verbose
                  cm = getEvaluationContext costModel
                  eu = transExUnits exUnits
                  BinaryPlutus script = binaryScript
                  PlutusData d = pData
               in case sl of
                    SPlutusV1 ->
                      case PV1.evaluateScriptRestricting pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV1 e) pd
                        (_, Right ex) -> DebugSuccess ex
                    SPlutusV2 ->
                      case PV2.evaluateScriptRestricting pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV2 e) pd
                        (_, Right ex) -> DebugSuccess ex
                    SPlutusV3 ->
                      case PV3.evaluateScriptRestricting pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV3 e) pd
                        (_, Right ex) -> DebugSuccess ex

-- The runPLCScript in the Specification has a slightly different type
-- than the one in the implementation below. Made necessary by the the type
-- of PV1.evaluateScriptRestricting which is the interface to Plutus, and in the impementation
-- we try to track why a script failed (if it does) by the [String] in the Fails constructor of
-- ScriptResut.

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
runPLCScript ::
  forall era.
  Era era =>
  Proxy era ->
  ProtVer ->
  Language ->
  CostModel ->
  SBS.ShortByteString ->
  ExUnits ->
  [PV1.Data] ->
  ScriptResult
runPLCScript _ pv lang cm scriptBytes units ds =
  runPlutusScript pv $
    PlutusWithContext
      { pwcScript = Plutus lang (BinaryPlutus scriptBytes)
      , pwcDatums = map (Data @era) ds
      , pwcExUnits = units
      , pwcCostModel = cm
      }
{-# DEPRECATED runPLCScript "In favor of `runPlutusScript`" #-}

runPlutusScript ::
  forall era.
  ProtVer ->
  PlutusWithContext era ->
  ScriptResult
runPlutusScript pv pwc@PlutusWithContext {pwcScript, pwcDatums, pwcExUnits, pwcCostModel} =
  case interpretedScript of
    Left evalError -> explainPlutusEvaluationError pv pwc evalError
    Right _ ->
      withSLanguage lang $ \slang ->
        scriptPass $
          PlutusDebug $
            PlutusDebugLang
              slang
              pwcCostModel
              pwcExUnits
              scriptBytes
              (PlutusData (map getPlutusData pwcDatums))
              pv
  where
    Plutus lang scriptBytes = pwcScript
    interpretedScript =
      snd $
        plutusInterpreter
          lang
          PV1.Quiet
          (getEvaluationContext pwcCostModel)
          (transExUnits pwcExUnits)
          (unBinaryPlutus scriptBytes)
          (map getPlutusData pwcDatums)
    plutusPV = transProtocolVersion pv
    plutusInterpreter PlutusV1 = PV1.evaluateScriptRestricting plutusPV
    plutusInterpreter PlutusV2 = PV2.evaluateScriptRestricting plutusPV
    plutusInterpreter PlutusV3 = PV3.evaluateScriptRestricting plutusPV -- TODO: Make class to unify all plutus versioned operations

explainPlutusFailure ::
  forall era.
  Era era =>
  Proxy era ->
  ProtVer ->
  Language ->
  SBS.ShortByteString ->
  PV1.EvaluationError ->
  [PV1.Data] ->
  CostModel ->
  ExUnits ->
  ScriptResult
explainPlutusFailure _proxy pv lang scriptbytestring e ds cm eu =
  explainPlutusEvaluationError pv pwc e
  where
    pwc =
      PlutusWithContext
        { pwcScript = Plutus lang (BinaryPlutus scriptbytestring)
        , pwcDatums = map (Data @era) ds
        , pwcExUnits = eu
        , pwcCostModel = cm
        }
{-# DEPRECATED explainPlutusFailure "In favor of `explainPlutusEvaluationError`" #-}

-- | Explain why a script might fail. Scripts come in two flavors:
--
-- (1) with 3 data arguments [data,redeemer,context]
--
-- (2) with 2 data arguments [redeemer,context].
--
-- It pays to decode the context data into a real context because that provides
-- way more information. But there is no guarantee the context data really can
-- be decoded.
explainPlutusEvaluationError ::
  forall era.
  ProtVer ->
  PlutusWithContext era ->
  PV1.EvaluationError ->
  ScriptResult
explainPlutusEvaluationError pv PlutusWithContext {pwcScript, pwcDatums, pwcExUnits, pwcCostModel} e =
  let Plutus lang binaryScript = pwcScript
      firstLine = "The " ++ show lang ++ " script failed:"
      pvLine = "The protocol version is: " ++ show pv
      plutusError = "The plutus evaluation error is: " ++ show e

      getCtxAsString :: Language -> PV1.Data -> Maybe String
      getCtxAsString PlutusV1 d = show . pretty <$> (PV1.fromData d :: Maybe PV1.ScriptContext)
      getCtxAsString PlutusV2 d = show . pretty <$> (PV2.fromData d :: Maybe PV2.ScriptContext)
      getCtxAsString PlutusV3 d = show . pretty <$> (PV3.fromData d :: Maybe PV3.ScriptContext)

      ctxMessage info =
        case getCtxAsString lang info of
          Nothing ->
            concat
              [ "The third data argument does not translate to a "
              , show lang
              , " script context\n"
              , show info
              ]
          Just ctx -> "The script context is:\n" ++ ctx
      datums = map getPlutusData pwcDatums
      dataLines =
        case datums of
          [dat, redeemer, info] ->
            [ "The datum is: " ++ show dat
            , "The redeemer is: " ++ show redeemer
            , ctxMessage info
            ]
          [redeemer, info] ->
            [ "The redeemer is: " ++ show redeemer
            , ctxMessage info
            ]
          ds ->
            [ "Received an unexpected number of Data"
            , "The data is:\n" ++ show ds
            ]
      line = pack . unlines $ "" : firstLine : show binaryScript : plutusError : pvLine : dataLines

      plutusDebug =
        withSLanguage lang $ \slang ->
          PlutusDebug $
            PlutusDebugLang
              { pdSLanguage = slang
              , pdCostModel = pwcCostModel
              , pdExUnits = pwcExUnits
              , pdPlutusScript = binaryScript
              , pdPlutusData = PlutusData datums
              , pdProtVer = pv
              }
   in scriptFail $ PlutusSF line plutusDebug

{-# DEPRECATED validPlutusdata "Plutus data bytestrings are not restricted to sixty-four bytes." #-}
validPlutusdata :: PV1.Data -> Bool
validPlutusdata (PV1.Constr _n ds) = all validPlutusdata ds
validPlutusdata (PV1.Map ds) =
  all (\(x, y) -> validPlutusdata x && validPlutusdata y) ds
validPlutusdata (PV1.List ds) = all validPlutusdata ds
validPlutusdata (PV1.I _n) = True
validPlutusdata (PV1.B bs) = BS.length bs <= 64

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
