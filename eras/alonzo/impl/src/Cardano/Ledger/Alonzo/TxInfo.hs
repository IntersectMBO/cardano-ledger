{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.TxInfo
  ( TxOutSource (..),
    TranslationError (..),
    transProtocolVersion,
    validScript,
    transDataHash,
    transDataHash',
    transKeyHash,
    transScriptHash,
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
    transValue,
    transDCert,
    transWdrl,
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
    PlutusDebug (..),
    PlutusError (..),
    PlutusDebugInfo (..),
    debugPlutus,
    runPLCScript,
    explainPlutusFailure,
    validPlutusdata,
    languages,
  )
where

-- =============================================

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull')
import Cardano.Crypto.Hash.Class (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data (..), Datum (..), getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    ExUnits (..),
    decodeCostModel,
    getEvaluationContext,
    transProtocolVersion,
    validScript,
  )
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoTx,
    CostModel,
    ScriptPurpose (..),
    txdats',
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    AlonzoEraTxOut (..),
    certsTxBodyL,
    mintTxBodyL,
    vldtTxBodyL,
    wdrlsTxBodyL,
  )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..), TxIx, certIxToInt, txIxToInt)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core as Core hiding (TranslationError)
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    Ptr (..),
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), hashKey)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash, hashAnnotated)
import Cardano.Ledger.Serialization (Sized (sizedValue))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
    Wdrl (..),
    WitVKey (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.Arrow (left)
import Data.ByteString as BS (ByteString, length)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short as SBS (ShortByteString, fromShort)
import qualified Data.ByteString.UTF8 as BSU
import Data.Coders
  ( Decode (..),
    Encode (..),
    decode,
    decodeList,
    encode,
    (!>),
    (<!),
  )
import Data.Fixed (HasResolution (resolution))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Lens.Micro
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as PV1
import Plutus.V1.Ledger.Contexts ()
import qualified Plutus.V2.Ledger.Api as PV2
import Prettyprinter (Pretty (..))

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

-- | A transaction output can be translated because it is a newly created output,
-- or because it is the output which is connected to a transaction input being spent.
data TxOutSource crypto
  = TxOutFromInput !(TxIn crypto)
  | TxOutFromOutput !TxIx
  deriving (Eq, Show, Generic, NoThunks)

instance CC.Crypto crypto => ToCBOR (TxOutSource crypto) where
  toCBOR = \case
    TxOutFromInput txIn -> encode $ Sum TxOutFromInput 0 !> To txIn
    TxOutFromOutput txIx -> encode $ Sum TxOutFromOutput 1 !> To txIx

instance CC.Crypto crypto => FromCBOR (TxOutSource crypto) where
  fromCBOR = decode (Summands "TxOutSource" dec)
    where
      dec 0 = SumD TxOutFromInput <! From
      dec 1 = SumD TxOutFromOutput <! From
      dec n = Invalid n

data TranslationError crypto
  = ByronTxOutInContext !(TxOutSource crypto)
  | TranslationLogicMissingInput !(TxIn crypto)
  | RdmrPtrPointsToNothing !RdmrPtr
  | LanguageNotSupported !Language
  | InlineDatumsNotSupported !(TxOutSource crypto)
  | ReferenceScriptsNotSupported !(TxOutSource crypto)
  | ReferenceInputsNotSupported !(Set (TxIn crypto))
  | TimeTranslationPastHorizon !Text
  deriving (Eq, Show, Generic, NoThunks)

instance CC.Crypto crypto => ToCBOR (TranslationError crypto) where
  toCBOR = \case
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

instance CC.Crypto crypto => FromCBOR (TranslationError crypto) where
  fromCBOR = decode (Summands "TranslationError" dec)
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

transScriptHash :: ScriptHash c -> PV1.ValidatorHash
transScriptHash (ScriptHash h) = PV1.ValidatorHash (PV1.toBuiltin (hashToBytes h))

transSafeHash :: SafeHash c i -> PV1.BuiltinByteString
transSafeHash = PV1.toBuiltin . hashToBytes . extractHash

transHash :: Hash h a -> BS.ByteString
transHash = hashToBytes

txInfoId :: TxId crypto -> PV1.TxId
txInfoId (TxId safe) = PV1.TxId (transSafeHash safe)

transStakeCred :: Credential keyrole crypto -> PV1.Credential
transStakeCred (ScriptHashObj (ScriptHash kh)) =
  PV1.ScriptCredential (PV1.ValidatorHash (PV1.toBuiltin (hashToBytes kh)))
transStakeCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))

transStakeReference :: StakeReference crypto -> Maybe PV1.StakingCredential
transStakeReference (StakeRefBase cred) = Just (PV1.StakingHash (transStakeCred cred))
transStakeReference (StakeRefPtr (Ptr (SlotNo slot) txIx certIx)) =
  let !txIxInteger = toInteger (txIxToInt txIx)
      !certIxInteger = toInteger (certIxToInt certIx)
   in Just (PV1.StakingPtr (fromIntegral slot) txIxInteger certIxInteger)
transStakeReference StakeRefNull = Nothing

transCred :: Credential keyrole crypto -> PV1.Credential
transCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))
transCred (ScriptHashObj (ScriptHash kh)) =
  PV1.ScriptCredential (PV1.ValidatorHash (PV1.toBuiltin (hashToBytes kh)))

transAddr :: Addr crypto -> Maybe PV1.Address
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
  HasField "_protocolVersion" (PParams era) ProtVer =>
  PParams era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  SlotNo ->
  Either Text PV1.POSIXTime
slotToPOSIXTime pp ei sysS s = do
  PV1.POSIXTime . transTime . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    <$> epochInfoSlotToUTCTime ei sysS s
  where
    transTime =
      if HardForks.translateTimeForPlutusScripts pp
        then
          truncate
            -- Convert to milliseconds
            . (* 1000)
        else resolution

-- | translate a validity interval to POSIX time
transVITime ::
  HasField "_protocolVersion" (PParams era) ProtVer =>
  PParams era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  ValidityInterval ->
  Either Text PV1.POSIXTimeRange
transVITime _ _ _ (ValidityInterval SNothing SNothing) = pure PV1.always
transVITime pp ei sysS (ValidityInterval (SJust i) SNothing) = do
  t <- slotToPOSIXTime pp ei sysS i
  pure $ PV1.from t
transVITime pp ei sysS (ValidityInterval SNothing (SJust i)) = do
  t <- slotToPOSIXTime pp ei sysS i
  pure $ PV1.to t
transVITime pp ei sysS (ValidityInterval (SJust i) (SJust j)) = do
  t1 <- slotToPOSIXTime pp ei sysS i
  t2 <- slotToPOSIXTime pp ei sysS j
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
  (AlonzoEraTxOut era, Value era ~ MaryValue (Crypto era)) =>
  TxIn (Crypto era) ->
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
  (AlonzoEraTxOut era, Value era ~ MaryValue (Crypto era)) =>
  TxOut era ->
  Maybe PV1.TxOut
txInfoOut txOut = do
  let val = txOut ^. valueTxOutL
      dataHash = txOut ^. dataHashTxOutL
  addr <- transTxOutAddr txOut
  pure (PV1.TxOut addr (transValue val) (transDataHash dataHash))

-- ==================================
-- translate Values

transPolicyID :: PolicyID crypto -> PV1.CurrencySymbol
transPolicyID (PolicyID (ScriptHash x)) = PV1.CurrencySymbol (PV1.toBuiltin (hashToBytes x))

transAssetName :: AssetName -> PV1.TokenName
transAssetName (AssetName bs) = PV1.TokenName (PV1.toBuiltin (SBS.fromShort bs))

transMultiAsset :: MultiAsset c -> PV1.Value
transMultiAsset (MultiAsset m) = Map.foldlWithKey' accum1 mempty m
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          PV1.unionWith
            (+)
            ans2
            (PV1.singleton (transPolicyID sym) (transAssetName tok) quantity)

transValue :: MaryValue c -> PV1.Value
transValue (MaryValue n m) = justAda <> transMultiAsset m
  where
    justAda = PV1.singleton PV1.adaSymbol PV1.adaToken n

-- =============================================
-- translate fileds like DCert, Wdrl, and similar

transDCert :: DCert c -> PV1.DCert
transDCert (DCertDeleg (RegKey stkcred)) =
  PV1.DCertDelegRegKey (PV1.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (DeRegKey stkcred)) =
  PV1.DCertDelegDeRegKey (PV1.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (Delegate (Delegation stkcred keyhash))) =
  PV1.DCertDelegDelegate
    (PV1.StakingHash (transStakeCred stkcred))
    (transKeyHash keyhash)
transDCert (DCertPool (RegPool pp)) =
  PV1.DCertPoolRegister (transKeyHash (_poolId pp)) (PV1.PubKeyHash (PV1.toBuiltin (transHash (_poolVrf pp))))
transDCert (DCertPool (RetirePool keyhash (EpochNo i))) =
  PV1.DCertPoolRetire (transKeyHash keyhash) (fromIntegral i)
transDCert (DCertGenesis _) = PV1.DCertGenesis
transDCert (DCertMir _) = PV1.DCertMir

transWdrl :: Wdrl crypto -> Map.Map PV1.StakingCredential Integer
transWdrl (Wdrl mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) =
      Map.insert (PV1.StakingHash (transStakeCred cred)) n ans

getWitVKeyHash :: (CC.Crypto crypto, Typeable kr) => WitVKey kr crypto -> PV1.PubKeyHash
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
  ExUnits <$> safeFromInteger memory
    <*> safeFromInteger steps
  where
    safeFromInteger :: Integral a => a -> Maybe Natural
    safeFromInteger i
      | i >= 0 = Just $ fromIntegral i
      | otherwise = Nothing

-- ===================================
-- translate Script Purpose

transScriptPurpose :: ScriptPurpose crypto -> PV1.ScriptPurpose
transScriptPurpose (Minting policyid) = PV1.Minting (transPolicyID policyid)
transScriptPurpose (Spending txin) = PV1.Spending (txInfoIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) =
  PV1.Rewarding (PV1.StakingHash (transStakeCred cred))
transScriptPurpose (Certifying dcert) = PV1.Certifying (transDCert dcert)

data VersionedTxInfo
  = TxInfoPV1 PV1.TxInfo
  | TxInfoPV2 PV2.TxInfo
  deriving (Show, Eq)

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
    Either (TranslationError (Crypto era)) VersionedTxInfo

  -- Compute two sets for all TwoPhase scripts in a Tx.
  -- set 1) DataHashes for each Two phase Script in a TxIn that has a DataHash
  -- set 2) TxIns that are TwoPhase scripts, and should have a DataHash but don't.
  {- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a} -}
  inputDataHashes ::
    Map.Map (ScriptHash (Crypto era)) (Script era) ->
    AlonzoTx era ->
    UTxO era ->
    (Set (DataHash (Crypto era)), Set (TxIn (Crypto era)))

  txscripts ::
    UTxO era ->
    Tx era ->
    Map.Map (ScriptHash (Crypto era)) (Script era)

  getAllowedSupplimentalDataHashes ::
    TxBody era ->
    UTxO era ->
    Set (DataHash (Crypto era))

  getDatum ::
    Tx era ->
    UTxO era ->
    ScriptPurpose (Crypto era) ->
    Maybe (Data era)

  getTxOutDatum ::
    TxOut era -> Datum era

  allOuts ::
    TxBody era ->
    [TxOut era]
  allOuts = map sizedValue . allSizedOuts

  allSizedOuts ::
    TxBody era ->
    [Sized (TxOut era)]

alonzoTxInfo ::
  forall era.
  ( EraTx era,
    AlonzoEraTxBody era,
    Value era ~ MaryValue (Crypto era),
    Witnesses era ~ TxWitness era,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (TranslationError (Crypto era)) VersionedTxInfo
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
          { PV1.txInfoInputs = mapMaybe (uncurry txInfoIn) txIns,
            PV1.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] txOuts),
            PV1.txInfoFee = transValue (inject @(MaryValue (Crypto era)) fee),
            PV1.txInfoMint = transMultiAsset (txBody ^. mintTxBodyL),
            PV1.txInfoDCert = foldr (\c ans -> transDCert c : ans) [] (txBody ^. certsTxBodyL),
            PV1.txInfoWdrl = Map.toList (transWdrl (txBody ^. wdrlsTxBodyL)),
            PV1.txInfoValidRange = timeRange,
            PV1.txInfoSignatories = map transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL)),
            PV1.txInfoData = map transDataPair datpairs,
            PV1.txInfoId = PV1.TxId (transSafeHash (hashAnnotated @(Crypto era) txBody))
          }
    _ -> Left $ LanguageNotSupported lang
  where
    txBody :: TxBody era
    txBody = tx ^. bodyTxL
    txWits :: TxWitness era
    txWits = tx ^. witsTxL
    txOuts = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    interval = txBody ^. vldtTxBodyL

    datpairs = Map.toList (unTxDats $ txdats' txWits)

-- | valContext pairs transaction data with a script purpose.
--   See figure 22 of the Alonzo specification.
valContext ::
  VersionedTxInfo ->
  ScriptPurpose (Crypto era) ->
  Data era
valContext (TxInfoPV1 txinfo) sp = Data (PV1.toData (PV1.ScriptContext txinfo (transScriptPurpose sp)))
valContext (TxInfoPV2 txinfo) sp = Data (PV2.toData (PV2.ScriptContext txinfo (transScriptPurpose sp)))

data ScriptFailure = PlutusSF Text PlutusDebug
  deriving (Eq, Generic, NoThunks)

data ScriptResult
  = Passes [PlutusDebug]
  | Fails [PlutusDebug] (NonEmpty ScriptFailure)
  deriving (Generic)

scriptPass :: PlutusDebug -> ScriptResult
scriptPass pd = Passes [pd]

scriptFail :: ScriptFailure -> ScriptResult
scriptFail pd = Fails [] (pure pd)

instance NoThunks ScriptResult

instance Semigroup ScriptResult where
  (Passes ps) <> (Passes qs) = Passes (ps <> qs)
  (Passes ps) <> (Fails qs xs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Passes qs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Fails qs ys) = Fails (ps <> qs) (xs <> ys)

instance Monoid ScriptResult where
  mempty = Passes mempty

data PlutusDebug
  = PlutusDebugV1
      CostModel
      ExUnits
      SBS.ShortByteString
      [PV1.Data]
      ProtVer
  | PlutusDebugV2
      CostModel
      ExUnits
      SBS.ShortByteString
      [PV2.Data]
      ProtVer
  deriving (Eq, Generic, NoThunks)

-- There is no Show instance for PlutusDebug intentionally, because it is too
-- expensive and it will be too tempting to use it incorrectly. If needed for
-- testing use 'StandaloneDeriving', otherwise define an efficient way to display
-- this info.

data PlutusError = PlutusErrorV1 PV1.EvaluationError | PlutusErrorV2 PV2.EvaluationError
  deriving (Show)

data PlutusDebugInfo
  = DebugSuccess PV1.ExBudget
  | DebugCannotDecode String
  | DebugInfo [Text] PlutusError PlutusDebug
  | DebugBadHex String

instance ToCBOR PlutusDebug where
  toCBOR (PlutusDebugV1 a b c d e) = encode $ Sum PlutusDebugV1 0 !> To a !> To b !> To c !> To d !> To e
  toCBOR (PlutusDebugV2 a b c d e) = encode $ Sum PlutusDebugV2 1 !> To a !> To b !> To c !> To d !> To e

instance FromCBOR PlutusDebug where
  fromCBOR = decode (Summands "PlutusDebug" dec)
    where
      dec 0 =
        SumD PlutusDebugV1
          <! D (decodeCostModel PlutusV1)
          <! From
          <! From
          <! D (decodeList Cborg.decode)
          <! From
      dec 1 =
        SumD PlutusDebugV2
          <! D (decodeCostModel PlutusV2)
          <! From
          <! From
          <! D (decodeList Cborg.decode)
          <! From
      dec n = Invalid n

debugPlutus :: String -> PlutusDebugInfo
debugPlutus db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      case decodeFull' bs of
        Left e -> DebugCannotDecode (show e)
        Right pdb@(PlutusDebugV1 cm units script ds pv) ->
          case PV1.evaluateScriptRestricting
            (transProtocolVersion pv)
            PV1.Verbose
            (getEvaluationContext cm)
            (transExUnits units)
            script
            ds of
            (logs, Left e) -> DebugInfo logs (PlutusErrorV1 e) pdb
            (_, Right ex) -> DebugSuccess ex
        Right pdb@(PlutusDebugV2 cm units script ds pv) ->
          case PV2.evaluateScriptRestricting
            (transProtocolVersion pv)
            PV2.Verbose
            (getEvaluationContext cm)
            (transExUnits units)
            script
            ds of
            (logs, Left e) -> DebugInfo logs (PlutusErrorV2 e) pdb
            (_, Right ex) -> DebugSuccess ex

-- The runPLCScript in the Specification has a slightly different type
-- than the one in the implementation below. Made necessary by the the type
-- of PV1.evaluateScriptRestricting which is the interface to Plutus, and in the impementation
-- we try to track why a script failed (if it does) by the [String] in the Fails constructor of ScriptResut.

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
runPLCScript ::
  forall era.
  Show (AlonzoScript era) =>
  Proxy era ->
  ProtVer ->
  Language ->
  CostModel ->
  SBS.ShortByteString ->
  ExUnits ->
  [PV1.Data] ->
  ScriptResult
runPLCScript proxy pv lang cm scriptbytestring units ds =
  case plutusInterpreter
    lang
    PV1.Quiet
    (getEvaluationContext cm)
    (transExUnits units)
    scriptbytestring
    ds of
    (_, Left e) -> explainPlutusFailure proxy pv lang scriptbytestring e ds cm units
    (_, Right _) -> scriptPass $ successConstructor lang cm units scriptbytestring ds pv
  where
    plutusPV = transProtocolVersion pv
    plutusInterpreter PlutusV1 = PV1.evaluateScriptRestricting plutusPV
    plutusInterpreter PlutusV2 = PV2.evaluateScriptRestricting plutusPV
    successConstructor PlutusV1 = PlutusDebugV1
    successConstructor PlutusV2 = PlutusDebugV2

-- | Explain why a script might fail. Scripts come in two flavors:
--
-- (1) with 3  data arguments [data,redeemer,context]
--
-- (2) with 2 data arguments [redeemer,context].
--
-- It pays to decode the context data into a real context because that provides
-- way more information. But there is no guarantee the context data really can
-- be decoded.
explainPlutusFailure ::
  forall era.
  Show (AlonzoScript era) =>
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
  let ss :: AlonzoScript era
      ss = PlutusScript lang scriptbytestring
      name :: String
      name = show ss
      firstLine = "\nThe " ++ show lang ++ " script (" ++ name ++ ") fails."
      pvLine = "The protocol version is: " ++ show pv
      plutusError = "The plutus error is: " ++ show e
      dataLines =
        case ds of
          [dat, redeemer, info] ->
            case lang of
              PlutusV1 ->
                case PV1.fromData info of
                  Nothing ->
                    [ "The data is: " ++ show dat,
                      "The redeemer is: " ++ show redeemer,
                      "The third data argument, does not translate to a V1 script context\n" ++ show info
                    ]
                  Just info2 ->
                    [ "The data is: " ++ show dat,
                      "The redeemer is: " ++ show redeemer,
                      "The script context is:\n" ++ show (pretty (info2 :: PV1.ScriptContext))
                    ]
              PlutusV2 ->
                case PV2.fromData info of
                  Nothing ->
                    [ "The data is: " ++ show dat,
                      "The redeemer is: " ++ show redeemer,
                      "The third data argument, does not translate to a V2 script context\n" ++ show info
                    ]
                  Just info2 ->
                    [ "The data is: " ++ show dat,
                      "The redeemer is: " ++ show redeemer,
                      "The script context is:\n" ++ show (pretty (info2 :: PV2.ScriptContext))
                    ]
          [redeemer, info] ->
            case lang of
              PlutusV1 ->
                case PV1.fromData info of
                  Nothing ->
                    [ "The redeemer is: " ++ show redeemer,
                      "The second data argument, does not translate to a V1 script context\n" ++ show info
                    ]
                  Just info2 ->
                    [ "The redeemer is: " ++ show redeemer,
                      "The script context is:\n" ++ show (pretty (info2 :: PV1.ScriptContext))
                    ]
              PlutusV2 ->
                case PV2.fromData info of
                  Nothing ->
                    [ "The redeemer is: " ++ show redeemer,
                      "The second data argument, does not translate to a V2 script context\n" ++ show info
                    ]
                  Just info2 ->
                    [ "The redeemer is: " ++ show redeemer,
                      "The script context is:\n" ++ show (pretty (info2 :: PV2.ScriptContext))
                    ]
          _ ->
            [ "Received an unexpected number of Data",
              "The data was:\n" ++ show ds
            ]
      line = pack . unlines $ firstLine : plutusError : pvLine : dataLines

      db = case lang of
        PlutusV1 -> PlutusDebugV1 cm eu scriptbytestring ds pv
        PlutusV2 -> PlutusDebugV2 cm eu scriptbytestring ds pv
   in scriptFail $ PlutusSF line db

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
  ( ExtendedUTxO era,
    Script era ~ AlonzoScript era
  ) =>
  Tx era ->
  UTxO era ->
  Set (ScriptHash (Crypto era)) ->
  Set Language
languages tx utxo sNeeded = Map.foldl' accum Set.empty allscripts
  where
    allscripts = Map.restrictKeys (txscripts @era utxo tx) sNeeded
    accum ans (TimelockScript _) = ans
    accum ans (PlutusScript l _) = Set.insert l ans
