{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.TxInfo where

-- =============================================

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull', serialize')
import Cardano.Crypto.Hash.Class (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data (..), getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
-- Instances only

import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..), Script (..), decodeCostModel)
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( certs',
    inputs',
    mint',
    outputs',
    reqSignerHashes',
    txfee',
    vldt',
    wdrls',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness (..), unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core as Core (PParams, TxBody, TxOut, Value)
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), Ptr (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (KeyHash (..), hashKey)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.SafeHash
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
  ( translateTimeForPlutusScripts,
  )
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
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
import Control.DeepSeq (deepseq)
import Data.ByteString as BS (ByteString, length)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short as SBS (ShortByteString)
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
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as PV1
import Plutus.V1.Ledger.Contexts ()
import qualified Plutus.V2.Ledger.Api as PV2

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

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
transStakeReference (StakeRefPtr (Ptr (SlotNo slot) i1 i2)) =
  Just (PV1.StakingPtr (fromIntegral slot) (fromIntegral i1) (fromIntegral i2))
transStakeReference StakeRefNull = Nothing

transCred :: Credential keyrole crypto -> PV1.Credential
transCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))
transCred (ScriptHashObj (ScriptHash kh)) =
  PV1.ScriptCredential (PV1.ValidatorHash (PV1.toBuiltin (hashToBytes kh)))

transAddr :: Addr crypto -> Maybe PV1.Address
transAddr (Addr _net object stake) = Just (PV1.Address (transCred object) (transStakeReference stake))
transAddr (AddrBootstrap _bootaddr) = Nothing

slotToPOSIXTime ::
  (Monad m, HasField "_protocolVersion" (PParams era) ProtVer) =>
  Core.PParams era ->
  EpochInfo m ->
  SystemStart ->
  SlotNo ->
  m PV1.POSIXTime
slotToPOSIXTime pp ei sysS s = do
  PV1.POSIXTime . transTime . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    <$> epochInfoSlotToUTCTime ei sysS s
  where
    transTime =
      if HardForks.translateTimeForPlutusScripts pp
        then
          truncate
            -- Convert to milliseconds
            . (* fromInteger 1000)
        else resolution

-- | translate a validity interval to POSIX time
transVITime ::
  (Monad m, HasField "_protocolVersion" (PParams era) ProtVer) =>
  Core.PParams era ->
  EpochInfo m ->
  SystemStart ->
  ValidityInterval ->
  m PV1.POSIXTimeRange
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

txInfoIn' :: CC.Crypto c => TxIn c -> PV1.TxOutRef
txInfoIn' (TxIn txid nat) = PV1.TxOutRef (txInfoId txid) (fromIntegral nat)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it and return
--   (Just translation). If does not exist in the UTxO, return Nothing.
txInfoIn ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era),
    Core.TxOut era ~ Alonzo.TxOut era
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Maybe PV1.TxInInfo
txInfoIn (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Nothing
    Just txout -> case transAddr addr of
      Just ad -> Just (PV1.TxInInfo (txInfoIn' txin) (PV1.TxOut ad valout dhash))
      Nothing -> Nothing
      where
        valout = transValue (getField @"value" txout)
        addr = getField @"address" txout
        dhash = case getField @"datahash" txout of
          SNothing -> Nothing
          SJust safehash -> Just (PV1.DatumHash (transSafeHash safehash))

-- | Given a TxOut, translate it and return (Just transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Nothing
--   I.e. don't include Bootstrap Addresses in the answer.
txInfoOut ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  Alonzo.TxOut era ->
  Maybe PV1.TxOut
txInfoOut (Alonzo.TxOut addr val datahash) =
  case transAddr addr of
    Just ad -> Just (PV1.TxOut ad (transValue @(Crypto era) val) (transDataHash datahash))
    Nothing -> Nothing

-- ==================================
-- translate Values

transPolicyID :: Mary.PolicyID crypto -> PV1.CurrencySymbol
transPolicyID (Mary.PolicyID (ScriptHash x)) = PV1.CurrencySymbol (PV1.toBuiltin (hashToBytes x))

transAssetName :: Mary.AssetName -> PV1.TokenName
transAssetName (Mary.AssetName bs) = PV1.TokenName (PV1.toBuiltin bs)

transValue :: Mary.Value c -> PV1.Value
transValue (Mary.Value n mp) = Map.foldlWithKey' accum1 justada mp
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          PV1.unionWith
            (+)
            ans2
            (PV1.singleton (transPolicyID sym) (transAssetName tok) quantity)
    justada = PV1.singleton PV1.adaSymbol PV1.adaToken n

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
  Maybe (PV2.ScriptPurpose, PV2.Redeemer)
transRedeemerPtr txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Nothing
    SJust sp -> Just (transScriptPurpose sp, transRedeemer d)

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

transScriptPurpose :: CC.Crypto crypto => ScriptPurpose crypto -> PV1.ScriptPurpose
transScriptPurpose (Minting policyid) = PV1.Minting (transPolicyID policyid)
transScriptPurpose (Spending txin) = PV1.Spending (txInfoIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) =
  PV1.Rewarding (PV1.StakingHash (transStakeCred cred))
transScriptPurpose (Certifying dcert) = PV1.Certifying (transDCert dcert)

data VersionedTxInfo
  = TxInfoPV1 PV1.TxInfo
  | TxInfoPV2 PV2.TxInfo
  deriving (Show, Eq)

-- ===================================

-- | Compute a Digest of the current transaction to pass to the script
--   This is the major component of the valContext function.
txInfo ::
  forall era tx m.
  ( Era era,
    Monad m,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era),
    HasField "body" tx (Core.TxBody era),
    HasField "wits" tx (TxWitness era),
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  Core.PParams era ->
  Language ->
  EpochInfo m ->
  SystemStart ->
  UTxO era ->
  tx ->
  m VersionedTxInfo
txInfo pp lang ei sysS utxo tx = do
  timeRange <- transVITime pp ei sysS interval
  pure $
    case lang of
      PlutusV1 ->
        TxInfoPV1 $
          PV1.TxInfo
            { PV1.txInfoInputs = mapMaybe (txInfoIn utxo) (Set.toList (inputs' tbody)),
              PV1.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] outs),
              PV1.txInfoFee = transValue (inject @(Mary.Value (Crypto era)) fee),
              PV1.txInfoMint = transValue forge,
              PV1.txInfoDCert = foldr (\c ans -> transDCert c : ans) [] (certs' tbody),
              PV1.txInfoWdrl = Map.toList (transWdrl (wdrls' tbody)),
              PV1.txInfoValidRange = timeRange,
              PV1.txInfoSignatories = map transKeyHash (Set.toList (reqSignerHashes' tbody)),
              PV1.txInfoData = map transDataPair datpairs,
              PV1.txInfoId = PV1.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody))
            }
      PlutusV2 ->
        TxInfoPV2 $
          PV2.TxInfo
            { PV2.txInfoInputs = mapMaybe (txInfoIn utxo) (Set.toList (inputs' tbody)),
              PV2.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] outs),
              PV2.txInfoFee = transValue (inject @(Mary.Value (Crypto era)) fee),
              PV2.txInfoMint = transValue forge,
              PV2.txInfoDCert = foldr (\c ans -> transDCert c : ans) [] (certs' tbody),
              PV2.txInfoWdrl = PV2.fromList $ Map.toList (transWdrl (wdrls' tbody)),
              PV2.txInfoValidRange = timeRange,
              PV2.txInfoSignatories = map transKeyHash (Set.toList (reqSignerHashes' tbody)),
              PV2.txInfoRedeemers = PV2.fromList $ mapMaybe (transRedeemerPtr tbody) rdmrs,
              PV2.txInfoData = PV2.fromList $ map transDataPair datpairs,
              PV2.txInfoId = PV2.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody))
            }
  where
    tbody = getField @"body" tx
    _witnesses = getField @"wits" tx
    outs = outputs' tbody
    fee = txfee' tbody
    forge = mint' tbody
    interval = vldt' tbody
    datpairs = Map.toList (unTxDats $ txdats' _witnesses)
    rdmrs = Map.toList (unRedeemers $ txrdmrs' _witnesses)

-- ===============================================================
-- From the specification, Figure 7 "Script Validation, cont."
-- ===============================================================

-- | valContext collects info from the Tx and the UTxO an
--   translates it into a 'Data', which the Plutus language knows how to interpret.
--   The UTxO and the PtrMap are used to 'resolve' the TxIn and the StakeRefPtr's
valContext ::
  Era era =>
  VersionedTxInfo ->
  ScriptPurpose (Crypto era) ->
  Data era
valContext (TxInfoPV1 txinfo) sp = Data (PV1.toData (PV1.ScriptContext txinfo (transScriptPurpose sp)))
valContext (TxInfoPV2 txinfo) sp = Data (PV2.toData (PV2.ScriptContext txinfo (transScriptPurpose sp)))

data FailureDescription
  = OnePhaseFailure Text
  | PlutusFailure Text ByteString
  deriving (Show, Eq, Ord, Generic, NoThunks)

instance ToCBOR FailureDescription where
  toCBOR (OnePhaseFailure s) = encode $ Sum OnePhaseFailure 0 !> To s
  toCBOR (PlutusFailure s b) = encode $ Sum PlutusFailure 1 !> To s !> To b

instance FromCBOR FailureDescription where
  fromCBOR = decode (Summands "FailureDescription" dec)
    where
      dec 0 = SumD OnePhaseFailure <! From
      dec 1 = SumD PlutusFailure <! From <! From
      dec n = Invalid n

data ScriptResult = Passes | Fails ![FailureDescription]
  deriving (Show, Generic, NoThunks)

instance ToCBOR ScriptResult where
  toCBOR Passes = encode $ Sum Passes 0
  toCBOR (Fails fs) = encode $ Sum Fails 1 !> To fs

instance FromCBOR ScriptResult where
  fromCBOR = decode (Summands "ScriptResult" dec)
    where
      dec 0 = SumD Passes
      dec 1 = SumD Fails <! From
      dec n = Invalid n

andResult :: ScriptResult -> ScriptResult -> ScriptResult
andResult Passes Passes = Passes
andResult Passes ans = ans
andResult ans Passes = ans
andResult (Fails xs) (Fails ys) = Fails (xs ++ ys)

instance Semigroup ScriptResult where
  (<>) = andResult

data PlutusDebug = PlutusDebug
  { debugCostModel :: CostModel,
    debugExUnits :: ExUnits,
    debugScript :: SBS.ShortByteString,
    debugData :: [PV1.Data],
    debugVersion :: Language
  }
  deriving (Show)

data PlutusDebugInfo
  = DebugSuccess PV1.ExBudget
  | DebugCannotDecode String
  | DebugInfo [Text] String
  | DebugBadHex String
  deriving (Show)

instance ToCBOR PlutusDebug where
  toCBOR (PlutusDebug a b c d e) = encode (Rec PlutusDebug !> To a !> To b !> To c !> To d !> To e)

instance FromCBOR PlutusDebug where
  fromCBOR =
    decode $
      RecD PlutusDebug
        <! D (decodeCostModel PlutusV1)
        <! From
        <! From
        <! D (decodeList Cborg.decode)
        <! From

debugPlutus :: Language -> String -> PlutusDebugInfo
debugPlutus lang db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      case decodeFull' bs of
        Left e -> DebugCannotDecode (show e)
        Right (PlutusDebug (CostModel cost) units script ds _version) ->
          case interpreter
            verbose
            cost
            (transExUnits units)
            script
            ds of
            (logs, Left e) -> DebugInfo logs (show e)
            (_, Right ex) -> DebugSuccess ex
  where
    (interpreter, verbose) = case lang of
      PlutusV1 -> (PV1.evaluateScriptRestricting, PV1.Verbose)
      PlutusV2 -> (PV2.evaluateScriptRestricting, PV2.Verbose)

-- The runPLCScript in the Specification has a slightly different type
-- than the one in the implementation below. Made necessary by the the type
-- of PV1.evaluateScriptRestricting which is the interface to Plutus, and in the impementation
-- we try to track why a script failed (if it does) by the [String] in the Fails constructor of ScriptResut.

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
runPLCScript ::
  forall era.
  Show (Script era) =>
  Proxy era ->
  Language ->
  CostModel ->
  SBS.ShortByteString ->
  ExUnits ->
  [PV1.Data] ->
  ScriptResult
runPLCScript proxy lang (CostModel cost) scriptbytestring units ds =
  case plutusInterpreter
    lang
    PV1.Quiet
    cost
    (transExUnits units)
    scriptbytestring
    ds of
    (_, Left e) -> explainPlutusFailure proxy lang scriptbytestring e ds (CostModel cost) units
    (_, Right _) -> Passes
  where
    plutusInterpreter PlutusV1 = PV1.evaluateScriptRestricting
    plutusInterpreter PlutusV2 = PV2.evaluateScriptRestricting

-- | Explain why a script might fail. Scripts come in two flavors:
--
-- (1) with 3  data arguments [data,redeemer,context]
--
-- (2) with 2 data arguments [redeemer,context].
--
-- It pays to decode the context data into a real context because that provides
-- way more information. But there is no guarantee the context data really can
-- be decoded.
explainPlutusFailure,
  explain_plutus_failure ::
    forall era.
    Show (Script era) =>
    Proxy era ->
    Language ->
    SBS.ShortByteString ->
    PV1.EvaluationError ->
    [PV1.Data] ->
    CostModel ->
    ExUnits ->
    ScriptResult
explainPlutusFailure _proxy lang scriptbytestring e ds@[dat, redeemer, info] cm eu =
  -- A three data argument script.
  let ss :: Script era
      ss = PlutusScript lang scriptbytestring
      name :: String
      name = show ss
   in case PV1.fromData info of
        Nothing -> Fails [PlutusFailure line db]
          where
            line =
              pack $
                unlines
                  [ "\nThe 3 arg plutus script (" ++ name ++ ") fails.",
                    show e,
                    "The data is: " ++ show dat,
                    "The redeemer is: " ++ show redeemer,
                    "The third data argument, does not decode to a context\n" ++ show info
                  ]
            db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
        Just info2 -> Fails [PlutusFailure line db]
          where
            info3 = show (pretty (info2 :: PV1.ScriptContext))
            line =
              pack $
                unlines
                  [ "\nThe 3 arg plutus script (" ++ name ++ ") fails.",
                    show e,
                    "The data is: " ++ show dat,
                    "The redeemer is: " ++ show redeemer,
                    "The context is:\n" ++ info3
                  ]
            db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
explainPlutusFailure _proxy lang scriptbytestring e ds@[redeemer, info] cm eu =
  -- A two data argument script.
  let ss :: Script era
      ss = PlutusScript lang scriptbytestring
      name :: String
      name = show ss
   in case PV1.fromData info of
        Nothing -> Fails [PlutusFailure line db]
          where
            line =
              pack $
                unlines
                  [ "\nThe 2 arg plutus script (" ++ name ++ ") fails.",
                    show e,
                    "The redeemer is: " ++ show redeemer,
                    "The second data argument, does not decode to a context\n" ++ show info
                  ]
            db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
        Just info2 -> Fails [PlutusFailure line db]
          where
            info3 = show (pretty (info2 :: PV1.ScriptContext))
            line =
              pack $
                unlines
                  [ "\nThe 2 arg plutus script (" ++ name ++ ") fails.",
                    show e,
                    "The redeemer is: " ++ show redeemer,
                    "The context is:\n" ++ info3
                  ]
            db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
explainPlutusFailure _proxy lang scriptbytestring e ds cm eu =
  -- A script with the wrong number of arguments
  Fails [PlutusFailure line db]
  where
    ss :: Script era
    ss = PlutusScript lang scriptbytestring
    name :: String
    name = show ss
    line =
      pack $
        unlines
          ( [ "\nThe plutus script (" ++ name ++ ") fails.",
              show e,
              "It was passed these " ++ show (Prelude.length ds) ++ " data arguments."
            ]
              ++ map show ds
          )
    db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
explain_plutus_failure = explainPlutusFailure
{-# DEPRECATED explain_plutus_failure "In favor of properly named `explainPlutusFailure`" #-}

validPlutusdata :: PV1.Data -> Bool
validPlutusdata (PV1.Constr _n ds) = all validPlutusdata ds
validPlutusdata (PV1.Map ds) =
  all (\(x, y) -> validPlutusdata x && validPlutusdata y) ds
validPlutusdata (PV1.List ds) = all validPlutusdata ds
validPlutusdata (PV1.I _n) = True
validPlutusdata (PV1.B bs) = BS.length bs <= 64

-- | Test that every Alonzo script represents a real Script.
--     Run deepseq to see that there are no infinite computations and that
--     every Plutus Script unflattens into a real PV1.Script
validScript :: Script era -> Bool
validScript scrip = case scrip of
  TimelockScript sc -> deepseq sc True
  PlutusScript PlutusV1 bytes -> PV1.validateScript bytes
  PlutusScript PlutusV2 bytes -> PV2.validateScript bytes
