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
import Cardano.Crypto.Hash.Class (Hash (UnsafeHash), HashAlgorithm)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data (..), getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
-- Instances only

import Cardano.Ledger.Alonzo.PParams (ProtVer)
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
import Cardano.Ledger.Alonzo.TxWitness (TxWitness, unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
    TxId (..),
    TxIn (..),
    Wdrl (..),
    WitVKey (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Control.DeepSeq (deepseq)
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
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import qualified Plutus.V1.Ledger.Api as P
  ( Address (..),
    BuiltinByteString,
    Credential (..),
    CurrencySymbol (..),
    DCert (..),
    Data (..),
    Datum (..),
    DatumHash (..),
    EvaluationError (..),
    ExBudget (..),
    ExCPU (..),
    ExMemory (..),
    Interval (..),
    POSIXTime (..),
    POSIXTimeRange,
    PubKeyHash (..),
    ScriptContext (..),
    ScriptPurpose (..),
    StakingCredential (..),
    TokenName (..),
    TxId (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
    ValidatorHash (..),
    Value (..),
    VerboseMode (..),
    adaSymbol,
    adaToken,
    always,
    dataToBuiltinData,
    evaluateScriptRestricting,
    from,
    fromData,
    lowerBound,
    singleton,
    strictUpperBound,
    to,
    toBuiltin,
    toData,
    unionWith,
    validateScript,
  )
import Plutus.V1.Ledger.Contexts ()

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

transDataHash :: CC.Crypto c => StrictMaybe (DataHash c) -> Maybe P.DatumHash
transDataHash (SJust safe) = Just (transDataHash' safe)
transDataHash SNothing = Nothing

transDataHash' :: CC.Crypto c => DataHash c -> P.DatumHash
transDataHash' safe = P.DatumHash (transSafeHash safe)

transKeyHash :: CC.Crypto c => KeyHash d c -> P.PubKeyHash
transKeyHash (KeyHash (UnsafeHash h)) = P.PubKeyHash (P.toBuiltin (fromShort h))

transScriptHash :: CC.Crypto c => ScriptHash c -> P.ValidatorHash
transScriptHash (ScriptHash (UnsafeHash h)) = P.ValidatorHash (P.toBuiltin (fromShort h))

transSafeHash :: CC.Crypto c => SafeHash c i -> P.BuiltinByteString
transSafeHash safe = case extractHash safe of UnsafeHash b -> P.toBuiltin (fromShort b)

transHash :: HashAlgorithm h => Hash h a -> BS.ByteString
transHash (UnsafeHash h) = fromShort h

txInfoId :: CC.Crypto crypto => TxId crypto -> P.TxId
txInfoId (TxId safe) = P.TxId (transSafeHash safe)

transStakeCred :: CC.Crypto crypto => Credential keyrole crypto -> P.Credential
transStakeCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) =
  P.ScriptCredential (P.ValidatorHash (P.toBuiltin (fromShort kh)))
transStakeCred (KeyHashObj (KeyHash (UnsafeHash kh))) =
  P.PubKeyCredential (P.PubKeyHash (P.toBuiltin (fromShort kh)))

transStakeReference :: CC.Crypto crypto => StakeReference crypto -> Maybe P.StakingCredential
transStakeReference (StakeRefBase cred) = Just (P.StakingHash (transStakeCred cred))
transStakeReference (StakeRefPtr (Ptr (SlotNo slot) i1 i2)) =
  Just (P.StakingPtr (fromIntegral slot) (fromIntegral i1) (fromIntegral i2))
transStakeReference StakeRefNull = Nothing

transCred :: CC.Crypto crypto => Credential keyrole crypto -> P.Credential
transCred (KeyHashObj (KeyHash (UnsafeHash kh))) =
  P.PubKeyCredential (P.PubKeyHash (P.toBuiltin (fromShort kh)))
transCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) =
  P.ScriptCredential (P.ValidatorHash (P.toBuiltin (fromShort kh)))

transAddr :: CC.Crypto crypto => Addr crypto -> Maybe P.Address
transAddr (Addr _net object stake) = Just (P.Address (transCred object) (transStakeReference stake))
transAddr (AddrBootstrap _bootaddr) = Nothing

slotToPOSIXTime ::
  (Monad m, HasField "_protocolVersion" (PParams era) ProtVer) =>
  Core.PParams era ->
  EpochInfo m ->
  SystemStart ->
  SlotNo ->
  m P.POSIXTime
slotToPOSIXTime pp ei sysS s = do
  P.POSIXTime . transTime . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
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
  m P.POSIXTimeRange
transVITime _ _ _ (ValidityInterval SNothing SNothing) = pure P.always
transVITime pp ei sysS (ValidityInterval (SJust i) SNothing) = do
  t <- slotToPOSIXTime pp ei sysS i
  pure $ P.from t
transVITime pp ei sysS (ValidityInterval SNothing (SJust i)) = do
  t <- slotToPOSIXTime pp ei sysS i
  pure $ P.to t
transVITime pp ei sysS (ValidityInterval (SJust i) (SJust j)) = do
  t1 <- slotToPOSIXTime pp ei sysS i
  t2 <- slotToPOSIXTime pp ei sysS j
  pure $
    P.Interval
      (P.lowerBound t1)
      (P.strictUpperBound t2)

-- ========================================
-- translate TxIn and TxOut

txInfoIn' :: CC.Crypto c => TxIn c -> P.TxOutRef
txInfoIn' (TxIn txid nat) = P.TxOutRef (txInfoId txid) (fromIntegral nat)

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
  Maybe P.TxInInfo
txInfoIn (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Nothing
    Just txout -> case transAddr addr of
      Just ad -> Just (P.TxInInfo (txInfoIn' txin) (P.TxOut ad valout dhash))
      Nothing -> Nothing
      where
        valout = transValue (getField @"value" txout)
        addr = getField @"address" txout
        dhash = case getField @"datahash" txout of
          SNothing -> Nothing
          SJust safehash -> Just (P.DatumHash (transSafeHash safehash))

-- | Given a TxOut, translate it and return (Just transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Nothing
--   I.e. don't include Bootstrap Addresses in the answer.
txInfoOut ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  Alonzo.TxOut era ->
  Maybe P.TxOut
txInfoOut (Alonzo.TxOut addr val datahash) =
  case transAddr addr of
    Just ad -> Just (P.TxOut ad (transValue @(Crypto era) val) (transDataHash datahash))
    Nothing -> Nothing

-- ==================================
-- translate Values

transPolicyID :: CC.Crypto crypto => Mary.PolicyID crypto -> P.CurrencySymbol
transPolicyID (Mary.PolicyID (ScriptHash (UnsafeHash x))) = P.CurrencySymbol (P.toBuiltin (fromShort x))

transAssetName :: Mary.AssetName -> P.TokenName
transAssetName (Mary.AssetName bs) = P.TokenName (P.toBuiltin bs)

transValue :: forall c. CC.Crypto c => Mary.Value c -> P.Value
transValue (Mary.Value n mp) = Map.foldlWithKey' accum1 justada mp
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          P.unionWith
            (+)
            ans2
            (P.singleton (transPolicyID sym) (transAssetName tok) quantity)
    justada = P.singleton P.adaSymbol P.adaToken n

-- =============================================
-- translate fileds like DCert, Wdrl, and similar

transDCert :: CC.Crypto c => DCert c -> P.DCert
transDCert (DCertDeleg (RegKey stkcred)) =
  P.DCertDelegRegKey (P.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (DeRegKey stkcred)) =
  P.DCertDelegDeRegKey (P.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (Delegate (Delegation stkcred keyhash))) =
  P.DCertDelegDelegate
    (P.StakingHash (transStakeCred stkcred))
    (transKeyHash keyhash)
transDCert (DCertPool (RegPool pp)) =
  P.DCertPoolRegister (transKeyHash (_poolId pp)) (P.PubKeyHash (P.toBuiltin (transHash (_poolVrf pp))))
transDCert (DCertPool (RetirePool keyhash (EpochNo i))) =
  P.DCertPoolRetire (transKeyHash keyhash) (fromIntegral i)
transDCert (DCertGenesis _) = P.DCertGenesis
transDCert (DCertMir _) = P.DCertMir

transWdrl :: CC.Crypto crypto => Wdrl crypto -> Map.Map P.StakingCredential Integer
transWdrl (Wdrl mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) =
      Map.insert (P.StakingHash (transStakeCred cred)) n ans

getWitVKeyHash :: (CC.Crypto crypto, Typeable kr) => WitVKey kr crypto -> P.PubKeyHash
getWitVKeyHash =
  P.PubKeyHash
    . P.toBuiltin
    . fromShort
    . (\(UnsafeHash x) -> x)
    . (\(KeyHash x) -> x)
    . hashKey
    . (\(WitVKey x _) -> x)

transDataPair :: CC.Crypto c => (DataHash c, Data era) -> (P.DatumHash, P.Datum)
transDataPair (x, y) = (transDataHash' x, P.Datum (P.dataToBuiltinData (getPlutusData y)))

transExUnits :: ExUnits -> P.ExBudget
transExUnits (ExUnits mem steps) =
  P.ExBudget (P.ExCPU (fromIntegral steps)) (P.ExMemory (fromIntegral mem))

exBudgetToExUnits :: P.ExBudget -> Maybe ExUnits
exBudgetToExUnits (P.ExBudget (P.ExCPU steps) (P.ExMemory memory)) =
  ExUnits <$> safeFromInteger (toInteger memory)
    <*> safeFromInteger (toInteger steps)
  where
    safeFromInteger :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
    safeFromInteger i
      | toInteger (minBound :: a) <= i && i <= toInteger (maxBound :: a) = Just $ fromInteger i
      | otherwise = Nothing

-- ===================================
-- translate Script Purpose

transScriptPurpose :: CC.Crypto crypto => ScriptPurpose crypto -> P.ScriptPurpose
transScriptPurpose (Minting policyid) = P.Minting (transPolicyID policyid)
transScriptPurpose (Spending txin) = P.Spending (txInfoIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) =
  P.Rewarding (P.StakingHash (transStakeCred cred))
transScriptPurpose (Certifying dcert) = P.Certifying (transDCert dcert)

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
  EpochInfo m ->
  SystemStart ->
  UTxO era ->
  tx ->
  m P.TxInfo
txInfo pp ei sysS utxo tx = do
  timeRange <- transVITime pp ei sysS interval
  pure $
    P.TxInfo
      { P.txInfoInputs = mapMaybe (txInfoIn utxo) (Set.toList (inputs' tbody)),
        P.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] outs),
        P.txInfoFee = transValue (inject @(Mary.Value (Crypto era)) fee),
        P.txInfoMint = transValue forge,
        P.txInfoDCert = foldr (\c ans -> transDCert c : ans) [] (certs' tbody),
        P.txInfoWdrl = Map.toList (transWdrl (wdrls' tbody)),
        P.txInfoValidRange = timeRange,
        P.txInfoSignatories = map transKeyHash (Set.toList (reqSignerHashes' tbody)),
        P.txInfoData = map transDataPair datpairs,
        P.txInfoId = P.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody))
      }
  where
    tbody = getField @"body" tx
    _witnesses = getField @"wits" tx
    outs = outputs' tbody
    fee = txfee' tbody
    forge = mint' tbody
    interval = vldt' tbody
    datpairs = Map.toList (unTxDats $ txdats' _witnesses)

-- ===============================================================
-- From the specification, Figure 7 "Script Validation, cont."
-- ===============================================================

-- | valContext collects info from the Tx and the UTxO an
--   translates it into a 'Data', which the Plutus language knows how to interpret.
--   The UTxO and the PtrMap are used to 'resolve' the TxIn and the StakeRefPtr's
valContext ::
  Era era =>
  P.TxInfo ->
  ScriptPurpose (Crypto era) ->
  Data era
valContext txinfo sp = Data (P.toData (P.ScriptContext txinfo (transScriptPurpose sp)))

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
    debugData :: [P.Data],
    debugVersion :: Language
  }
  deriving (Show)

data PlutusDebugInfo
  = DebugSuccess
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

debugPlutus :: String -> PlutusDebugInfo
debugPlutus db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      case decodeFull' bs of
        Left e -> DebugCannotDecode (show e)
        Right (PlutusDebug (CostModel cost) units script ds _version) ->
          case P.evaluateScriptRestricting
            P.Verbose
            cost
            (transExUnits units)
            script
            ds of
            (logs, Left e) -> DebugInfo logs (show e)
            (_, Right ()) -> DebugSuccess

-- The runPLCScript in the Specification has a slightly different type
-- than the one in the implementation below. Made necessary by the the type
-- of P.evaluateScriptRestricting which is the interface to Plutus, and in the impementation
-- we try to track why a script failed (if it does) by the [String] in the Fails constructor of ScriptResut.

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
runPLCScript ::
  forall era.
  Show (Script era) =>
  Proxy era ->
  CostModel ->
  SBS.ShortByteString ->
  ExUnits ->
  [P.Data] ->
  ScriptResult
runPLCScript proxy (CostModel cost) scriptbytestring units ds =
  case P.evaluateScriptRestricting
    P.Quiet
    cost
    (transExUnits units)
    scriptbytestring
    ds of
    (_, Left e) -> explainPlutusFailure proxy scriptbytestring e ds (CostModel cost) units
    (_, Right ()) -> Passes

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
    SBS.ShortByteString ->
    P.EvaluationError ->
    [P.Data] ->
    CostModel ->
    ExUnits ->
    ScriptResult
explainPlutusFailure _proxy scriptbytestring e ds@[dat, redeemer, info] cm eu =
  -- A three data argument script.
  let ss :: Script era
      ss = PlutusScript scriptbytestring
      name :: String
      name = show ss
   in case P.fromData info of
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
            info3 = show (pretty (info2 :: P.ScriptContext))
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
explainPlutusFailure _proxy scriptbytestring e ds@[redeemer, info] cm eu =
  -- A two data argument script.
  let ss :: Script era
      ss = PlutusScript scriptbytestring
      name :: String
      name = show ss
   in case P.fromData info of
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
            info3 = show (pretty (info2 :: P.ScriptContext))
            line =
              pack $
                unlines
                  [ "\nThe 2 arg plutus script (" ++ name ++ ") fails.",
                    show e,
                    "The redeemer is: " ++ show redeemer,
                    "The context is:\n" ++ info3
                  ]
            db = B64.encode . serialize' $ PlutusDebug cm eu scriptbytestring ds PlutusV1
explainPlutusFailure _proxy scriptbytestring e ds cm eu =
  -- A script with the wrong number of arguments
  Fails [PlutusFailure line db]
  where
    ss :: Script era
    ss = PlutusScript scriptbytestring
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

validPlutusdata :: P.Data -> Bool
validPlutusdata (P.Constr _n ds) = all validPlutusdata ds
validPlutusdata (P.Map ds) =
  all (\(x, y) -> validPlutusdata x && validPlutusdata y) ds
validPlutusdata (P.List ds) = all validPlutusdata ds
validPlutusdata (P.I _n) = True
validPlutusdata (P.B bs) = BS.length bs <= 64

-- | Test that every Alonzo script represents a real Script.
--     Run deepseq to see that there are no infinite computations and that
--     every Plutus Script unflattens into a real P.Script
validScript :: Script era -> Bool
validScript scrip = case scrip of
  TimelockScript sc -> deepseq sc True
  PlutusScript bytes -> P.validateScript bytes
