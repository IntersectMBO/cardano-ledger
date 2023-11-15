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

module Cardano.Ledger.Plutus.TxInfo (
  TxOutSource (..),
  transProtocolVersion,
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
  txInfoIn',
  transWithdrawals,
  getWitVKeyHash,
  transDataPair,
  transExUnits,
  exBudgetToExUnits,
  VersionedTxInfo (..),
  EraPlutusContext (..),
  PlutusTxCert (..),
  unTxCertV1,
  unTxCertV2,
  unTxCertV3,
)
where

import Cardano.Crypto.Hash.Class (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (
  ProtVer (..),
  StrictMaybe (..),
  TxIx,
  certIxToInt,
  getVersion64,
  txIxToInt,
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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), hashKey)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus.Data (Data (..), getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SystemStart)
import Data.ByteString as BS (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import PlutusLedgerApi.V1 (SatInt, fromSatInt)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

-- | A transaction output can be translated because it is a newly created output,
-- or because it is the output which is connected to a transaction input being spent.
data TxOutSource c
  = TxOutFromInput !(TxIn c)
  | TxOutFromOutput !TxIx
  deriving (Eq, Show, Generic, NoThunks)

instance ToExpr (TxOutSource era)

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

transProtocolVersion :: ProtVer -> PV1.MajorProtocolVersion
transProtocolVersion (ProtVer major _minor) =
  PV1.MajorProtocolVersion ((fromIntegral :: Word64 -> Int) (getVersion64 major))

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

-- ========================================
-- translate TxIn and TxOut

txInfoIn' :: TxIn c -> PV1.TxOutRef
txInfoIn' (TxIn txid txIx) = PV1.TxOutRef (txInfoId txid) (toInteger (txIxToInt txIx))

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

data VersionedTxInfo
  = TxInfoPV1 PV1.TxInfo
  | TxInfoPV2 PV2.TxInfo
  | TxInfoPV3 PV3.TxInfo
  deriving (Show, Eq, Generic)
