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
  txOutSourceToText,
  transAddr,
  transRewardAccount,
  transDataHash,
  transKeyHash,
  transSafeHash,
  transScriptHash,
  transTxId,
  transStakeReference,
  transCred,
  slotToPOSIXTime,
  transTxIn,
  transCoinToValue,
  transCoinToLovelace,
  transDataPair,
  transExUnits,
  exBudgetToExUnits,
  transBoundedRational,
  transEpochNo,
  transEpochInterval,
  transDatum,
)
where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  BoundedRational (unboundRational),
  EpochInterval (..),
  EpochNo (..),
  TxIx,
  certIxToInt,
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
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Plutus.Data (Data (..), getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), txInToText)
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Text as T (Text, pack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import PlutusLedgerApi.V1 (SatInt, fromSatInt)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V3 as PV3

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

-- | A transaction output can be translated because it is a newly created output,
-- or because it is the output which is connected to a transaction input being spent.
data TxOutSource c
  = TxOutFromInput !(TxIn c)
  | TxOutFromOutput !TxIx
  deriving (Eq, Show, Generic, NoThunks)

instance NFData (TxOutSource era) where
  rnf = rwhnf

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

instance ToJSON (TxOutSource c) where
  toJSON = String . txOutSourceToText

txOutSourceToText :: TxOutSource c -> Text
txOutSourceToText = \case
  TxOutFromInput txIn -> "Input: " <> txInToText txIn
  TxOutFromOutput txIx -> "Output: " <> T.pack (show txIx)

transBoundedRational :: BoundedRational r => r -> PV3.Rational
transBoundedRational = PV3.fromGHC . unboundRational

transDataHash :: DataHash c -> PV1.DatumHash
transDataHash safe = PV1.DatumHash (transSafeHash safe)

transKeyHash :: KeyHash d c -> PV1.PubKeyHash
transKeyHash (KeyHash h) = PV1.PubKeyHash (PV1.toBuiltin (hashToBytes h))

transSafeHash :: SafeHash c i -> PV1.BuiltinByteString
transSafeHash = PV1.toBuiltin . hashToBytes . extractHash

transScriptHash :: ScriptHash c -> PV1.ScriptHash
transScriptHash (ScriptHash h) = PV1.ScriptHash (PV1.toBuiltin (hashToBytes h))

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

-- | Translate an address. `Cardano.Ledger.BaseTypes.NetworkId` is discarded and Byron
-- Addresses will result in Nothing.
transAddr :: Addr c -> Maybe PV1.Address
transAddr = \case
  AddrBootstrap {} -> Nothing
  Addr _networkId paymentCred stakeReference ->
    Just (PV1.Address (transCred paymentCred) (transStakeReference stakeReference))

-- | Translate reward account by discarding `NetowrkId` and only translating the staking credential.
--
-- /Note/ - This function is the right one to use starting with PlutusV3, prior to that an
-- extra `PV1.StakingHash` wrapper is needed.
transRewardAccount :: RewardAccount c -> PV1.Credential
transRewardAccount (RewardAccount _networkId cred) = transCred cred

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

transTxId :: TxId c -> PV1.TxId
transTxId (TxId safe) = PV1.TxId (transSafeHash safe)

transTxIn :: TxIn c -> PV1.TxOutRef
transTxIn (TxIn txid txIx) = PV1.TxOutRef (transTxId txid) (toInteger (txIxToInt txIx))

transCoinToValue :: Coin -> PV1.Value
transCoinToValue (Coin c) = PV1.singleton PV1.adaSymbol PV1.adaToken c

transDataPair :: (DataHash c, Data era) -> (PV1.DatumHash, PV1.Datum)
transDataPair (x, y) = (transDataHash x, PV1.Datum (PV1.dataToBuiltinData (getPlutusData y)))

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

transCoinToLovelace :: Coin -> PV1.Lovelace
transCoinToLovelace (Coin c) = PV1.Lovelace c

transEpochNo :: EpochNo -> Integer
transEpochNo = toInteger . unEpochNo

transEpochInterval :: EpochInterval -> Integer
transEpochInterval = toInteger . unEpochInterval

transDatum :: Data era -> PV3.Datum
transDatum = PV1.Datum . PV1.dataToBuiltinData . getPlutusData
