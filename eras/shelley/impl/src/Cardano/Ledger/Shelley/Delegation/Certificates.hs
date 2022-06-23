{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Delegation.Certificates
  ( Delegation (..),
    DCert (..),
    DelegCert (..),
    PoolCert (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    delegCWitness,
    poolCWitness,
    genesisCWitness,
    isRegKey,
    isDeRegKey,
    isDelegation,
    isGenesisDelegation,
    isRegPool,
    isRetirePool,
    isInstantaneousRewards,
    isReservesMIRCert,
    isTreasuryMIRCert,
    requiresVKeyWitness,
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (..),
    TokenType (TypeMapLen, TypeMapLen64, TypeMapLenIndef),
    decodeWord,
    encodeListLen,
    peekTokenType,
  )
import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    VerKeyVRF,
  )
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordNamed,
    decodeRecordSum,
    listLenInt,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.PoolParams
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: !(StakeCredential crypto),
    _delegatee :: !(KeyHash 'StakePool crypto)
  }
  deriving (Eq, Generic, Show, NFData)

instance NoThunks (Delegation crypto)

data DelegCert crypto
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential crypto)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential crypto)
  | -- | A stake delegation certificate.
    Delegate !(Delegation crypto)
  deriving (Show, Generic, Eq, NFData)

data PoolCert crypto
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams crypto)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool crypto) !EpochNo
  deriving (Show, Generic, Eq, NFData)

-- | Genesis key delegation certificate
data GenesisDelegCert crypto
  = GenesisDelegCert
      !(KeyHash 'Genesis crypto)
      !(KeyHash 'GenesisDelegate crypto)
      !(Hash crypto (VerKeyVRF crypto))
  deriving (Show, Generic, Eq, NFData)

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData, Ord, Enum, Bounded)

deriving instance NoThunks MIRPot

instance ToCBOR MIRPot where
  toCBOR ReservesMIR = toCBOR (0 :: Word8)
  toCBOR TreasuryMIR = toCBOR (1 :: Word8)

instance FromCBOR MIRPot where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure ReservesMIR
      1 -> pure TreasuryMIR
      k -> invalidKey k

-- | MIRTarget specifies if funds from either the reserves
-- or the treasury are to be handed out to a collection of
-- reward accounts or instead transfered to the opposite pot.
data MIRTarget crypto
  = StakeAddressesMIR (Map (Credential 'Staking crypto) DeltaCoin)
  | SendToOppositePotMIR Coin
  deriving (Show, Generic, Eq, NFData)

deriving instance NoThunks (MIRTarget crypto)

instance
  CC.Crypto crypto =>
  FromCBOR (MIRTarget crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeMapLen -> StakeAddressesMIR <$> mapFromCBOR
      TypeMapLen64 -> StakeAddressesMIR <$> mapFromCBOR
      TypeMapLenIndef -> StakeAddressesMIR <$> mapFromCBOR
      _ -> SendToOppositePotMIR <$> fromCBOR

instance
  CC.Crypto crypto =>
  ToCBOR (MIRTarget crypto)
  where
  toCBOR (StakeAddressesMIR m) = mapToCBOR m
  toCBOR (SendToOppositePotMIR c) = toCBOR c

-- | Move instantaneous rewards certificate
data MIRCert crypto = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: MIRTarget crypto
  }
  deriving (Show, Generic, Eq, NFData)

instance
  CC.Crypto crypto =>
  FromCBOR (MIRCert crypto)
  where
  fromCBOR =
    decodeRecordNamed "MIRCert" (const 2) (MIRCert <$> fromCBOR <*> fromCBOR)

instance
  CC.Crypto crypto =>
  ToCBOR (MIRCert crypto)
  where
  toCBOR (MIRCert pot targets) =
    encodeListLen 2
      <> toCBOR pot
      <> toCBOR targets

-- | A heavyweight certificate.
data DCert crypto
  = DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  | DCertMir !(MIRCert crypto)
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (DelegCert crypto)

instance NoThunks (PoolCert crypto)

instance NoThunks (GenesisDelegCert crypto)

instance NoThunks (MIRCert crypto)

instance NoThunks (DCert crypto)

-- CBOR

instance
  CC.Crypto crypto =>
  ToCBOR (DCert crypto)
  where
  toCBOR = \case
    -- DCertDeleg
    DCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR cred
    DCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR cred
    DCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR cred
        <> toCBOR poolkh
    -- DCertPool
    DCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> toCBOR (3 :: Word8)
        <> toCBORGroup poolParams
    DCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch
    -- DCertGenesis
    DCertGenesis (GenesisDelegCert gk kh vrf) ->
      encodeListLen 4
        <> toCBOR (5 :: Word8)
        <> toCBOR gk
        <> toCBOR kh
        <> toCBOR vrf
    -- DCertMIR
    DCertMir mir ->
      encodeListLen 2
        <> toCBOR (6 :: Word8)
        <> toCBOR mir

instance
  CC.Crypto crypto =>
  FromCBOR (DCert crypto)
  where
  fromCBOR = decodeRecordSum "DCert crypto" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . RegKey $ x)
      1 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . DeRegKey $ x)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- fromCBORGroup
        pure (fromIntegral (1 + listLenInt group), DCertPool (RegPool group))
      4 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, DCertGenesis $ GenesisDelegCert a b c)
      6 -> do
        x <- fromCBOR
        pure (2, DCertMir x)
      k -> invalidKey k

-- | Determine the certificate author
delegCWitness :: DelegCert crypto -> Credential 'Staking crypto
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = _delegator delegation

poolCWitness :: PoolCert crypto -> Credential 'StakePool crypto
poolCWitness (RegPool pool) = KeyHashObj $ _poolId pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: GenesisDelegCert crypto -> KeyHash 'Genesis crypto
genesisCWitness (GenesisDelegCert gk _ _) = gk

-- | Check for 'RegKey' constructor
isRegKey :: DCert crypto -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: DCert crypto -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: DCert crypto -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: DCert crypto -> Bool
isGenesisDelegation (DCertGenesis GenesisDelegCert {}) = True
isGenesisDelegation _ = False

-- | Check for 'RegPool' constructor
isRegPool :: DCert crypto -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: DCert crypto -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: DCert crypto -> Bool
isReservesMIRCert (DCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: DCert crypto -> Bool
isTreasuryMIRCert (DCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: DCert crypto -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
