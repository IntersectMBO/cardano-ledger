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

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary
  ( FromCBOR (fromCBOR),
    FromCBORGroup (..),
    ToCBOR (..),
    ToCBORGroup (..),
    TokenType (TypeMapLen, TypeMapLen64, TypeMapLenIndef),
    decodeRecordNamed,
    decodeRecordSum,
    decodeWord,
    encodeListLen,
    listLenInt,
    peekTokenType,
  )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    VerKeyVRF,
  )
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | The delegation of one stake key to another.
data Delegation c = Delegation
  { dDelegator :: !(StakeCredential c),
    dDelegatee :: !(KeyHash 'StakePool c)
  }
  deriving (Eq, Generic, Show, NFData)

instance NoThunks (Delegation c)

data DelegCert c
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential c)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential c)
  | -- | A stake delegation certificate.
    Delegate !(Delegation c)
  deriving (Show, Generic, Eq, NFData)

data PoolCert c
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams c)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool c) !EpochNo
  deriving (Show, Generic, Eq, NFData)

-- | Genesis key delegation certificate
data GenesisDelegCert c
  = GenesisDelegCert
      !(KeyHash 'Genesis c)
      !(KeyHash 'GenesisDelegate c)
      !(Hash c (VerKeyVRF c))
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
data MIRTarget c
  = StakeAddressesMIR (Map (Credential 'Staking c) DeltaCoin)
  | SendToOppositePotMIR Coin
  deriving (Show, Generic, Eq, NFData)

deriving instance NoThunks (MIRTarget c)

instance
  CC.Crypto c =>
  FromCBOR (MIRTarget c)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeMapLen -> StakeAddressesMIR <$> fromCBOR
      TypeMapLen64 -> StakeAddressesMIR <$> fromCBOR
      TypeMapLenIndef -> StakeAddressesMIR <$> fromCBOR
      _ -> SendToOppositePotMIR <$> fromCBOR

instance
  CC.Crypto c =>
  ToCBOR (MIRTarget c)
  where
  toCBOR (StakeAddressesMIR m) = toCBOR m
  toCBOR (SendToOppositePotMIR c) = toCBOR c

-- | Move instantaneous rewards certificate
data MIRCert c = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: MIRTarget c
  }
  deriving (Show, Generic, Eq, NFData)

instance
  CC.Crypto c =>
  FromCBOR (MIRCert c)
  where
  fromCBOR =
    decodeRecordNamed "MIRCert" (const 2) (MIRCert <$> fromCBOR <*> fromCBOR)

instance
  CC.Crypto c =>
  ToCBOR (MIRCert c)
  where
  toCBOR (MIRCert pot targets) =
    encodeListLen 2
      <> toCBOR pot
      <> toCBOR targets

-- | A heavyweight certificate.
data DCert c
  = DCertDeleg !(DelegCert c)
  | DCertPool !(PoolCert c)
  | DCertGenesis !(GenesisDelegCert c)
  | DCertMir !(MIRCert c)
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (DelegCert c)

instance NoThunks (PoolCert c)

instance NoThunks (GenesisDelegCert c)

instance NoThunks (MIRCert c)

instance NoThunks (DCert c)

-- CBOR

instance
  CC.Crypto c =>
  ToCBOR (DCert c)
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
  CC.Crypto c =>
  FromCBOR (DCert c)
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
delegCWitness :: DelegCert c -> Credential 'Staking c
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = dDelegator delegation

poolCWitness :: PoolCert c -> Credential 'StakePool c
poolCWitness (RegPool pool) = KeyHashObj $ ppId pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: GenesisDelegCert c -> KeyHash 'Genesis c
genesisCWitness (GenesisDelegCert gk _ _) = gk

-- | Check for 'RegKey' constructor
isRegKey :: DCert c -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: DCert c -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: DCert c -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: DCert c -> Bool
isGenesisDelegation (DCertGenesis GenesisDelegCert {}) = True
isGenesisDelegation _ = False

-- | Check for 'RegPool' constructor
isRegPool :: DCert c -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: DCert c -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: DCert c -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: DCert c -> Bool
isReservesMIRCert (DCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: DCert c -> Bool
isTreasuryMIRCert (DCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: DCert c -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
