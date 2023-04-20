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

module Cardano.Ledger.Shelley.Delegation.Certificates (
  Delegation (..),
  DCert (..),
  DelegCert (..),
  PoolCert (..),
  ConstitutionalDelegCert (..),
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
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
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
import Cardano.Ledger.Keys (
  GenesisVRF,
  Hash,
  KeyHash (..),
  KeyRole (..),
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
  { dDelegator :: !(StakeCredential c)
  , dDelegatee :: !(KeyHash 'StakePool c)
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
data ConstitutionalDelegCert c
  = ConstitutionalDelegCert
      !(KeyHash 'Genesis c)
      !(KeyHash 'GenesisDelegate c)
      !(Hash c GenesisVRF)
  deriving (Show, Generic, Eq, NFData)

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData, Ord, Enum, Bounded)

deriving instance NoThunks MIRPot

instance EncCBOR MIRPot where
  encCBOR ReservesMIR = encCBOR (0 :: Word8)
  encCBOR TreasuryMIR = encCBOR (1 :: Word8)

instance DecCBOR MIRPot where
  decCBOR =
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
  DecCBOR (MIRTarget c)
  where
  decCBOR = do
    peekTokenType >>= \case
      TypeMapLen -> StakeAddressesMIR <$> decCBOR
      TypeMapLen64 -> StakeAddressesMIR <$> decCBOR
      TypeMapLenIndef -> StakeAddressesMIR <$> decCBOR
      _ -> SendToOppositePotMIR <$> decCBOR

instance
  CC.Crypto c =>
  EncCBOR (MIRTarget c)
  where
  encCBOR (StakeAddressesMIR m) = encCBOR m
  encCBOR (SendToOppositePotMIR c) = encCBOR c

-- | Move instantaneous rewards certificate
data MIRCert c = MIRCert
  { mirPot :: MIRPot
  , mirRewards :: MIRTarget c
  }
  deriving (Show, Generic, Eq, NFData)

instance
  CC.Crypto c =>
  DecCBOR (MIRCert c)
  where
  decCBOR =
    decodeRecordNamed "MIRCert" (const 2) (MIRCert <$> decCBOR <*> decCBOR)

instance
  CC.Crypto c =>
  EncCBOR (MIRCert c)
  where
  encCBOR (MIRCert pot targets) =
    encodeListLen 2
      <> encCBOR pot
      <> encCBOR targets

-- | A heavyweight certificate.
data DCert c
  = DCertDeleg !(DelegCert c)
  | DCertPool !(PoolCert c)
  | DCertGenesis !(ConstitutionalDelegCert c)
  | DCertMir !(MIRCert c)
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (DelegCert c)

instance NoThunks (PoolCert c)

instance NoThunks (ConstitutionalDelegCert c)

instance NoThunks (MIRCert c)

instance NoThunks (DCert c)

-- CBOR

instance
  CC.Crypto c =>
  EncCBOR (DCert c)
  where
  encCBOR = \case
    -- DCertDeleg
    DCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR cred
    DCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR cred
    DCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR cred
        <> encCBOR poolkh
    -- DCertPool
    DCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> encCBOR (3 :: Word8)
        <> encCBORGroup poolParams
    DCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR vk
        <> encCBOR epoch
    -- DCertGenesis
    DCertGenesis (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> encCBOR (5 :: Word8)
        <> encCBOR gk
        <> encCBOR kh
        <> encCBOR vrf
    -- DCertMIR
    DCertMir mir ->
      encodeListLen 2
        <> encCBOR (6 :: Word8)
        <> encCBOR mir

instance
  CC.Crypto c =>
  DecCBOR (DCert c)
  where
  decCBOR = decodeRecordSum "DCert crypto" $
    \case
      0 -> do
        x <- decCBOR
        pure (2, DCertDeleg . RegKey $ x)
      1 -> do
        x <- decCBOR
        pure (2, DCertDeleg . DeRegKey $ x)
      2 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, DCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- decCBORGroup
        pure (fromIntegral (1 + listLenInt group), DCertPool (RegPool group))
      4 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, DCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- decCBOR
        b <- decCBOR
        c <- decCBOR
        pure (4, DCertGenesis $ ConstitutionalDelegCert a b c)
      6 -> do
        x <- decCBOR
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

genesisCWitness :: ConstitutionalDelegCert c -> KeyHash 'Genesis c
genesisCWitness (ConstitutionalDelegCert gk _ _) = gk

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
isGenesisDelegation (DCertGenesis ConstitutionalDelegCert {}) = True
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
