{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    PoolCert (..),
    GenesisDelegCert (..),
    MIRCert (..),
    StakeCreds (..),
    PoolDistr (..),
    IndividualPoolStake (..),
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

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Control.DeepSeq (NFData)
import Control.Iterate.SetAlgebra
  ( BaseRep (MapR),
    Embed (..),
    Exp (Base),
    HasExp (toExp),
  )
import Data.Map.Strict (Map)
import Data.Relation (Relation (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolParams (..),
    StakeCreds (..),
  )

instance
  HasExp
    (PoolDistr crypto)
    ( Map
        (KeyHash 'StakePool crypto)
        (IndividualPoolStake crypto)
    )
  where
  toExp (PoolDistr x) = Base MapR x

instance
  Embed
    (PoolDistr crypto)
    ( Map
        (KeyHash 'StakePool crypto)
        (IndividualPoolStake crypto)
    )
  where
  toBase (PoolDistr x) = x
  fromBase x = (PoolDistr x)

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
isGenesisDelegation (DCertGenesis (GenesisDelegCert {})) = True
isGenesisDelegation _ = False

-- | Check for 'RegPool' constructor
isRegPool :: DCert crypto -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

newtype PoolDistr crypto = PoolDistr
  { unPoolDistr ::
      Map (KeyHash 'StakePool crypto) (IndividualPoolStake crypto)
  }
  deriving stock (Show, Eq)
  deriving newtype (ToCBOR, FromCBOR, NFData, NoThunks, Relation)

data IndividualPoolStake crypto = IndividualPoolStake
  { individualPoolStake :: !Rational,
    individualPoolStakeVrf :: !(Hash crypto (VerKeyVRF crypto))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

instance CC.Crypto crypto => ToCBOR (IndividualPoolStake crypto) where
  toCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2,
        toCBOR stake,
        toCBOR vrf
      ]

instance CC.Crypto crypto => FromCBOR (IndividualPoolStake crypto) where
  fromCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> fromCBOR
        <*> fromCBOR

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
