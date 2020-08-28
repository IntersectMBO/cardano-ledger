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
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Data.Map.Strict (Map)
import Data.Relation (Relation (..))
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.TxData
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

instance HasExp (PoolDistr era) (Map (KeyHash 'StakePool era) (IndividualPoolStake era)) where
  toExp (PoolDistr x) = Base MapR x

instance Embed (PoolDistr era) (Map (KeyHash 'StakePool era) (IndividualPoolStake era)) where
  toBase (PoolDistr x) = x
  fromBase x = (PoolDistr x)

-- | Determine the certificate author
delegCWitness :: DelegCert era -> Credential 'Staking era
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = _delegator delegation

poolCWitness :: PoolCert era -> Credential 'StakePool era
poolCWitness (RegPool pool) = KeyHashObj $ _poolPubKey pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: GenesisDelegCert era -> KeyHash 'Genesis era
genesisCWitness (GenesisDelegCert gk _ _) = gk

-- | Check for `RegKey` constructor
isRegKey :: DCert era -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for `DeRegKey` constructor
isDeRegKey :: DCert era -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for `Delegation` constructor
isDelegation :: DCert era -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for `GenesisDelegate` constructor
isGenesisDelegation :: DCert era -> Bool
isGenesisDelegation (DCertGenesis (GenesisDelegCert {})) = True
isGenesisDelegation _ = False

-- | Check for `RegPool` constructor
isRegPool :: DCert era -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for `RetirePool` constructor
isRetirePool :: DCert era -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

newtype PoolDistr era = PoolDistr
  { unPoolDistr ::
      Map (KeyHash 'StakePool era) (IndividualPoolStake era)
  }
  deriving stock (Show, Eq)
  deriving newtype (ToCBOR, FromCBOR, NFData, NoUnexpectedThunks, Relation)

data IndividualPoolStake era = IndividualPoolStake
  { individualPoolStake :: !Rational,
    individualPoolStakeVrf :: !(Hash era (VerKeyVRF era))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoUnexpectedThunks)

instance Era era => ToCBOR (IndividualPoolStake era) where
  toCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2,
        toCBOR stake,
        toCBOR vrf
      ]

instance Era era => FromCBOR (IndividualPoolStake era) where
  fromCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> fromCBOR
        <*> fromCBOR

isInstantaneousRewards :: DCert era -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: DCert era -> Bool
isReservesMIRCert (DCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: DCert era -> Bool
isTreasuryMIRCert (DCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of `cwitness` is safe.
requiresVKeyWitness :: DCert era -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
