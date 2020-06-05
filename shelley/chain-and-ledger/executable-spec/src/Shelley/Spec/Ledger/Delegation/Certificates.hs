{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    PoolCert (..),
    GenesisDelegCert (..),
    MIRCert (..),
    StakeCreds (..),
    StakePools (..),
    PoolDistr (..),
    delegCWitness,
    poolCWitness,
    genesisCWitness,
    dvalue,
    isRegKey,
    isDeRegKey,
    isDelegation,
    isGenesisDelegation,
    isRegPool,
    isRetirePool,
    isInstantaneousRewards,
    requiresVKeyWitness,
  )
where

import Byron.Spec.Ledger.Core (Relation (..))
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude (NoUnexpectedThunks (..))
import Data.Map.Strict (Map)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    PoolCert (..),
    PoolParams (..),
    StakeCreds (..),
    StakePools (..),
  )

-- | Determine the certificate author
delegCWitness :: DelegCert crypto -> Credential 'Staking crypto
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = _delegator delegation

poolCWitness :: PoolCert crypto -> Credential 'StakePool crypto
poolCWitness (RegPool pool) = KeyHashObj $ _poolPubKey pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: GenesisDelegCert crypto -> KeyHash 'Genesis crypto
genesisCWitness (GenesisDelegCert gk _ _) = gk

-- | Retrieve the deposit amount for a certificate
dvalue :: DCert crypto -> PParams -> Coin
dvalue (DCertDeleg (RegKey _)) = _keyDeposit
dvalue (DCertPool (RegPool _)) = _poolDeposit
dvalue _ = const $ Coin 0

-- | Check for `RegKey` constructor
isRegKey :: DCert crypto -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for `DeRegKey` constructor
isDeRegKey :: DCert crypto -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for `Delegation` constructor
isDelegation :: DCert crypto -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for `GenesisDelegate` constructor
isGenesisDelegation :: DCert crypto -> Bool
isGenesisDelegation (DCertGenesis (GenesisDelegCert {})) = True
isGenesisDelegation _ = False

-- | Check for `RegPool` constructor
isRegPool :: DCert crypto -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for `RetirePool` constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

newtype PoolDistr crypto = PoolDistr
  { unPoolDistr ::
      ( Map
          (KeyHash 'StakePool crypto)
          (Rational, Hash crypto (VerKeyVRF crypto))
      )
  }
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks, Relation)

isInstantaneousRewards :: DCert crypto -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of `cwitness` is safe.
requiresVKeyWitness :: DCert crypto -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
