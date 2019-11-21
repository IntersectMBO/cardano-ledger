{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Delegation.Certificates
  (
    DCert(..)
  , StakeCreds(..)
  , StakePools(..)
  , PoolDistr(..)
  , cwitness
  , dvalue
  , refund
  , releasing
  , allocating
  , dretire
  , dderegister
  , decayKey
  , decayPool
  , isRegKey
  , isDeRegKey
  , isRegPool
  , isRetirePool
  , isInstantaneousRewards
  , requiresVKeyWitness
  ) where

import           BaseTypes (FixedPoint, UnitInterval, fpEpsilon, intervalValue)
import           Cardano.Ledger.Shelley.Crypto
import           Coin (Coin (..))
import           Keys (Hash, KeyHash, VRFAlgorithm (VerKeyVRF))
import           Ledger.Core (Relation (..))
import           NonIntegral (exp')
import           PParams (PParams (..), keyDecayRate, keyDeposit, keyMinRefund, poolDecayRate,
                     poolDeposit, poolMinRefund)
import           Slot (Duration (..))
import           TxData (Credential (..), DCert (..), StakeCredential, StakeCreds (..),
                     StakePools (..), delegator, poolPubKey)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Map.Strict (Map)
import           Data.Ratio (approxRational)

import           Lens.Micro ((^.))

-- |Determine the certificate author
cwitness :: DCert crypto-> StakeCredential crypto
cwitness (RegKey _)                = error "no witness in key registration certificate"
cwitness (DeRegKey hk)             = hk
cwitness (RegPool pool)            = KeyHashObj $ pool ^. poolPubKey
cwitness (RetirePool k _)          = KeyHashObj k
cwitness (Delegate delegation)     = delegation ^. delegator
cwitness (GenesisDelegate (gk, _)) = GenesisHashObj gk
cwitness (InstantaneousRewards _)  = error "no witness in MIR certificate"

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert crypto-> PParams -> Coin
dvalue (RegKey _)  = flip (^.) keyDeposit
dvalue (RegPool _) = flip (^.) poolDeposit
dvalue _ = const $ Coin 0

-- |Compute a refund on a deposit
refund :: Coin -> UnitInterval -> Rational -> Duration -> Coin
refund (Coin dval) dmin lambda delta = floor refund'
  where
    pow     = fromRational (-lambda * fromIntegral delta) :: FixedPoint
    refund' = fromIntegral dval * (dmin' + (1 - dmin') * dCay)
    dmin'   = intervalValue dmin
    dCay    = approxRational (exp' pow) fpEpsilon

-- | Check whether certificate is of releasing type, i.e., key deregistration or
-- pool retirement.
releasing :: DCert crypto-> Bool
releasing c = dderegister c || dretire c

dderegister :: DCert crypto-> Bool
dderegister (DeRegKey _) = True
dderegister _            = False

dretire :: DCert crypto-> Bool
dretire (RetirePool _ _) = True
dretire _                = False

-- | Check whether certificate is of allocating type, i.e, key or pool
-- registration.
allocating :: DCert crypto-> Bool
allocating (RegKey _)  = True
allocating (RegPool _) = True
allocating _           = False

-- | Check for `RegKey` constructor
isRegKey :: DCert crypto-> Bool
isRegKey (RegKey _) = True
isRegKey _ = False

-- | Check for `DeRegKey` constructor
isDeRegKey :: DCert crypto-> Bool
isDeRegKey (DeRegKey _) = True
isDeRegKey _ = False

-- | Check for `RegPool` constructor
isRegPool :: DCert crypto-> Bool
isRegPool (RegPool _) = True
isRegPool _ = False

-- | Check for `RetirePool` constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (RetirePool _ _) = True
isRetirePool _ = False

decayKey :: PParams -> (Coin, UnitInterval, Rational)
decayKey pc = (dval, dmin, lambdad)
    where dval    = fromIntegral $ pc ^. keyDeposit
          dmin    = pc ^. keyMinRefund
          lambdad = pc ^. keyDecayRate

decayPool :: PParams -> (Coin, UnitInterval, Rational)
decayPool pc = (pval, pmin, lambdap)
    where pval    = fromIntegral $ pc ^. poolDeposit
          pmin    = pc ^. poolMinRefund
          lambdap = pc ^. poolDecayRate

newtype PoolDistr crypto=
  PoolDistr (Map (KeyHash crypto) (Rational, Hash (HASH crypto) (VerKeyVRF (VRF crypto))))
  deriving (Show, Eq, NoUnexpectedThunks, Relation)

isInstantaneousRewards :: DCert crypto-> Bool
isInstantaneousRewards (InstantaneousRewards _) = True
isInstantaneousRewards _                        = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling `cwitness` is safe.
requiresVKeyWitness :: DCert crypto-> Bool
requiresVKeyWitness (InstantaneousRewards _) = False
requiresVKeyWitness (RegKey _) = False
requiresVKeyWitness _ = True
