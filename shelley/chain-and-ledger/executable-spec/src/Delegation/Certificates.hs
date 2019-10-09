{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Delegation.Certificates
  (
    DCert(..)
  , StakeKeys(..)
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
  ) where

import           Coin (Coin (..))
import           Keys (Hash, KeyHash, VRFAlgorithm (VerKeyVRF))
import           PParams (PParams (..), keyDecayRate, keyDeposit, keyMinRefund, poolDecayRate,
                     poolDeposit, poolMinRefund)
import           Slot (Duration (..))
import           TxData (Credential (..), DCert (..), StakeCredential, StakeKeys (..),
                     StakePools (..), delegator, poolPubKey)

import           BaseTypes (FixedPoint, UnitInterval, fpEpsilon, intervalValue)
import           NonIntegral (exp')

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Map.Strict (Map)
import           Data.Ratio (approxRational)

import           Lens.Micro ((^.))

-- |Determine the certificate author
cwitness :: DCert hashAlgo dsignAlgo vrfAlgo -> StakeCredential hashAlgo dsignAlgo
cwitness (RegKey hk)               = hk
cwitness (DeRegKey hk)             = hk
cwitness (RegPool pool)            = KeyHashObj $ pool ^. poolPubKey
cwitness (RetirePool k _)          = KeyHashObj k
cwitness (Delegate delegation)     = delegation ^. delegator
cwitness (GenesisDelegate (gk, _)) = GenesisHashObj gk

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert hashAlgo dsignAlgo vrfAlgo -> PParams -> Coin
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
releasing :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
releasing c = dderegister c || dretire c

dderegister :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
dderegister (DeRegKey _) = True
dderegister _            = False

dretire :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
dretire (RetirePool _ _) = True
dretire _                = False

-- | Check whether certificate is of allocating type, i.e, key or pool
-- registration.
allocating :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
allocating (RegKey _)  = True
allocating (RegPool _) = True
allocating _           = False

-- | Check for `RegKey` constructor
isRegKey :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
isRegKey (RegKey _) = True
isRegKey _ = False

-- | Check for `DeRegKey` constructor
isDeRegKey :: DCert hashAlgo dsignAlgo vrfAlgo -> Bool
isDeRegKey (DeRegKey _) = True
isDeRegKey _ = False

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

newtype PoolDistr hashAlgo dsignAlgo vrfAlgo =
  PoolDistr (Map (KeyHash hashAlgo dsignAlgo) (Rational, Hash hashAlgo (VerKeyVRF vrfAlgo)))
  deriving (Show, Eq, NoUnexpectedThunks)
