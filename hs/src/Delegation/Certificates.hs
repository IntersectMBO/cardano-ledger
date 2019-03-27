module Delegation.Certificates
  (
    DCert(..)
  , StakeKeys(..)
  , StakePools(..)
  , PoolDistr(..)
  , authDCert
  , getRequiredSigningKey
  , dvalue
  , refund
  , releasing
  , allocating
  , dretire
  , dderegister
  , decayKey
  , decayPool
  ) where

import           Coin (Coin(..))
import           Keys
import           Slot (Duration(..), Epoch(..), Slot(..))
import           PParams (PParams(..), decayRate, minRefund,
                                 keyDeposit, poolDeposit, poolMinRefund,
                                 poolDecayRate)

import           Delegation.PoolParams

import           NonIntegral (exp')
import           BaseTypes

import qualified Data.Map.Strict as Map
import           Data.Ratio (approxRational)

import Lens.Micro ((^.))

newtype StakeKeys  = StakeKeys  (Map.Map HashKey Slot)
    deriving (Show, Eq)
newtype StakePools = StakePools (Map.Map HashKey Slot)
    deriving (Show, Eq)

-- | A heavyweight certificate.
data DCert = -- | A stake key registration certificate.
            RegKey VKey
            -- | A stake key deregistration certificate.
          | DeRegKey VKey --TODO this is actually HashKey on page 13, is that what we want?
            -- | A stake pool registration certificate.
          | RegPool PoolParams
            -- | A stake pool retirement certificate.
          | RetirePool VKey Epoch
            -- | A stake delegation certificate.
          | Delegate Delegation
  deriving (Show, Eq, Ord)

-- |Determine the certificate author
getRequiredSigningKey :: DCert -> VKey
getRequiredSigningKey (RegKey k)            = k
getRequiredSigningKey (DeRegKey k)          = k
getRequiredSigningKey (RegPool pool)        = pool ^. poolPubKey
getRequiredSigningKey (RetirePool k _)      = k
getRequiredSigningKey (Delegate delegation) = delegation ^. delegator

-- |Determine if a certificate is authorized by the given key.
authDCert :: VKey -> DCert -> Bool
authDCert key cert = getRequiredSigningKey cert == key

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert -> PParams -> Coin
dvalue (RegKey _)  = flip (^.) keyDeposit
dvalue (RegPool _) = flip (^.) poolDeposit
dvalue _ = const $ Coin 0

-- |Compute a refund on a deposit
refund :: Coin -> UnitInterval -> Rational -> Duration -> Coin
refund (Coin dval) dmin lambda delta = floor refund'
  where
    pow     = fromRational (-lambda * fromIntegral delta) :: FixedPoint
    refund' = fromIntegral dval * (dmin' + (1 - dmin')) * dCay
    dmin'   = intervalValue dmin
    dCay    = approxRational (exp' pow) fpEpsilon

-- | Check whether certificate is of releasing type, i.e., key deregistration or
-- pool retirement.
releasing :: DCert -> Bool
releasing c = dderegister c || dretire c

dderegister :: DCert -> Bool
dderegister (DeRegKey _) = True
dderegister _            = False

dretire :: DCert -> Bool
dretire (RetirePool _ _) = True
dretire _                = False

-- | Check whether certificate is of allocating type, i.e, key or pool
-- registration.
allocating :: DCert -> Bool
allocating (RegKey _)  = True
allocating (RegPool _) = True
allocating _           = False

decayKey :: PParams -> (Coin, UnitInterval, Rational)
decayKey pc = (dval, dmin, lambdad)
    where dval    = fromIntegral $ pc ^. keyDeposit
          dmin    = pc ^. minRefund
          lambdad = pc ^. decayRate

decayPool :: PParams -> (Coin, UnitInterval, Rational)
decayPool pc = (pval, pmin, lambdap)
    where pval    = fromIntegral $ pc ^. poolDeposit
          pmin    = pc ^. poolMinRefund
          lambdap = pc ^. poolDecayRate

newtype PoolDistr = PoolDistr (Map.Map HashKey Coin)
  deriving (Show, Eq)
