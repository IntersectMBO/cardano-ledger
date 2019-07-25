

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
  ) where

import           Coin (Coin (..))
import           Keys
import           PParams (PParams (..), keyDecayRate, keyDeposit, keyMinRefund, poolDecayRate,
                     poolDeposit, poolMinRefund)
import           Slot (Duration (..))
import           TxData

import           BaseTypes
import           NonIntegral (exp')

import qualified Data.Map.Strict as Map
import           Data.Ratio (approxRational)

import           Lens.Micro ((^.))

-- |Determine the certificate author
cwitness
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => DCert hashAlgo dsignAlgo
  -> StakeCredential hashAlgo dsignAlgo
cwitness (RegKey hk)               = hk
cwitness (DeRegKey hk)             = hk
cwitness (RegPool pool)            = KeyHashObj $ hashKey $ pool ^. poolPubKey
cwitness (RetirePool k _)          = KeyHashObj k
cwitness (Delegate delegation)     = delegation ^. delegator
cwitness (GenesisDelegate (gk, _)) = KeyHashObj $ hashGenesisKey gk

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert hashAlgo dsignAlgo -> PParams -> Coin
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
releasing :: DCert hashAlgo dsignAlgo -> Bool
releasing c = dderegister c || dretire c

dderegister :: DCert hashAlgo dsignAlgo -> Bool
dderegister (DeRegKey _) = True
dderegister _            = False

dretire :: DCert hashAlgo dsignAlgo -> Bool
dretire (RetirePool _ _) = True
dretire _                = False

-- | Check whether certificate is of allocating type, i.e, key or pool
-- registration.
allocating :: DCert hashAlgo dsignAlgo -> Bool
allocating (RegKey _)  = True
allocating (RegPool _) = True
allocating _           = False

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

newtype PoolDistr hashAlgo dsignAlgo =
  PoolDistr (Map.Map (KeyHash hashAlgo dsignAlgo) Rational)
  deriving (Show, Eq)
