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
import           Slot (Duration (..), Epoch (..), Slot (..))

import           Delegation.PoolParams

import           BaseTypes
import           NonIntegral (exp')

import qualified Data.Map.Strict as Map
import           Data.Ratio (approxRational)

import           Lens.Micro ((^.))

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
            -- | Genesis key delegation certificate
          | GenesisDelegate (VKeyGenesis, VKey)
  deriving (Show, Eq, Ord)

-- |Determine the certificate author
cwitness :: DCert -> HashKey
cwitness (RegKey k)            = hashKey k
cwitness (DeRegKey k)          = hashKey k
cwitness (RegPool pool)        = hashKey $ pool ^. poolPubKey
cwitness (RetirePool k _)      = hashKey k
cwitness (Delegate delegation) = hashKey $ delegation ^. delegator
cwitness (GenesisDelegate (gk, _)) = hashGenesisKey gk

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
          dmin    = pc ^. keyMinRefund
          lambdad = pc ^. keyDecayRate

decayPool :: PParams -> (Coin, UnitInterval, Rational)
decayPool pc = (pval, pmin, lambdap)
    where pval    = fromIntegral $ pc ^. poolDeposit
          pmin    = pc ^. poolMinRefund
          lambdap = pc ^. poolDecayRate

newtype PoolDistr = PoolDistr (Map.Map HashKey Rational)
  deriving (Show, Eq)
