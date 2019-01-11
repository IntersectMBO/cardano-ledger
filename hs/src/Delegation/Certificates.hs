module Delegation.Certificates
  (
    DCert(..)
  , Allocs
  , authDCert
  , getRequiredSigningKey
  , dvalue
  , refund
  , certRefund
  , releasing
  , allocating
  , dretire
  , dderegister
  , decayKey
  , decayPool
  ) where

import           Coin (Coin(..))
import           Keys
import           Slot (Duration(..), Epoch(..), Slot(..), (-*))
import           PParams (PParams(..), decayRate, minRefund,
                                 keyDeposit, poolDeposit, poolMinRefund,
                                 poolDecayRate, intervalValue)

import           Delegation.StakePool

import qualified Data.Map as Map

import Lens.Micro ((^.))

type Allocs = Map.Map HashKey Slot

-- | A heavyweight certificate.
data DCert = -- | A stake key registration certificate.
            RegKey VKey
            -- | A stake key deregistration certificate.
          | DeRegKey VKey --TODO this is actually HashKey on page 13, is that what we want?
            -- | A stake pool registration certificate.
          | RegPool StakePool
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
refund :: Coin -> Rational -> Rational -> Duration -> Coin
refund (Coin d) dmin lambda delta = floor refund'
  where
    pow     = -fromRational (lambda * fromIntegral delta)
    refund' = fromIntegral d
            * (fromRational dmin + (1 - fromRational dmin) * exp pow) :: Double

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

-- | Refund for a certificate.
certRefund :: PParams -> Allocs -> Slot -> DCert -> Coin
certRefund pc allocs slot cert
    | not $ releasing cert       = Coin 0
    | hsk `Map.notMember` allocs = Coin 0
    | otherwise                  = refund dval dmin lambda (slot -* (allocs Map.! hsk))
    where hsk    = hashKey $ getRequiredSigningKey cert
          (dval, dmin, lambda) = decayKey pc

decayKey :: PParams -> (Coin, Rational, Rational)
decayKey pc = (dval, dmin, lambdad)
    where dval    = fromIntegral $ pc ^. keyDeposit
          dmin    = fromRational $ intervalValue $ pc ^. minRefund
          lambdad = pc ^. decayRate

decayPool :: PParams -> (Coin, Rational, Rational)
decayPool pc = (pval, pmin, lambdap)
    where pval    = fromIntegral $ pc ^. poolDeposit
          pmin    = fromRational $ intervalValue $ pc ^. poolMinRefund
          lambdap = pc ^. poolDecayRate
