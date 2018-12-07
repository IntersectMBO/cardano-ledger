module Delegation.Certificates
  (
    DCert(..)
  , authDCert
  , dvalue
  , refund
  ) where

import           Coin (Coin(..))
import           Keys
import           Slot (Duration(..), Epoch(..))
import           PrtlConsts (PrtlConsts(..))

import           Delegation.StakePool

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

-- |Determine if a certificate is authorized by the given key.
authDCert :: VKey -> DCert -> Bool
authDCert key cert = getRequiredSigningKey cert == key
  where
    getRequiredSigningKey (RegKey k)            = k
    getRequiredSigningKey (DeRegKey k)          = k
    getRequiredSigningKey (RegPool pool)        = _poolPubKey pool
    getRequiredSigningKey (RetirePool k _)      = k
    getRequiredSigningKey (Delegate delegation) = _delegator delegation

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert -> PrtlConsts -> Coin
dvalue (RegKey _) = _keyDeposit
dvalue (RegPool _) = _poolDeposit
dvalue _ = const $ Coin 0

-- |Compute a refund on a deposit
refund :: DCert -> PrtlConsts -> Duration -> Coin
refund cert pc dur = floor refund'
  where
    dep = fromIntegral $ dvalue cert pc
    dmin = fromRational $ _minRefund pc
    pow = - fromRational (_decayRate pc * fromIntegral dur)
    refund' = dep * (dmin + (1-dmin) * exp pow) :: Double
