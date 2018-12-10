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
import           PrtlConsts (PrtlConsts(..), decayRate, minRefund,
                                       keyDeposit, poolDeposit)

import           Delegation.StakePool

import Lens.Micro ((^.))

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
    getRequiredSigningKey (RegPool pool)        = pool ^. poolPubKey
    getRequiredSigningKey (RetirePool k _)      = k
    getRequiredSigningKey (Delegate delegation) = delegation ^. delegator

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert -> PrtlConsts -> Coin
dvalue (RegKey _)  = flip (^.) keyDeposit
dvalue (RegPool _) = flip (^.) poolDeposit
dvalue _ = const $ Coin 0

-- |Compute a refund on a deposit
refund :: DCert -> PrtlConsts -> Duration -> Coin
refund cert pc dur = floor refund'
  where
    dep = fromIntegral $ dvalue cert pc
    dmin = fromRational $ pc ^. minRefund
    pow = - fromRational (pc ^. decayRate * fromIntegral dur)
    refund' = dep * (dmin + (1-dmin) * exp pow) :: Double
