{-# LANGUAGE LambdaCase #-}

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

import           Cardano.Binary (ToCBOR(toCBOR), encodeListLen)

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
import           Data.Word (Word8)

import           Lens.Micro ((^.))

newtype StakeKeys hashAlgo dsignAlgo =
  StakeKeys (Map.Map (KeyHash hashAlgo dsignAlgo) Slot)
  deriving (Show, Eq)

newtype StakePools hashAlgo dsignAlgo =
  StakePools (Map.Map (KeyHash hashAlgo dsignAlgo) Slot)
  deriving (Show, Eq)

-- | A heavyweight certificate.
data DCert hashAlgo dsignAlgo
    -- | A stake key registration certificate.
  = RegKey (VKey dsignAlgo)
    -- | A stake key deregistration certificate.
  | DeRegKey (VKey dsignAlgo) --TODO this is actually KeyHash on page 13, is that what we want?
    -- | A stake pool registration certificate.
  | RegPool (PoolParams hashAlgo dsignAlgo)
    -- | A stake pool retirement certificate.
  | RetirePool (VKey dsignAlgo) Epoch
    -- | A stake delegation certificate.
  | Delegate (Delegation dsignAlgo)
    -- | Genesis key delegation certificate
  | GenesisDelegate (VKeyGenesis dsignAlgo, VKey dsignAlgo)
  deriving (Show, Eq, Ord)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (DCert hashAlgo dsignAlgo)
 where
  toCBOR = \case
    RegKey vk ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR vk

    DeRegKey vk ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR vk

    RegPool poolParams ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR poolParams

    RetirePool vk epoch ->
      encodeListLen 3
        <> toCBOR (3 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch

    Delegate delegation ->
      encodeListLen 2
        <> toCBOR (4 :: Word8)
        <> toCBOR delegation

    GenesisDelegate keys ->
      encodeListLen 2
        <> toCBOR (5 :: Word8)
        <> toCBOR keys

-- |Determine the certificate author
cwitness
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => DCert hashAlgo dsignAlgo
  -> KeyHash hashAlgo dsignAlgo
cwitness (RegKey k)            = hashKey k
cwitness (DeRegKey k)          = hashKey k
cwitness (RegPool pool)        = hashKey $ pool ^. poolPubKey
cwitness (RetirePool k _)      = hashKey k
cwitness (Delegate delegation) = hashKey $ delegation ^. delegator
cwitness (GenesisDelegate (gk, _)) = hashGenesisKey gk

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
