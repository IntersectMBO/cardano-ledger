{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys
  ( KeyDiscriminator(..)
  , HashAlgorithm
  , Hash
  , hash

  , DSIGNAlgorithm
  , Signable
  , SKey(..)
  , DiscVKey(..)
  , VKey
  , pattern VKey
  , VKeyGenesis
  , pattern VKeyGenesis
  , DiscKeyHash(..)
  , KeyHash
  , pattern KeyHash
  , GenKeyHash
  , pattern AnyKeyHash
  , AnyKeyHash
  , undiscriminateKeyHash
  , KeyPair(..)
  , Sig
  , hashKey
  , hashAnyKey
  , sign
  , verify

  , GKeys(..)
  , Dms(..)

  , KESAlgorithm
  , KESignable
  , SKeyES(..)
  , VKeyES(..)
  , KeyHashES(..)
  , KESig
  , hashKeyES
  , signKES
  , verifyKES
  )
where

import           Crypto.Random (drgNewSeed, seedFromInteger, withDRG)
import           Data.Maybe (fromJust)
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)

import           Data.Map.Strict (Map)
import           Data.Set (Set)

import           Cardano.Binary (ToCBOR (toCBOR))
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (SignKeyDSIGN, Signable, VerKeyDSIGN, encodeSigDSIGN, encodeVerKeyDSIGN),
                     SignedDSIGN (SignedDSIGN), signedDSIGN, verifySignedDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hash, hashWithSerialiser)
import           Cardano.Crypto.KES
                     (KESAlgorithm (SignKeyKES, VerKeyKES, encodeSigKES, encodeVerKeyKES),
                     SignedKES (SignedKES), signedKES, verifySignedKES)
import qualified Cardano.Crypto.KES as KES

-- | Discriminate between keys based on their usage in the system.
data KeyDiscriminator
  = Genesis
  | Regular
  deriving (Show)

newtype SKey dsignAlgo = SKey (SignKeyDSIGN dsignAlgo)

deriving instance DSIGNAlgorithm dsignAlgo => Show (SKey dsignAlgo)
deriving instance Num (SignKeyDSIGN dsignAlgo) => Num (SKey dsignAlgo)

-- | Discriminated verification key
newtype DiscVKey (kd :: KeyDiscriminator) dsignAlgo = DiscVKey (VerKeyDSIGN dsignAlgo)

deriving instance DSIGNAlgorithm dsignAlgo => Show (DiscVKey kd dsignAlgo)
deriving instance DSIGNAlgorithm dsignAlgo => Eq   (DiscVKey kd dsignAlgo)
deriving instance Num (VerKeyDSIGN dsignAlgo) => Num (DiscVKey kd dsignAlgo)

instance (DSIGNAlgorithm dsignAlgo, Typeable kd) => ToCBOR (DiscVKey kd dsignAlgo) where
  toCBOR (DiscVKey vk) = encodeVerKeyDSIGN vk

type VKey = DiscVKey 'Regular
pattern VKey :: VerKeyDSIGN dsignAlgo -> DiscVKey 'Regular dsignAlgo
pattern VKey a = DiscVKey a
type VKeyGenesis = DiscVKey 'Genesis
pattern VKeyGenesis :: VerKeyDSIGN dsignAlgo -> DiscVKey 'Genesis dsignAlgo
pattern VKeyGenesis a = DiscVKey a

data KeyPair (kd :: KeyDiscriminator) dsignAlgo
  = KeyPair
      { vKey :: DiscVKey kd dsignAlgo
      , sKey :: SKey dsignAlgo
      } deriving (Show)

newtype Sig dsignAlgo a = Sig (SignedDSIGN dsignAlgo a)

deriving instance (DSIGNAlgorithm dsignAlgo) => Show (Sig dsignAlgo a)
deriving instance (DSIGNAlgorithm dsignAlgo) => Eq   (Sig dsignAlgo a)

instance (DSIGNAlgorithm dsignAlgo, Typeable a) => ToCBOR (Sig dsignAlgo a) where
  toCBOR (Sig (SignedDSIGN sigDSIGN)) = encodeSigDSIGN sigDSIGN
-- |Produce a digital signature
sign
  :: (DSIGNAlgorithm dsignAlgo, Signable dsignAlgo a)
  => SKey dsignAlgo
  -> a
  -> Sig dsignAlgo a
sign (SKey k) d =
  Sig
    . fst
    . withDRG (drgNewSeed (seedFromInteger 0))
    $ signedDSIGN d k

-- |Verify a digital signature
verify
  :: (DSIGNAlgorithm dsignAlgo, Signable dsignAlgo a)
  => DiscVKey kd dsignAlgo
  -> a
  -> Sig dsignAlgo a
  -> Bool
verify (DiscVKey vk) vd (Sig sigDSIGN) =
  either (const False) (const True) $ verifySignedDSIGN vk vd sigDSIGN

newtype SKeyES kesAlgo = SKeyES (SignKeyKES kesAlgo)

deriving instance (KESAlgorithm kesAlgo) => Show (SKeyES kesAlgo)

newtype VKeyES kesAlgo = VKeyES (VerKeyKES kesAlgo)

deriving instance (KESAlgorithm kesAlgo) => Show (VKeyES kesAlgo)
deriving instance (KESAlgorithm kesAlgo) => Eq   (VKeyES kesAlgo)

instance KESAlgorithm kesAlgo => ToCBOR (VKeyES kesAlgo) where
  toCBOR (VKeyES vKeyES) = encodeVerKeyKES vKeyES

type KESignable kesAlgo a = KES.Signable kesAlgo a

newtype KESig kesAlgo a = KESig (SignedKES kesAlgo a)

deriving instance (KESAlgorithm kesAlgo) => Show (KESig kesAlgo a)
deriving instance (KESAlgorithm kesAlgo) => Eq   (KESig kesAlgo a)

instance (KESAlgorithm kesAlgo, Typeable a) => ToCBOR (KESig kesAlgo a) where
  toCBOR (KESig (SignedKES sigKES)) = encodeSigKES sigKES


-- |Produce a key evolving signature
signKES
  :: (KESAlgorithm kesAlgo, KES.Signable kesAlgo a)
  => SKeyES kesAlgo
  -> a
  -> Natural
  -> KESig kesAlgo a
signKES (SKeyES k) d n =
  KESig
    . fst
    . fromJust
    . fst
    . withDRG (drgNewSeed (seedFromInteger 0))
    $ signedKES n d k

-- |Verify a key evolving signature
verifyKES
  :: (KESAlgorithm kesAlgo, KES.Signable kesAlgo a)
  => VKeyES kesAlgo
  -> a
  -> KESig kesAlgo a
  -> Natural
  -> Bool
verifyKES (VKeyES vKeyES) vd (KESig sigKES) n =
  either (const False) (const True)
    $ verifySignedKES vKeyES n vd sigKES

newtype Dms hashAlgo dsignAlgo =
  Dms (Map (GenKeyHash hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo))
  deriving (Show, Eq)

newtype GKeys dsignAlgo = GKeys (Set (VKeyGenesis dsignAlgo))
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype DiscKeyHash (discriminator :: KeyDiscriminator) hashAlgo dsignAlgo =
  DiscKeyHash (Hash hashAlgo (VerKeyDSIGN dsignAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

type KeyHash hashAlgo dsignAlgo = DiscKeyHash 'Regular hashAlgo dsignAlgo
pattern KeyHash
  :: Hash hashAlgo (VerKeyDSIGN dsignAlgo)
  -> DiscKeyHash 'Regular hashAlgo dsignAlgo
pattern KeyHash a = DiscKeyHash a
type GenKeyHash hashAlgo dsignAlgo = DiscKeyHash 'Genesis hashAlgo dsignAlgo

-- | Discriminated hash of public Key
newtype AnyKeyHash hashAlgo dsignAlgo =
  AnyKeyHash (Hash hashAlgo (VerKeyDSIGN dsignAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

undiscriminateKeyHash
  :: DiscKeyHash kd hashAlgo dsignAlgo
  -> AnyKeyHash hashAlgo dsignAlgo
undiscriminateKeyHash (DiscKeyHash x) = AnyKeyHash x

-- |Hash a given public key
hashKey
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => DiscVKey kd dsignAlgo
  -> DiscKeyHash kd hashAlgo dsignAlgo
hashKey (DiscVKey vk) = DiscKeyHash $ hashWithSerialiser encodeVerKeyDSIGN vk

-- | Hash a given public key and forget about what it's a hash of
hashAnyKey
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => DiscVKey kd dsignAlgo
  -> AnyKeyHash hashAlgo dsignAlgo
hashAnyKey = undiscriminateKeyHash . hashKey

-- |The hash of KES verification Key
newtype KeyHashES hashAlgo kesAlgo =
  KeyHashES (Hash hashAlgo (VerKeyKES kesAlgo))
  deriving (Show, Eq, Ord, ToCBOR)


-- |Hash a given public key
hashKeyES
  :: (HashAlgorithm hashAlgo, KESAlgorithm kesAlgo)
  => VKeyES kesAlgo
  -> KeyHashES hashAlgo kesAlgo
hashKeyES (VKeyES vKeyES) =
  KeyHashES $ hashWithSerialiser encodeVerKeyKES vKeyES
