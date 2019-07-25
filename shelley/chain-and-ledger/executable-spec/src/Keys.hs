{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys
  ( HashAlgorithm
  , Hash
  , hash

  , DSIGNAlgorithm
  , Signable
  , SKey(..)
  , VKey(..)
  , KeyHash(..)
  , KeyPair(..)
  , VKeyGenesis(..)
  , Sig
  , hashKey
  , hashGenesisKey
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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Binary (ToCBOR (toCBOR))
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (SignKeyDSIGN, Signable, VerKeyDSIGN, encodeSigDSIGN, encodeVerKeyDSIGN),
                     SignedDSIGN (SignedDSIGN), signedDSIGN, verifySignedDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hash, hashWithSerialiser)
import           Cardano.Crypto.KES
                     (KESAlgorithm (SignKeyKES, VerKeyKES, encodeSigKES, encodeVerKeyKES),
                     SignedKES (SignedKES), signedKES, verifySignedKES)
import qualified Cardano.Crypto.KES as KES

newtype SKey dsignAlgo = SKey (SignKeyDSIGN dsignAlgo)

deriving instance DSIGNAlgorithm dsignAlgo => Show (SKey dsignAlgo)
deriving instance DSIGNAlgorithm dsignAlgo => Eq   (SKey dsignAlgo)
deriving instance DSIGNAlgorithm dsignAlgo => Ord  (SKey dsignAlgo)
deriving instance Num (SignKeyDSIGN dsignAlgo) => Num (SKey dsignAlgo)

newtype VKey dsignAlgo = VKey (VerKeyDSIGN dsignAlgo)

deriving instance DSIGNAlgorithm dsignAlgo => Show (VKey dsignAlgo)
deriving instance DSIGNAlgorithm dsignAlgo => Eq   (VKey dsignAlgo)
deriving instance DSIGNAlgorithm dsignAlgo => Ord  (VKey dsignAlgo)
deriving instance Num (VerKeyDSIGN dsignAlgo) => Num (VKey dsignAlgo)

instance DSIGNAlgorithm dsignAlgo => ToCBOR (VKey dsignAlgo) where
  toCBOR (VKey vk) = encodeVerKeyDSIGN vk


data KeyPair dsignAlgo
  = KeyPair
      { vKey :: VKey dsignAlgo
      , sKey :: SKey dsignAlgo
      } deriving (Show, Eq, Ord)


newtype VKeyGenesis dsignAlgo = VKeyGenesis (VerKeyDSIGN dsignAlgo)

deriving instance (DSIGNAlgorithm dsignAlgo) => Show (VKeyGenesis dsignAlgo)
deriving instance (DSIGNAlgorithm dsignAlgo) => Eq   (VKeyGenesis dsignAlgo)
deriving instance (DSIGNAlgorithm dsignAlgo) => Ord  (VKeyGenesis dsignAlgo)

instance DSIGNAlgorithm dsignAlgo => ToCBOR (VKeyGenesis dsignAlgo) where
  toCBOR (VKeyGenesis vKeyGenesis) = encodeVerKeyDSIGN vKeyGenesis


newtype Sig dsignAlgo a = Sig (SignedDSIGN dsignAlgo a)

deriving instance (DSIGNAlgorithm dsignAlgo) => Show (Sig dsignAlgo a)
deriving instance (DSIGNAlgorithm dsignAlgo) => Eq   (Sig dsignAlgo a)
deriving instance (DSIGNAlgorithm dsignAlgo) => Ord  (Sig dsignAlgo a)

instance (DSIGNAlgorithm dsignAlgo, Typeable a) => ToCBOR (Sig dsignAlgo a) where
  toCBOR (Sig (SignedDSIGN sigDSIGN)) = encodeSigDSIGN sigDSIGN


-- |The hash of public Key
newtype KeyHash hashAlgo dsignAlgo =
  KeyHash (Hash hashAlgo (VerKeyDSIGN dsignAlgo))
  deriving (Show, Eq, Ord, ToCBOR)


-- |Hash a given public key
hashKey
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => VKey dsignAlgo
  -> KeyHash hashAlgo dsignAlgo
hashKey (VKey vk) = KeyHash $ hashWithSerialiser encodeVerKeyDSIGN vk

-- |Produce a digital signature
sign
  :: (DSIGNAlgorithm dsignAlgo, Signable dsignAlgo a, ToCBOR a)
  => SKey dsignAlgo
  -> a
  -> Sig dsignAlgo a
sign (SKey k) d =
  Sig
    . fst
    . withDRG (drgNewSeed (seedFromInteger 0))
    $ signedDSIGN toCBOR d k

-- |Verify a digital signature
verify
  :: (DSIGNAlgorithm dsignAlgo, Signable dsignAlgo a, ToCBOR a)
  => VKey dsignAlgo
  -> a
  -> Sig dsignAlgo a
  -> Bool
verify (VKey vk) vd (Sig sigDSIGN) =
  either (const False) (const True) $ verifySignedDSIGN toCBOR vk vd sigDSIGN


newtype SKeyES kesAlgo = SKeyES (SignKeyKES kesAlgo)

deriving instance (KESAlgorithm kesAlgo) => Show (SKeyES kesAlgo)
deriving instance (KESAlgorithm kesAlgo) => Eq   (SKeyES kesAlgo)
deriving instance (KESAlgorithm kesAlgo) => Ord  (SKeyES kesAlgo)


newtype VKeyES kesAlgo = VKeyES (VerKeyKES kesAlgo)

deriving instance (KESAlgorithm kesAlgo) => Show (VKeyES kesAlgo)
deriving instance (KESAlgorithm kesAlgo) => Eq   (VKeyES kesAlgo)
deriving instance (KESAlgorithm kesAlgo) => Ord  (VKeyES kesAlgo)

instance KESAlgorithm kesAlgo => ToCBOR (VKeyES kesAlgo) where
  toCBOR (VKeyES vKeyES) = encodeVerKeyKES vKeyES


type KESignable kesAlgo a = KES.Signable kesAlgo a

newtype KESig kesAlgo a = KESig (SignedKES kesAlgo a)

deriving instance (KESAlgorithm kesAlgo) => Show (KESig kesAlgo a)
deriving instance (KESAlgorithm kesAlgo) => Eq   (KESig kesAlgo a)
deriving instance (KESAlgorithm kesAlgo) => Ord  (KESig kesAlgo a)

instance (KESAlgorithm kesAlgo, Typeable a) => ToCBOR (KESig kesAlgo a) where
  toCBOR (KESig (SignedKES sigKES)) = encodeSigKES sigKES


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

-- |Produce a key evolving signature
signKES
  :: (KESAlgorithm kesAlgo, KES.Signable kesAlgo a, ToCBOR a)
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
    $ signedKES toCBOR n d k

-- |Verify a key evolving signature
verifyKES
  :: (KESAlgorithm kesAlgo, KES.Signable kesAlgo a, ToCBOR a)
  => VKeyES kesAlgo
  -> a
  -> KESig kesAlgo a
  -> Natural
  -> Bool
verifyKES (VKeyES vKeyES) vd (KESig sigKES) n =
  either (const False) (const True)
    $ verifySignedKES toCBOR vKeyES n vd sigKES

newtype Dms dsignAlgo =
  Dms (Map.Map (VKeyGenesis dsignAlgo) (VKey dsignAlgo))
  deriving (Show, Ord, Eq)

newtype GKeys dsignAlgo = GKeys (Set.Set (VKeyGenesis dsignAlgo))
  deriving (Show, Ord, Eq)

hashGenesisKey
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => VKeyGenesis dsignAlgo
  -> KeyHash hashAlgo dsignAlgo
hashGenesisKey (VKeyGenesis vKeyGenesis) =
  KeyHash $ hashWithSerialiser encodeVerKeyDSIGN vKeyGenesis
