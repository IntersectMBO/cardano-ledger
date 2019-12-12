{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , pattern GenKeyHash
  , AnyKeyHash
  , pattern AnyKeyHash
  , undiscriminateKeyHash
  , KeyPair(..)
  , Sig(..)
  , hashKey
  , hashAnyKey
  , sign
  , verify

  , GKeys(..)
  , GenDelegs(..)

  , KESAlgorithm
  , KESignable
  , SKeyES(..)
  , VKeyES(..)
  , KeyHashES(..)
  , KESig
  , hashKeyES
  , signKES
  , verifyKES

  , VRFAlgorithm(SignKeyVRF, VerKeyVRF)
  , VRFValue(..)
  , VRF.CertifiedVRF(..)
  , hashKeyVRF
  )
where

import           Cardano.Ledger.Shelley.Crypto
import           Crypto.Random (drgNewSeed, seedFromInteger, withDRG)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromJust)
import           Data.Ratio ((%))
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           BaseTypes (Nonce, UnitInterval, mkNonce, truncateUnitInterval)
import           Cardano.Binary (FromCBOR (..), ToCBOR (toCBOR))
import           Cardano.Crypto.DSIGN
                     (DSIGNAlgorithm (SignKeyDSIGN, Signable, VerKeyDSIGN, encodeVerKeyDSIGN),
                     SignedDSIGN (SignedDSIGN), signedDSIGN, verifySignedDSIGN)
import qualified Cardano.Crypto.DSIGN as DSIGN
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hash, hashWithSerialiser)
import           Cardano.Crypto.KES
                     (KESAlgorithm (SignKeyKES, VerKeyKES, encodeSigKES, encodeVerKeyKES),
                     SignedKES (SignedKES), decodeSignedKES, signedKES, verifySignedKES)
import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF (VRFAlgorithm (VerKeyVRF))
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Prelude (NoUnexpectedThunks (..))

-- | Discriminate between keys based on their usage in the system.
data KeyDiscriminator
  = Genesis
  | Regular
  deriving (Show)

newtype SKey crypto = SKey (SignKeyDSIGN (DSIGN crypto))

deriving instance Crypto crypto => NoUnexpectedThunks (SKey crypto)

deriving instance Crypto crypto => Show (SKey crypto)
deriving instance Num (SignKeyDSIGN (DSIGN crypto)) => Num (SKey crypto)

-- | Discriminated verification key
newtype DiscVKey (kd :: KeyDiscriminator) crypto = DiscVKey (VerKeyDSIGN (DSIGN crypto))

deriving instance Crypto crypto => Show (DiscVKey kd crypto)
deriving instance Crypto crypto => Eq   (DiscVKey kd crypto)
deriving instance Num (VerKeyDSIGN (DSIGN crypto)) => Num (DiscVKey kd crypto)
deriving instance Crypto crypto => NoUnexpectedThunks (DiscVKey kd crypto)

instance (Crypto crypto, Typeable kd) => FromCBOR (DiscVKey kd crypto) where
  fromCBOR = DiscVKey <$> DSIGN.decodeVerKeyDSIGN
instance (Crypto crypto, Typeable kd) => ToCBOR (DiscVKey kd crypto) where
  toCBOR (DiscVKey vk) = encodeVerKeyDSIGN vk

type VKey = DiscVKey 'Regular
pattern VKey :: VerKeyDSIGN (DSIGN crypto) -> DiscVKey 'Regular crypto
pattern VKey a = DiscVKey a
type VKeyGenesis = DiscVKey 'Genesis
pattern VKeyGenesis :: VerKeyDSIGN (DSIGN crypto) -> DiscVKey 'Genesis crypto
pattern VKeyGenesis a = DiscVKey a

data KeyPair (kd :: KeyDiscriminator) crypto
  = KeyPair
      { vKey :: DiscVKey kd crypto
      , sKey :: SKey crypto
      } deriving (Generic, Show)

instance Crypto crypto => NoUnexpectedThunks (KeyPair kd crypto)

newtype Sig crypto a = UnsafeSig (SignedDSIGN (DSIGN crypto) a)

deriving instance (Crypto crypto) => Show (Sig crypto a)
deriving instance (Crypto crypto) => Eq   (Sig crypto a)
deriving instance Crypto crypto => NoUnexpectedThunks (Sig crypto a)

instance (Crypto crypto, Typeable a) => FromCBOR (Sig crypto a) where
  fromCBOR = UnsafeSig <$> DSIGN.decodeSignedDSIGN
instance (Crypto crypto, Typeable a) => ToCBOR (Sig crypto a) where
  toCBOR (UnsafeSig s) = DSIGN.encodeSignedDSIGN s

-- |Produce a digital signature
sign
  :: (Crypto crypto, Signable (DSIGN crypto) a)
  => SKey crypto
  -> a
  -> Sig crypto a
sign (SKey k) d =
  UnsafeSig
    . fst
    . withDRG (drgNewSeed (seedFromInteger 0))
    $ signedDSIGN () d k

-- |Verify a digital signature
verify
  :: (Crypto crypto, Signable (DSIGN crypto) a)
  => DiscVKey kd crypto
  -> a
  -> Sig crypto a
  -> Bool
verify (DiscVKey vk) vd (UnsafeSig sigDSIGN) =
  either (const False) (const True) $ verifySignedDSIGN () vk vd sigDSIGN

newtype SKeyES crypto = SKeyES (SignKeyKES (KES crypto))

deriving instance (KESAlgorithm (KES crypto)) => Show (SKeyES crypto)

newtype VKeyES crypto = VKeyES (VerKeyKES (KES crypto))

deriving instance (KESAlgorithm (KES crypto)) => Show (VKeyES crypto)
deriving instance (KESAlgorithm (KES crypto)) => Eq   (VKeyES crypto)
deriving instance (KESAlgorithm (KES crypto)) => NoUnexpectedThunks (VKeyES crypto)

instance Crypto crypto => ToCBOR (VKeyES crypto) where
  toCBOR (VKeyES vKeyES) = encodeVerKeyKES vKeyES
instance Crypto crypto => FromCBOR (VKeyES crypto) where
  fromCBOR = VKeyES <$> KES.decodeVerKeyKES

type KESignable crypto a = KES.Signable (KES crypto) a

newtype KESig crypto a = KESig (SignedKES (KES crypto) a)

deriving instance (Crypto crypto) => Show (KESig crypto a)
deriving instance (Crypto crypto) => Eq   (KESig crypto a)
deriving instance (Crypto crypto) => NoUnexpectedThunks (KESig crypto a)

instance (Crypto crypto, Typeable a) => ToCBOR (KESig crypto a) where
  toCBOR (KESig (SignedKES sigKES)) = encodeSigKES sigKES
instance (Crypto crypto, Typeable a) => FromCBOR (KESig crypto a) where
  fromCBOR = KESig <$> decodeSignedKES


-- |Produce a key evolving signature
signKES
  :: (Crypto crypto, KES.Signable (KES crypto) a)
  => SKeyES crypto
  -> a
  -> Natural
  -> KESig crypto a
signKES (SKeyES k) d n =
  KESig
    . fst
    . fromJust
    . fst
    . withDRG (drgNewSeed (seedFromInteger 0))
    $ signedKES () n d k

-- |Verify a key evolving signature
verifyKES
  :: (Crypto crypto, KES.Signable (KES crypto) a)
  => VKeyES crypto
  -> a
  -> KESig crypto a
  -> Natural
  -> Bool
verifyKES (VKeyES vKeyES) vd (KESig sigKES) n =
  either (const False) (const True)
    $ verifySignedKES () vKeyES n vd sigKES

newtype GenDelegs crypto =
  GenDelegs (Map (GenKeyHash crypto) (KeyHash crypto))
  deriving (Show, Eq, NoUnexpectedThunks)

newtype GKeys crypto = GKeys (Set (VKeyGenesis crypto))
  deriving (Show, Eq, NoUnexpectedThunks)

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype DiscKeyHash (discriminator :: KeyDiscriminator) crypto =
  DiscKeyHash (Hash (HASH crypto) (VerKeyDSIGN (DSIGN crypto)))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance (Crypto crypto, Typeable disc) => ToCBOR (DiscKeyHash disc crypto)
deriving instance (Crypto crypto, Typeable disc) => FromCBOR (DiscKeyHash disc crypto)

type KeyHash crypto = DiscKeyHash 'Regular crypto
{-# COMPLETE KeyHash #-}
pattern KeyHash
  :: Hash (HASH crypto) (VerKeyDSIGN (DSIGN crypto))
  -> DiscKeyHash 'Regular crypto
pattern KeyHash a = DiscKeyHash a
type GenKeyHash crypto = DiscKeyHash 'Genesis crypto
{-# COMPLETE GenKeyHash #-}
pattern GenKeyHash
  :: Hash (HASH crypto) (VerKeyDSIGN (DSIGN crypto))
  -> DiscKeyHash 'Genesis crypto
pattern GenKeyHash a = DiscKeyHash a

-- | Discriminated hash of public Key
newtype AnyKeyHash crypto =
  AnyKeyHash (Hash (HASH crypto) (VerKeyDSIGN (DSIGN crypto)))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (AnyKeyHash crypto)

undiscriminateKeyHash
  :: DiscKeyHash kd crypto
  -> AnyKeyHash crypto
undiscriminateKeyHash (DiscKeyHash x) = AnyKeyHash x

-- |Hash a given public key
hashKey
  :: Crypto crypto
  => DiscVKey kd crypto
  -> DiscKeyHash kd crypto
hashKey (DiscVKey vk) = DiscKeyHash $ hashWithSerialiser encodeVerKeyDSIGN vk

-- | Hash a given public key and forget about what it's a hash of
hashAnyKey
  :: Crypto crypto
  => DiscVKey kd crypto
  -> AnyKeyHash crypto
hashAnyKey = undiscriminateKeyHash . hashKey

-- |The hash of KES verification Key
newtype KeyHashES crypto =
  KeyHashES (Hash (HASH crypto) (VerKeyKES (KES crypto)))
  deriving (Show, Eq, Ord)

deriving instance Crypto crypto => ToCBOR (KeyHashES crypto)

-- |Hash a given public key
hashKeyES
  :: Crypto crypto
  => VKeyES crypto
  -> KeyHashES crypto
hashKeyES (VKeyES vKeyES) =
  KeyHashES $ hashWithSerialiser encodeVerKeyKES vKeyES

-- | Our VRFAlgorithm provides a 'Natural', so we must exhibit an extractor to
--   use the bits as some other type
class VRFValue a where
  -- | Extract a value from a natural number derived from the VRF computation.
  fromNatural :: Natural -> a

instance VRFValue Nonce where
  fromNatural = mkNonce

instance VRFValue UnitInterval where
  -- TODO Consider whether this is a reasonable thing to do
  fromNatural k = truncateUnitInterval $ toInteger (k `mod` 10000) % 10000

hashKeyVRF
  :: Crypto crypto
  => VRF.VerKeyVRF (VRF crypto)
  -> Hash (HASH crypto) (VerKeyVRF (VRF crypto))
hashKeyVRF = hashWithSerialiser VRF.encodeVerKeyVRF
