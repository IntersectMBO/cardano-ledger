{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Ledger.Binary.Version (
  -- * Versioning
  Version,
  getVersion,
  MinVersion,
  MaxVersion,
  natVersion,
  natVersionProxy,
  succVersion,
  mkVersion,
  mkVersion64,
  getVersion64,
  allVersions,

  -- ** Concrete era versions
  byronProtVer,
  shelleyProtVer,
)
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownNat, natVal, type (<=))
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | Protocol version number that is used during encoding and decoding. All supported
-- versions are in the range from `MinVersion` to `MaxVersion`.
newtype Version = Version Word64
  deriving (Eq, Ord, Show, NFData, NoThunks, ToCBOR, ToJSON)

-- | Minimum supported version
type MinVersion = 0

-- | Maximum supported version. This is the major protocol version of the latest known
-- protocol version that we want to support, including for development and testing.
type MaxVersion = 11

instance Enum Version where
  toEnum = errorFail . mkVersion
  fromEnum (Version v) = fromEnum v

instance Bounded Version where
  minBound = Version (fromInteger (natVal (Proxy @MinVersion)))
  maxBound = Version (fromInteger (natVal (Proxy @MaxVersion)))

instance FromCBOR Version where
  fromCBOR = fromCBOR >>= mkVersion64
  {-# INLINE fromCBOR #-}

instance FromJSON Version where
  parseJSON v = parseJSON v >>= mkVersion64

-- | Same as `natVersionProxy`, construct a version from a type level `Nat`, except it can be
-- supplied through @TypeApplications@.
natVersion :: forall v. (KnownNat v, MinVersion <= v, v <= MaxVersion) => Version
natVersion = natVersionProxy (Proxy @v)
{-# INLINE natVersion #-}

-- | Safely construct a `Version` from a type level `Nat`, which is supplied as a `Proxy`
natVersionProxy :: (KnownNat v, MinVersion <= v, v <= MaxVersion) => Proxy v -> Version
natVersionProxy = Version . fromInteger . natVal
{-# INLINE natVersionProxy #-}

-- | Construct a `Version` and fail if the supplied value is not a supported version number.
mkVersion :: (Integral i, MonadFail m) => i -> m Version
mkVersion v
  | vi < toInteger (minBound :: Word64) = fail $ "Version is too small: " ++ show vi
  | vi > toInteger (maxBound :: Word64) = fail $ "Version is too big: " ++ show vi
  | otherwise = mkVersion64 (fromIntegral v)
  where
    vi = toInteger v
{-# INLINE mkVersion #-}

-- | Construct a `Version` and fail if the supplied value is not supported version number.
mkVersion64 :: MonadFail m => Word64 -> m Version
mkVersion64 v
  | minVersion <= v && v <= maxVersion =
      pure (Version (fromIntegral v))
  | otherwise =
      fail $
        "Unsupported version value: "
          ++ show v
          ++ ". Expected value in bounds: ["
          ++ show minVersion
          ++ ", "
          ++ show maxVersion
          ++ "]"
  where
    Version minVersion = minBound
    Version maxVersion = maxBound
{-# INLINE mkVersion64 #-}

-- | Convert a `Version` to an `Integral` value.
--
-- /Note/ - Version spans a fairly small range of non-negative numbers, so this should be
-- safe even for smallest integral types.
getVersion :: Integral i => Version -> i
getVersion (Version w64) = fromIntegral w64
{-# INLINE getVersion #-}

-- | Extract `Word64` representation of the `Version`
getVersion64 :: Version -> Word64
getVersion64 (Version w64) = w64
{-# INLINE getVersion64 #-}

-- | Increment version by 1.
succVersion :: MonadFail m => Version -> m Version
succVersion (Version v64) = mkVersion64 (v64 + 1)
{-# INLINE succVersion #-}

allVersions :: [Version]
allVersions = [minBound .. maxBound]

byronProtVer :: Version
byronProtVer = natVersion @1
{-# INLINE byronProtVer #-}

shelleyProtVer :: Version
shelleyProtVer = natVersion @2
{-# INLINE shelleyProtVer #-}
