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

import Cardano.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (toExpr))
import Control.DeepSeq (NFData)
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
  deriving (Eq, Ord, Show, Enum, NFData, NoThunks, EncCBOR)

-- | Minimum supported version
type MinVersion = 1

-- | Maximum supported version. This is the protocol version of the next upcoming era
type MaxVersion = 10

instance Bounded Version where
  minBound = Version (fromInteger (natVal (Proxy @MinVersion)))
  maxBound = Version (fromInteger (natVal (Proxy @MaxVersion)))

instance DecCBOR Version where
  decCBOR = decCBOR >>= mkVersion64

-- | Same as `natVersionProxy`, construct a version from a type level `Nat`, except it can be
-- supplied through @TypeApplications@.
natVersion :: forall v. (KnownNat v, MinVersion <= v, v <= MaxVersion) => Version
natVersion = natVersionProxy (Proxy @v)

-- | Safely construct a `Version` from a type level `Nat`, which is supplied as a `Proxy`
natVersionProxy :: (KnownNat v, MinVersion <= v, v <= MaxVersion) => Proxy v -> Version
natVersionProxy = Version . fromInteger . natVal

-- | Construct a `Version` and fail if the supplied value is not a supported version number.
mkVersion :: (Integral i, MonadFail m) => i -> m Version
mkVersion v
  | vi < toInteger (minBound :: Word64) = fail $ "Decoder version is too small: " ++ show vi
  | vi > toInteger (maxBound :: Word64) = fail $ "Decoder version is too big: " ++ show vi
  | otherwise = mkVersion64 (fromIntegral v)
  where
    vi = toInteger v

-- | Construct a `Version` and fail if the supplied value is not supported version number.
mkVersion64 :: MonadFail m => Word64 -> m Version
mkVersion64 v
  | minVersion <= v && v <= maxVersion =
      pure (Version (fromIntegral v))
  | otherwise =
      fail $
        "Unsupported decoder version: "
          ++ show v
          ++ ". Expected value in bounds: ["
          ++ show minVersion
          ++ ", "
          ++ show maxVersion
          ++ "]"
  where
    Version minVersion = minBound
    Version maxVersion = maxBound

-- | Extract `Word64` representation of the `Version`
getVersion64 :: Version -> Word64
getVersion64 (Version w64) = w64

-- | Increment version by 1.
succVersion :: MonadFail m => Version -> m Version
succVersion (Version v64) = mkVersion64 (v64 + 1)

allVersions :: [Version]
allVersions = [minBound .. maxBound]

byronProtVer :: Version
byronProtVer = natVersion @1

shelleyProtVer :: Version
shelleyProtVer = natVersion @2

-- ==================================

instance ToExpr Version where
  toExpr (Version n) = App "Version" [toExpr n]
