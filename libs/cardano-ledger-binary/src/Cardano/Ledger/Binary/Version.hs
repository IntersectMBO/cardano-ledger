{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Ledger.Binary.Version
  ( -- * Versioning
    Version,
    MinVersion,
    MaxVersion,
    natVersion,
    natVersionProxy,
    mkVersion,
    allVersions,
  )
where

import Data.Proxy
import GHC.TypeLits
#if __GLASGOW_HASKELL__ < 900
-- This import is dedundant wih ghc-9.2.
import Numeric.Natural (Natural)
#endif

--------------------------------------------------------------------------------
-- Versioned Decoder
--------------------------------------------------------------------------------

-- | Protocol version number that is used during encoding and decoding. All supported
-- versions are in the range from `MinVersion` to `MaxVersion`.
newtype Version = Version Word
  deriving (Eq, Ord, Show, Enum)

-- | Minimum supported version
type MinVersion = 1

-- | Maximum supported version
type MaxVersion = 8

instance Bounded Version where
  minBound = Version (fromInteger (natVal (Proxy @MinVersion)))
  maxBound = Version (fromInteger (natVal (Proxy @MaxVersion)))

-- | Same as `natVersionProxy`, construct a version from a type level `Nat`, except it can be
-- supplied through @TypeApplications@.
natVersion :: forall v. (KnownNat v, MinVersion <= v, v <= MaxVersion) => Version
natVersion = natVersionProxy (Proxy @v)

-- | Safely construct a `Version` from a type level `Nat`, which is supplied as a `Proxy`
natVersionProxy :: (KnownNat v, MinVersion <= v, v <= MaxVersion) => Proxy v -> Version
natVersionProxy = Version . fromInteger . natVal

-- | Construct a `Version` and fail if the supplied value is not supported version number.
mkVersion :: MonadFail m => Natural -> m Version
mkVersion v
  | fromIntegral minVersion <= v && v <= fromIntegral maxVersion =
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

allVersions :: [Version]
allVersions = [minBound .. maxBound]
