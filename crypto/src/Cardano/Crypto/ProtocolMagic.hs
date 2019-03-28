{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Crypto.ProtocolMagic
  ( ProtocolMagicId(..)
  , ProtocolMagic(..)
  , RequiresNetworkMagic(..)
  , getProtocolMagic
  )
where

import Cardano.Prelude

import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import Text.JSON.Canonical (FromJSON(..), JSValue(..), ToJSON(..), expected)

import Cardano.Binary.Class (Bi)
-- | Magic number which should differ for different clusters. It's
--   defined here, because it's used for signing. It also used for other
--   things (e. g. it's part of a serialized block).
--
-- mhueschen: As part of CO-353 I am adding `getRequiresNetworkMagic` in
-- order to pipe configuration to functions which must generate & verify
-- Addresses (which now must be aware of `NetworkMagic`).
data ProtocolMagic = ProtocolMagic
  { getProtocolMagicId      :: !ProtocolMagicId
  , getRequiresNetworkMagic :: !RequiresNetworkMagic
  } deriving (Eq, Show, Generic, NFData)

newtype ProtocolMagicId = ProtocolMagicId
  { unProtocolMagicId :: Int32
  } deriving (Show, Eq, Generic)
    deriving newtype Bi
    deriving anyclass NFData

instance A.ToJSON ProtocolMagicId where
  toJSON = A.toJSON . unProtocolMagicId

instance A.FromJSON ProtocolMagicId where
  parseJSON v = ProtocolMagicId <$> A.parseJSON v

-- mhueschen: For backwards-compatibility reasons, I redefine this function
-- in terms of the two record accessors.
getProtocolMagic :: ProtocolMagic -> Int32
getProtocolMagic = unProtocolMagicId . getProtocolMagicId

instance A.ToJSON ProtocolMagic where
  toJSON (ProtocolMagic (ProtocolMagicId ident) rnm) =
    A.object ["pm" .= ident, "requiresNetworkMagic" .= rnm]

instance A.FromJSON ProtocolMagic where
  parseJSON = A.withObject "ProtocolMagic" $ \o ->
    ProtocolMagic
      <$> o .: "pm"
      <*> o .: "requiresNetworkMagic"

-- Canonical JSON instances
instance Monad m => ToJSON m ProtocolMagicId where
  toJSON (ProtocolMagicId ident) = toJSON ident

instance MonadError SchemaError m => FromJSON m ProtocolMagicId where
  fromJSON v = ProtocolMagicId <$> fromJSON v


--------------------------------------------------------------------------------
-- RequiresNetworkMagic
--------------------------------------------------------------------------------

-- | Bool-isomorphic flag indicating whether we're on testnet
-- or mainnet/staging.
data RequiresNetworkMagic
  = RequiresNoMagic
  | RequiresMagic
  deriving (Show, Eq, Generic, NFData)

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
  toJSON RequiresNoMagic = A.String "RequiresNoMagic"
  toJSON RequiresMagic   = A.String "RequiresMagic"

instance A.FromJSON RequiresNetworkMagic where
  parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
    "RequiresNoMagic" -> Right RequiresNoMagic
    "RequiresMagic"   -> Right RequiresMagic
    "NMMustBeNothing" -> Right RequiresNoMagic
    "NMMustBeJust"    -> Right RequiresMagic
    other   -> Left ("invalid value " <> other <>
                     ", acceptable values are RequiresNoMagic | RequiresMagic")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
  toJSON RequiresNoMagic = pure (JSString "RequiresNoMagic")
  toJSON RequiresMagic   = pure (JSString "RequiresMagic")

instance MonadError SchemaError m => FromJSON m RequiresNetworkMagic where
  fromJSON = \case
    JSString "RequiresNoMagic" -> pure RequiresNoMagic
    JSString "RequiresMagic"    -> pure RequiresMagic
    other ->
      expected "RequiresNoMagic | RequiresMagic" (Just $ show other)
