{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides newtype wrappers for 'IPv4' and 'IPv6' addresses
-- from the @iproute@ package. These wrappers exist to have a correct 'Show'
-- instance and eliminate the need for orphan instances.
module Cardano.Ledger.Binary.Network (
  IPv4 (..),
  IPv6 (..),
  toIPv4,
  toIPv4w,
  fromIPv4,
  toIPv6,
  toIPv6w,
  fromIPv6,
) where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.IP as IP
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Text.Read (readMaybe, readPrec)

newtype IPv4 = IPv4 {unIPv4 :: IP.IPv4}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)

instance NFData IPv4 where
  rnf = rwhnf

-- Pattern matching to force WHNF, then it's all newtypes and Word32 in there
instance NoThunks IPv4 where
  wNoThunks _ (IPv4 _) = return Nothing
  showTypeOf _ = "IPv4"

-- >>> show (toIPv4 [192, 168, 1, 1])
-- "\"192.168.1.1\""
instance Show IPv4 where
  show (IPv4 ip) = show (show ip)

-- >>> read "\"192.168.1.1\"" :: IPv4
-- "192.168.1.1"
instance Read IPv4 where
  readPrec = do
    s <- readPrec
    case readMaybe s of
      Just ip -> pure (IPv4 ip)
      Nothing -> fail "invalid IPv4"

instance FromJSON IPv4 where
  parseJSON =
    Aeson.withText "IPv4" $ \txt -> case readMaybe (Text.unpack txt) of
      Just ip -> pure (IPv4 ip)
      Nothing -> fail $ "failed to read as IPv4 " ++ show txt

instance ToJSON IPv4 where
  toJSON (IPv4 ip) = Aeson.toJSON (show ip)

newtype IPv6 = IPv6 {unIPv6 :: IP.IPv6}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)

instance NFData IPv6 where
  rnf = rwhnf

-- Pattern matching to force WHNF, then it's all newtypes and Word32 in there
instance NoThunks IPv6 where
  wNoThunks _ (IPv6 _) = return Nothing
  showTypeOf _ = "IPv6"

-- >>> show (toIPv6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 1])
-- "\"2001:db8::1\""
instance Show IPv6 where
  show (IPv6 ip) = show (show ip)

-- >>> read "\"2001:db8::1\"" :: IPv6
-- "2001:db8::1"
instance Read IPv6 where
  readPrec = do
    s <- readPrec
    case readMaybe s of
      Just ip -> pure (IPv6 ip)
      Nothing -> fail "invalid IPv6"

instance FromJSON IPv6 where
  parseJSON =
    Aeson.withText "IPv6" $ \txt -> case readMaybe (Text.unpack txt) of
      Just ip -> pure (IPv6 ip)
      Nothing -> fail $ "failed to read as IPv6 " ++ show txt

instance ToJSON IPv6 where
  toJSON (IPv6 ip) = Aeson.toJSON (show ip)

-- >>> toIPv4 [192, 168, 1, 1]
-- "192.168.1.1"
toIPv4 :: [Int] -> IPv4
toIPv4 = IPv4 . IP.toIPv4

toIPv4w :: Word32 -> IPv4
toIPv4w = IPv4 . IP.toIPv4w

fromIPv4 :: IPv4 -> [Int]
fromIPv4 = IP.fromIPv4 . unIPv4

-- >>> toIPv6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 1]
-- "2001:db8::1"
toIPv6 :: [Int] -> IPv6
toIPv6 = IPv6 . IP.toIPv6

toIPv6w :: (Word32, Word32, Word32, Word32) -> IPv6
toIPv6w = IPv6 . IP.toIPv6w

fromIPv6 :: IPv6 -> [Int]
fromIPv6 = IP.fromIPv6 . unIPv6
