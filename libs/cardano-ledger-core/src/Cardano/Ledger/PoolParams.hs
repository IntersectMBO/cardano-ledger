{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.PoolParams
  ( PoolParams (..),
    PoolMetadata (..),
    StakePoolRelay (..),
    SizeOfPoolRelays (..),
    SizeOfPoolOwners (..),
  )
where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes
  ( DnsName,
    Port,
    StrictMaybe (..),
    UnitInterval,
    Url,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Binary
  ( CBORGroup (..),
    Case (..),
    FromCBOR (fromCBOR),
    FromCBORGroup (..),
    Size,
    ToCBOR (..),
    ToCBORGroup (..),
    decodeNullMaybe,
    decodeRecordNamed,
    decodeRecordSum,
    encodeListLen,
    encodeNullMaybe,
    szCases,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    VerKeyVRF,
  )
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData ())
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (asum)
import Data.IP (IPv4, IPv6)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- ========================================================================

data PoolMetadata = PoolMetadata
  { pmUrl :: !Url,
    pmHash :: !ByteString
  }
  deriving (Eq, Ord, Generic, Show)

deriving instance NFData PoolMetadata

instance ToJSON PoolMetadata where
  toJSON pmd =
    Aeson.object
      [ "url" .= pmUrl pmd,
        "hash" .= (Text.decodeLatin1 . B16.encode) (pmHash pmd)
      ]

instance FromJSON PoolMetadata where
  parseJSON =
    Aeson.withObject "PoolMetadata" $ \obj -> do
      url <- obj .: "url"
      hash <- explicitParseField parseJsonBase16 obj "hash"
      return $ PoolMetadata url hash

parseJsonBase16 :: Value -> Parser ByteString
parseJsonBase16 v = do
  s <- parseJSON v
  case B16.decode (Char8.pack s) of
    Right bs -> return bs
    Left msg -> fail msg

instance NoThunks PoolMetadata

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    SingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
  | -- | An @A@ or @AAAA@ DNS record
    SingleHostName !(StrictMaybe Port) !DnsName
  | -- | A @SRV@ DNS record
    MultiHostName !DnsName
  deriving (Eq, Ord, Generic, Show)

instance FromJSON StakePoolRelay where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum
        [ explicitParseField parser1 obj "single host address",
          explicitParseField parser2 obj "single host name",
          explicitParseField parser3 obj "multi host name"
        ]
    where
      parser1 = Aeson.withObject "SingleHostAddr" $ \obj ->
        SingleHostAddr
          <$> obj .:? "port" .!= SNothing
          <*> obj .:? "IPv4" .!= SNothing
          <*> obj .:? "IPv6" .!= SNothing
      parser2 = Aeson.withObject "SingleHostName" $ \obj ->
        SingleHostName
          <$> obj .:? "port" .!= SNothing
          <*> obj .: "dnsName"
      parser3 = Aeson.withObject "MultiHostName" $ \obj ->
        MultiHostName
          <$> obj .: "dnsName"

instance ToJSON StakePoolRelay where
  toJSON (SingleHostAddr port ipv4 ipv6) =
    Aeson.object
      [ "single host address"
          .= Aeson.object
            [ "port" .= port,
              "IPv4" .= ipv4,
              "IPv6" .= ipv6
            ]
      ]
  toJSON (SingleHostName port dnsName) =
    Aeson.object
      [ "single host name"
          .= Aeson.object
            [ "port" .= port,
              "dnsName" .= dnsName
            ]
      ]
  toJSON (MultiHostName dnsName) =
    Aeson.object
      [ "multi host name"
          .= Aeson.object
            [ "dnsName" .= dnsName
            ]
      ]

instance NoThunks StakePoolRelay

instance NFData StakePoolRelay

instance ToCBOR StakePoolRelay where
  toCBOR (SingleHostAddr p ipv4 ipv6) =
    encodeListLen 4
      <> toCBOR (0 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe ipv4)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe ipv6)
  toCBOR (SingleHostName p n) =
    encodeListLen 3
      <> toCBOR (1 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> toCBOR n
  toCBOR (MultiHostName n) =
    encodeListLen 2
      <> toCBOR (2 :: Word8)
      <> toCBOR n

instance FromCBOR StakePoolRelay where
  fromCBOR = decodeRecordSum "StakePoolRelay" $
    \case
      0 ->
        (\x y z -> (4, SingleHostAddr x y z))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
      1 ->
        (\x y -> (3, SingleHostName x y))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> fromCBOR
      2 -> do
        x <- fromCBOR
        pure (2, MultiHostName x)
      k -> invalidKey k

-- | A stake pool.
data PoolParams c = PoolParams
  { ppId :: !(KeyHash 'StakePool c),
    ppVrf :: !(Hash c (VerKeyVRF c)),
    ppPledge :: !Coin,
    ppCost :: !Coin,
    ppMargin :: !UnitInterval,
    ppRewardAcnt :: !(RewardAcnt c),
    ppOwners :: !(Set (KeyHash 'Staking c)),
    ppRelays :: !(StrictSeq StakePoolRelay),
    ppMetadata :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (ToCBOR) via CBORGroup (PoolParams c)
  deriving (FromCBOR) via CBORGroup (PoolParams c)

instance NoThunks (PoolParams c)

deriving instance NFData (PoolParams c)

instance CC.Crypto c => ToJSON (PoolParams c) where
  toJSON pp =
    Aeson.object
      [ "publicKey" .= ppId pp, -- TODO publicKey is an unfortunate name, should be poolId
        "vrf" .= ppVrf pp,
        "pledge" .= ppPledge pp,
        "cost" .= ppCost pp,
        "margin" .= ppMargin pp,
        "rewardAccount" .= ppRewardAcnt pp,
        "owners" .= ppOwners pp,
        "relays" .= ppRelays pp,
        "metadata" .= ppMetadata pp
      ]

instance CC.Crypto c => FromJSON (PoolParams c) where
  parseJSON =
    Aeson.withObject "PoolParams" $ \obj ->
      PoolParams
        <$> obj .: "publicKey" -- TODO publicKey is an unfortunate name, should be poolId
        <*> obj .: "vrf"
        <*> obj .: "pledge"
        <*> obj .: "cost"
        <*> obj .: "margin"
        <*> obj .: "rewardAccount"
        <*> obj .: "owners"
        <*> obj .: "relays"
        <*> obj .: "metadata"

instance ToCBOR PoolMetadata where
  toCBOR (PoolMetadata u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetadata where
  fromCBOR = do
    decodeRecordNamed "PoolMetadata" (const 2) (PoolMetadata <$> fromCBOR <*> fromCBOR)

-- | The size of the 'ppOwners' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolOwners = SizeOfPoolOwners

instance ToCBOR SizeOfPoolOwners where
  toCBOR = error "The `SizeOfPoolOwners` type cannot be encoded!"

-- | The size of the 'ppRelays' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolRelays = SizeOfPoolRelays

instance ToCBOR SizeOfPoolRelays where
  toCBOR = error "The `SizeOfPoolRelays` type cannot be encoded!"

instance
  CC.Crypto c =>
  ToCBORGroup (PoolParams c)
  where
  toCBORGroup poolParams =
    toCBOR (ppId poolParams)
      <> toCBOR (ppVrf poolParams)
      <> toCBOR (ppPledge poolParams)
      <> toCBOR (ppCost poolParams)
      <> toCBOR (ppMargin poolParams)
      <> toCBOR (ppRewardAcnt poolParams)
      <> toCBOR (ppOwners poolParams)
      <> toCBOR (ppRelays poolParams)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe (ppMetadata poolParams))

  encodedGroupSizeExpr size' proxy =
    encodedSizeExpr size' (ppId <$> proxy)
      + encodedSizeExpr size' (ppVrf <$> proxy)
      + encodedSizeExpr size' (ppPledge <$> proxy)
      + encodedSizeExpr size' (ppCost <$> proxy)
      + encodedSizeExpr size' (ppMargin <$> proxy)
      + encodedSizeExpr size' (ppRewardAcnt <$> proxy)
      + 2
      + poolSize * encodedSizeExpr size' (elementProxy (ppOwners <$> proxy))
      + 2
      + relaySize * encodedSizeExpr size' (elementProxy (ppRelays <$> proxy))
      + szCases
        [ Case "Nothing" 1,
          Case "Just" $ encodedSizeExpr size' (elementProxy (ppMetadata <$> proxy))
        ]
    where
      poolSize, relaySize :: Size
      poolSize = size' (Proxy @SizeOfPoolOwners)
      relaySize = size' (Proxy @SizeOfPoolRelays)
      elementProxy :: Proxy (f a) -> Proxy a
      elementProxy _ = Proxy

  listLen _ = 9
  listLenBound _ = 9

instance
  CC.Crypto c =>
  FromCBORGroup (PoolParams c)
  where
  fromCBORGroup = do
    hk <- fromCBOR
    vrf <- fromCBOR
    pledge <- fromCBOR
    cost <- fromCBOR
    margin <- fromCBOR
    ra <- fromCBOR
    owners <- fromCBOR
    relays <- fromCBOR
    md <- decodeNullMaybe fromCBOR
    pure $
      PoolParams
        { ppId = hk,
          ppVrf = vrf,
          ppPledge = pledge,
          ppCost = cost,
          ppMargin = margin,
          ppRewardAcnt = ra,
          ppOwners = owners,
          ppRelays = relays,
          ppMetadata = maybeToStrictMaybe md
        }
