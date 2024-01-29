{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.PoolParams (
  PoolParams (..),
  PoolMetadata (..),
  StakePoolRelay (..),
  SizeOfPoolRelays (..),
  SizeOfPoolOwners (..),

  -- * Deprecations
  ppRewardAcnt,
)
where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  DnsName,
  Port,
  StrictMaybe (..),
  UnitInterval,
  Url,
  invalidKey,
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  CBORGroup (..),
  Case (..),
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  Size,
  decodeNullMaybe,
  decodeRecordNamed,
  decodeRecordSum,
  encodeListLen,
  encodeNullMaybe,
  szCases,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  Hash,
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
import Data.Default.Class (Default (..))
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
  { pmUrl :: !Url
  , pmHash :: !ByteString
  }
  deriving (Eq, Ord, Generic, Show)

deriving instance NFData PoolMetadata

instance ToJSON PoolMetadata where
  toJSON pmd =
    Aeson.object
      [ "url" .= pmUrl pmd
      , "hash" .= Text.decodeLatin1 (B16.encode (pmHash pmd))
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
    Aeson.withObject "StakePoolRelay" $ \obj ->
      asum
        [ explicitParseField parser1 obj "single host address"
        , explicitParseField parser2 obj "single host name"
        , explicitParseField parser3 obj "multi host name"
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
            [ "port" .= port
            , "IPv4" .= ipv4
            , "IPv6" .= ipv6
            ]
      ]
  toJSON (SingleHostName port dnsName) =
    Aeson.object
      [ "single host name"
          .= Aeson.object
            [ "port" .= port
            , "dnsName" .= dnsName
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

instance EncCBOR StakePoolRelay where
  encCBOR (SingleHostAddr p ipv4 ipv6) =
    encodeListLen 4
      <> encCBOR (0 :: Word8)
      <> encodeNullMaybe encCBOR (strictMaybeToMaybe p)
      <> encodeNullMaybe encCBOR (strictMaybeToMaybe ipv4)
      <> encodeNullMaybe encCBOR (strictMaybeToMaybe ipv6)
  encCBOR (SingleHostName p n) =
    encodeListLen 3
      <> encCBOR (1 :: Word8)
      <> encodeNullMaybe encCBOR (strictMaybeToMaybe p)
      <> encCBOR n
  encCBOR (MultiHostName n) =
    encodeListLen 2
      <> encCBOR (2 :: Word8)
      <> encCBOR n

instance DecCBOR StakePoolRelay where
  decCBOR = decodeRecordSum "StakePoolRelay" $
    \case
      0 ->
        (\x y z -> (4, SingleHostAddr x y z))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe decCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe decCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe decCBOR)
      1 ->
        (\x y -> (3, SingleHostName x y))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe decCBOR)
          <*> decCBOR
      2 -> do
        x <- decCBOR
        pure (2, MultiHostName x)
      k -> invalidKey k

-- | A stake pool.
data PoolParams c = PoolParams
  { ppId :: !(KeyHash 'StakePool c)
  , ppVrf :: !(Hash c (VerKeyVRF c))
  , ppPledge :: !Coin
  , ppCost :: !Coin
  , ppMargin :: !UnitInterval
  , ppRewardAccount :: !(RewardAccount c)
  , ppOwners :: !(Set (KeyHash 'Staking c))
  , ppRelays :: !(StrictSeq StakePoolRelay)
  , ppMetadata :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (EncCBOR) via CBORGroup (PoolParams c)
  deriving (DecCBOR) via CBORGroup (PoolParams c)

ppRewardAcnt :: PoolParams c -> RewardAccount c
ppRewardAcnt = ppRewardAccount
{-# DEPRECATED ppRewardAcnt "Use `ppRewardAccount` instead" #-}

instance Crypto c => Default (PoolParams c) where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

instance NoThunks (PoolParams c)

deriving instance NFData (PoolParams c)

instance Crypto c => ToJSON (PoolParams c) where
  toJSON pp =
    Aeson.object
      [ "publicKey" .= ppId pp -- TODO publicKey is an unfortunate name, should be poolId
      , "vrf" .= ppVrf pp
      , "pledge" .= ppPledge pp
      , "cost" .= ppCost pp
      , "margin" .= ppMargin pp
      , "rewardAccount" .= ppRewardAcnt pp
      , "owners" .= ppOwners pp
      , "relays" .= ppRelays pp
      , "metadata" .= ppMetadata pp
      ]

instance Crypto c => FromJSON (PoolParams c) where
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

instance EncCBOR PoolMetadata where
  encCBOR (PoolMetadata u h) =
    encodeListLen 2
      <> encCBOR u
      <> encCBOR h

instance DecCBOR PoolMetadata where
  decCBOR = do
    decodeRecordNamed "PoolMetadata" (const 2) (PoolMetadata <$> decCBOR <*> decCBOR)

-- | The size of the 'ppOwners' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolOwners = SizeOfPoolOwners

instance EncCBOR SizeOfPoolOwners where
  encCBOR = error "The `SizeOfPoolOwners` type cannot be encoded!"

-- | The size of the 'ppRelays' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolRelays = SizeOfPoolRelays

instance EncCBOR SizeOfPoolRelays where
  encCBOR = error "The `SizeOfPoolRelays` type cannot be encoded!"

instance Crypto c => EncCBORGroup (PoolParams c) where
  encCBORGroup poolParams =
    encCBOR (ppId poolParams)
      <> encCBOR (ppVrf poolParams)
      <> encCBOR (ppPledge poolParams)
      <> encCBOR (ppCost poolParams)
      <> encCBOR (ppMargin poolParams)
      <> encCBOR (ppRewardAcnt poolParams)
      <> encCBOR (ppOwners poolParams)
      <> encCBOR (ppRelays poolParams)
      <> encodeNullMaybe encCBOR (strictMaybeToMaybe (ppMetadata poolParams))

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
        [ Case "Nothing" 1
        , Case "Just" $ encodedSizeExpr size' (elementProxy (ppMetadata <$> proxy))
        ]
    where
      poolSize, relaySize :: Size
      poolSize = size' (Proxy @SizeOfPoolOwners)
      relaySize = size' (Proxy @SizeOfPoolRelays)
      elementProxy :: Proxy (f a) -> Proxy a
      elementProxy _ = Proxy

  listLen _ = 9
  listLenBound _ = 9

instance Crypto c => DecCBORGroup (PoolParams c) where
  decCBORGroup = do
    hk <- decCBOR
    vrf <- decCBOR
    pledge <- decCBOR
    cost <- decCBOR
    margin <- decCBOR
    ra <- decCBOR
    owners <- decCBOR
    relays <- decCBOR
    md <- decodeNullMaybe decCBOR
    pure $
      PoolParams
        { ppId = hk
        , ppVrf = vrf
        , ppPledge = pledge
        , ppCost = cost
        , ppMargin = margin
        , ppRewardAccount = ra
        , ppOwners = owners
        , ppRelays = relays
        , ppMetadata = maybeToStrictMaybe md
        }
