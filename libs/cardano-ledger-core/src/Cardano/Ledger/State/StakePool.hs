{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the 'StakePoolState' data type, which represents the
-- state of a stake pool within the ledger. Unlike 'PoolParams', which includes
-- the pool ID and is used for pool registration and updates, 'StakePoolState'
-- is designed specifically for state management and excludes the pool ID
-- (since it's already used as the key in state maps).
--
-- This separation allows for:
-- * More efficient state storage (no redundant pool ID)
-- * Future extensibility of pool state without affecting registration parameters
-- * Clear distinction between registration parameters and actual pool state
--
-- This module also contains all the legacy pool-related types that were
-- previously in 'Cardano.Ledger.PoolParams', which is now deprecated.
-- These types are re-exported by 'Cardano.Ledger.State' for convenient access.
module Cardano.Ledger.State.StakePool (
  -- * Stake Pool State
  StakePoolState (..),

  -- * Lenses
  spsVrfL,
  spsPledgeL,
  spsCostL,
  spsMarginL,
  spsRewardAccountL,
  spsOwnersL,
  spsRelaysL,
  spsMetadataL,
  spsDepositL,

  -- * Conversions
  mkStakePoolState,
  stakePoolStateToPoolParams,

  -- * Pool Parameters and Related Types

  -- | These types were previously defined in 'Cardano.Ledger.PoolParams'.
  -- They are now part of this module and re-exported by 'Cardano.Ledger.State'.
  PoolParams (..),
  PoolMetadata (..),
  StakePoolRelay (..),
  SizeOfPoolRelays (..),
  SizeOfPoolOwners (..),
  ppCostL,
  ppMetadataL,
  ppVrfL,
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (
  DnsName,
  Port,
  StrictMaybe (..),
  UnitInterval,
  Url,
  invalidKey,
 )
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (..),
  DecCBORGroup (..),
  DecShareCBOR (..),
  EncCBOR (..),
  EncCBORGroup (..),
  decodeNullStrictMaybe,
  decodeRecordNamed,
  decodeRecordSum,
  encodeListLen,
  encodeNullStrictMaybe,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), KeyRoleVRF (StakePoolVRF), VRFVerKeyHash)
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (Default (..))
import Data.Foldable (asum)
import Data.IP (IPv4, IPv6)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | State representation of a stake pool. This type contains all the same
-- information as 'PoolParams' except for the pool ID, which is stored
-- separately as the key in state maps.
data StakePoolState = StakePoolState
  { spsVrf :: !(VRFVerKeyHash 'StakePoolVRF)
  -- ^ VRF verification key hash for leader election
  , spsPledge :: !Coin
  -- ^ Pledge amount committed by the pool operator
  , spsCost :: !Coin
  -- ^ Fixed operational cost per epoch
  , spsMargin :: !UnitInterval
  -- ^ Pool profit margin (variable fee percentage)
  , spsRewardAccount :: !RewardAccount
  -- ^ Reward account for pool rewards
  , spsOwners :: !(Set (KeyHash 'Staking))
  -- ^ Set of stake key hashes that own this pool
  , spsRelays :: !(StrictSeq StakePoolRelay)
  -- ^ Network relay information for pool connectivity
  , spsMetadata :: !(StrictMaybe PoolMetadata)
  -- ^ Optional metadata (URL and hash)
  , spsDeposit :: !(CompactForm Coin)
  -- ^ Deposit for each pool
  }
  deriving (Show, Generic, Eq, Ord, NoThunks, NFData, FromJSON, ToJSON)

spsVrfL :: Lens' StakePoolState (VRFVerKeyHash 'StakePoolVRF)
spsVrfL = lens spsVrf (\sps u -> sps {spsVrf = u})

spsPledgeL :: Lens' StakePoolState Coin
spsPledgeL = lens spsPledge $ \sps c -> sps {spsPledge = c}

spsCostL :: Lens' StakePoolState Coin
spsCostL = lens spsCost $ \sps c -> sps {spsCost = c}

spsMarginL :: Lens' StakePoolState UnitInterval
spsMarginL = lens spsMargin $ \sps m -> sps {spsMargin = m}

spsRewardAccountL :: Lens' StakePoolState RewardAccount
spsRewardAccountL = lens spsRewardAccount $ \sps ra -> sps {spsRewardAccount = ra}

spsOwnersL :: Lens' StakePoolState (Set (KeyHash 'Staking))
spsOwnersL = lens spsOwners $ \sps s -> sps {spsOwners = s}

spsRelaysL :: Lens' StakePoolState (StrictSeq StakePoolRelay)
spsRelaysL = lens spsRelays $ \sps rs -> sps {spsRelays = rs}

spsMetadataL :: Lens' StakePoolState (StrictMaybe PoolMetadata)
spsMetadataL = lens spsMetadata $ \sps md -> sps {spsMetadata = md}

spsDepositL :: Lens' StakePoolState (CompactForm Coin)
spsDepositL = lens spsDeposit $ \sps d -> sps {spsDeposit = d}

instance EncCBOR StakePoolState where
  encCBOR sps =
    encode $
      Rec StakePoolState
        !> To (spsVrf sps)
        !> To (spsPledge sps)
        !> To (spsCost sps)
        !> To (spsMargin sps)
        !> To (spsRewardAccount sps)
        !> To (spsOwners sps)
        !> To (spsRelays sps)
        !> To (spsMetadata sps)
        !> To (spsDeposit sps)

instance DecCBOR StakePoolState where
  decCBOR =
    decode $
      RecD StakePoolState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance DecShareCBOR StakePoolState where
  decShareCBOR _ = decCBOR

instance Default StakePoolState where
  def =
    StakePoolState
      { spsVrf = def
      , spsPledge = Coin 0
      , spsCost = Coin 0
      , spsMargin = def
      , spsRewardAccount = def
      , spsOwners = def
      , spsRelays = def
      , spsMetadata = def
      , spsDeposit = mempty
      }

-- | Convert 'PoolParams' to 'StakePoolState' by dropping the pool ID.
-- This is the primary way to create a 'StakePoolState' from registration
-- or update parameters.
mkStakePoolState :: CompactForm Coin -> PoolParams -> StakePoolState
mkStakePoolState deposit pp =
  StakePoolState
    { spsVrf = ppVrf pp
    , spsPledge = ppPledge pp
    , spsCost = ppCost pp
    , spsMargin = ppMargin pp
    , spsRewardAccount = ppRewardAccount pp
    , spsOwners = ppOwners pp
    , spsRelays = ppRelays pp
    , spsMetadata = ppMetadata pp
    , spsDeposit = deposit
    }

-- | Convert 'StakePoolState' back to 'PoolParams' by providing the pool ID.
-- This is useful when you need to reconstruct the full parameters from
-- the state representation.
stakePoolStateToPoolParams :: KeyHash 'StakePool -> StakePoolState -> PoolParams
stakePoolStateToPoolParams poolId sps =
  PoolParams
    { ppId = poolId
    , ppVrf = spsVrf sps
    , ppPledge = spsPledge sps
    , ppCost = spsCost sps
    , ppMargin = spsMargin sps
    , ppRewardAccount = spsRewardAccount sps
    , ppOwners = spsOwners sps
    , ppRelays = spsRelays sps
    , ppMetadata = spsMetadata sps
    }

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
  txt <- parseJSON v
  unless (Text.isAscii txt) $ fail $ "Supplied text contains non-ASCII characters: " <> show txt
  case B16.decode (Text.encodeUtf8 txt) of
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
      <> encodeNullStrictMaybe encCBOR p
      <> encodeNullStrictMaybe encCBOR ipv4
      <> encodeNullStrictMaybe encCBOR ipv6
  encCBOR (SingleHostName p n) =
    encodeListLen 3
      <> encCBOR (1 :: Word8)
      <> encodeNullStrictMaybe encCBOR p
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
          <$> decodeNullStrictMaybe decCBOR
          <*> decodeNullStrictMaybe decCBOR
          <*> decodeNullStrictMaybe decCBOR
      1 ->
        (\x y -> (3, SingleHostName x y))
          <$> decodeNullStrictMaybe decCBOR
          <*> decCBOR
      2 -> do
        x <- decCBOR
        pure (2, MultiHostName x)
      k -> invalidKey k

-- | A stake pool.
data PoolParams = PoolParams
  { ppId :: !(KeyHash 'StakePool)
  , ppVrf :: !(VRFVerKeyHash 'StakePoolVRF)
  , ppPledge :: !Coin
  , ppCost :: !Coin
  , ppMargin :: !UnitInterval
  , ppRewardAccount :: !RewardAccount
  , ppOwners :: !(Set (KeyHash 'Staking))
  , ppRelays :: !(StrictSeq StakePoolRelay)
  , ppMetadata :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (EncCBOR) via CBORGroup PoolParams
  deriving (DecCBOR) via CBORGroup PoolParams

ppVrfL :: Lens' PoolParams (VRFVerKeyHash 'StakePoolVRF)
ppVrfL = lens ppVrf (\pp u -> pp {ppVrf = u})

ppCostL :: Lens' PoolParams Coin
ppCostL = lens ppCost (\pp u -> pp {ppCost = u})

ppMetadataL :: Lens' PoolParams (StrictMaybe PoolMetadata)
ppMetadataL = lens ppMetadata (\pp u -> pp {ppMetadata = u})

instance Default PoolParams where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

instance NoThunks PoolParams

deriving instance NFData PoolParams

instance ToJSON PoolParams where
  toJSON pp =
    Aeson.object
      [ "publicKey" .= ppId pp -- TODO publicKey is an unfortunate name, should be poolId
      , "vrf" .= ppVrf pp
      , "pledge" .= ppPledge pp
      , "cost" .= ppCost pp
      , "margin" .= ppMargin pp
      , "rewardAccount" .= ppRewardAccount pp
      , "owners" .= ppOwners pp
      , "relays" .= ppRelays pp
      , "metadata" .= ppMetadata pp
      ]

instance FromJSON PoolParams where
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

instance EncCBORGroup PoolParams where
  encCBORGroup poolParams =
    encCBOR (ppId poolParams)
      <> encCBOR (ppVrf poolParams)
      <> encCBOR (ppPledge poolParams)
      <> encCBOR (ppCost poolParams)
      <> encCBOR (ppMargin poolParams)
      <> encCBOR (ppRewardAccount poolParams)
      <> encCBOR (ppOwners poolParams)
      <> encCBOR (ppRelays poolParams)
      <> encodeNullStrictMaybe encCBOR (ppMetadata poolParams)
  listLen _ = 9
  listLenBound _ = 9

instance DecCBORGroup PoolParams where
  decCBORGroup = do
    hk <- decCBOR
    vrf <- decCBOR
    pledge <- decCBOR
    cost <- decCBOR
    margin <- decCBOR
    ra <- decCBOR
    owners <- decCBOR
    relays <- decCBOR
    md <- decodeNullStrictMaybe decCBOR
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
        , ppMetadata = md
        }
