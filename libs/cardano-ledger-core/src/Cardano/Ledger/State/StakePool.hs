{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
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
  spsAccountIdL,
  spsOwnersL,
  spsRelaysL,
  spsMetadataL,
  spsDelegatorsL,
  spsDepositL,

  -- * Conversions
  mkStakePoolState,
  stakePoolStateToStakePoolParams,

  -- * Pool Parameters and Related Types

  -- | These types were previously defined in 'Cardano.Ledger.PoolParams'.
  -- They are now part of this module and re-exported by 'Cardano.Ledger.State'.
  StakePoolParams (
    ..,
    PoolParams,
    ppId,
    ppVrf,
    ppLeiosKey,
    ppPledge,
    ppCost,
    ppMargin,
    ppAccountAddress,
    ppOwners,
    ppRelays,
    ppMetadata
  ),
  withStakePoolParamsFlatEncoding,
  decodeStakePoolParamsFlat,
  PoolMetadata (..),
  LeiosKey (..),
  LeiosPubKey (..),
  LeiosPossessionProof (..),
  StakePoolRelay (..),
  SizeOfPoolRelays (..),
  SizeOfPoolOwners (..),
  sppCostL,
  sppMetadataL,
  sppVrfL,
) where

import Cardano.Base.IP (IPv4, IPv6)
import Cardano.Crypto.DSIGN (
  BLS12381MinSigDSIGN,
  DSIGNAggregatable (
    PossessionProofDSIGN,
    rawDeserialisePossessionProofDSIGN,
    rawSerialisePossessionProofDSIGN
  ),
  DSIGNAlgorithm (VerKeyDSIGN, rawDeserialiseVerKeyDSIGN, rawSerialiseVerKeyDSIGN),
 )
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (
  DnsName,
  Network,
  Port,
  StrictMaybe (..),
  UnitInterval,
  Url,
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  Interns,
  TokenType (TypeListLen, TypeListLen64, TypeListLenIndef, TypeNull),
  decodeNull,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  decodeRecordNamedT,
  decodeRecordSum,
  encodeListLen,
  encodeNullStrictMaybe,
  ifDecoderVersionAtLeast,
  natVersion,
  peekTokenType,
  withCurrentEncodingVersion,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Binary.Crypto (
  decodePossessionProofDSIGN,
  decodeVerKeyDSIGN,
  encodePossessionProofDSIGN,
  encodeVerKeyDSIGN,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), KeyRoleVRF (StakePoolVRF), VRFVerKeyHash)
import Cardano.Ledger.Orphans ()
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, explicitParseField)
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import Data.Default (Default (..))
import Data.Foldable (asum)
import Data.MemPack.Buffer (byteArrayFromShortByteString, byteArrayToShortByteString)
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
  { spsVrf :: !(VRFVerKeyHash StakePoolVRF)
  -- ^ VRF verification key hash for leader election
  , spsPledge :: !Coin
  -- ^ Pledge amount committed by the pool operator
  , spsCost :: !Coin
  -- ^ Fixed operational cost per epoch
  , spsMargin :: !UnitInterval
  -- ^ Pool profit margin (variable fee percentage)
  , spsAccountId :: !AccountId
  -- ^ Account address credential for pool rewards
  , spsOwners :: !(Set (KeyHash Staking))
  -- ^ Set of stake key hashes that own this pool
  , spsRelays :: !(StrictSeq StakePoolRelay)
  -- ^ Network relay information for pool connectivity
  , spsMetadata :: !(StrictMaybe PoolMetadata)
  -- ^ Optional metadata (URL and hash)
  , spsDeposit :: !(CompactForm Coin)
  -- ^ Deposit for each pool
  , spsDelegators :: !(Set (Credential Staking))
  -- ^ Credentials that have delegated to the pool
  }
  deriving (Show, Generic, Eq, Ord, NoThunks, NFData, FromJSON, ToJSON)

spsVrfL :: Lens' StakePoolState (VRFVerKeyHash StakePoolVRF)
spsVrfL = lens spsVrf (\sps u -> sps {spsVrf = u})

spsPledgeL :: Lens' StakePoolState Coin
spsPledgeL = lens spsPledge $ \sps c -> sps {spsPledge = c}

spsCostL :: Lens' StakePoolState Coin
spsCostL = lens spsCost $ \sps c -> sps {spsCost = c}

spsMarginL :: Lens' StakePoolState UnitInterval
spsMarginL = lens spsMargin $ \sps m -> sps {spsMargin = m}

spsAccountIdL :: Lens' StakePoolState AccountId
spsAccountIdL = lens spsAccountId $ \sps sc -> sps {spsAccountId = sc}

spsOwnersL :: Lens' StakePoolState (Set (KeyHash Staking))
spsOwnersL = lens spsOwners $ \sps s -> sps {spsOwners = s}

spsRelaysL :: Lens' StakePoolState (StrictSeq StakePoolRelay)
spsRelaysL = lens spsRelays $ \sps rs -> sps {spsRelays = rs}

spsMetadataL :: Lens' StakePoolState (StrictMaybe PoolMetadata)
spsMetadataL = lens spsMetadata $ \sps md -> sps {spsMetadata = md}

spsDepositL :: Lens' StakePoolState (CompactForm Coin)
spsDepositL = lens spsDeposit $ \sps d -> sps {spsDeposit = d}

spsDelegatorsL :: Lens' StakePoolState (Set (Credential Staking))
spsDelegatorsL = lens spsDelegators $ \sps delegators -> sps {spsDelegators = delegators}

instance EncCBOR StakePoolState where
  encCBOR sps =
    encode $
      Rec StakePoolState
        !> To (spsVrf sps)
        !> To (spsPledge sps)
        !> To (spsCost sps)
        !> To (spsMargin sps)
        !> To (spsAccountId sps)
        !> To (spsOwners sps)
        !> To (spsRelays sps)
        !> To (spsMetadata sps)
        !> To (spsDeposit sps)
        !> To (spsDelegators sps)

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
        <! From

instance DecShareCBOR StakePoolState where
  type Share StakePoolState = Interns (Credential Staking)
  decSharePlusCBOR =
    decodeRecordNamedT "StakePoolState" (const 10) $
      StakePoolState
        <$> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> lift decCBOR
        <*> decSharePlusCBOR

instance Default StakePoolState where
  def =
    StakePoolState
      { spsVrf = def
      , spsPledge = Coin 0
      , spsCost = Coin 0
      , spsMargin = def
      , spsAccountId = AccountId def
      , spsOwners = def
      , spsRelays = def
      , spsMetadata = def
      , spsDeposit = mempty
      , spsDelegators = def
      }

-- | Convert 'StakePoolParams' to 'StakePoolState' by dropping the pool ID.
-- This is the primary way to create a 'StakePoolState' from registration
-- or update parameters.
mkStakePoolState ::
  CompactForm Coin -> Set (Credential Staking) -> StakePoolParams -> StakePoolState
mkStakePoolState deposit delegators spp =
  StakePoolState
    { spsVrf = sppVrf spp
    , spsPledge = sppPledge spp
    , spsCost = sppCost spp
    , spsMargin = sppMargin spp
    , spsAccountId = aaId (sppAccountAddress spp)
    , spsOwners = sppOwners spp
    , spsRelays = sppRelays spp
    , spsMetadata = sppMetadata spp
    , spsDeposit = deposit
    , spsDelegators = delegators
    }

-- | Convert 'StakePoolState' back to 'StakePoolParams' by providing the pool ID.
-- This is useful when you need to reconstruct the full parameters from
-- the state representation.
stakePoolStateToStakePoolParams :: Network -> KeyHash StakePool -> StakePoolState -> StakePoolParams
stakePoolStateToStakePoolParams networkId poolId sps =
  StakePoolParams
    { sppId = poolId
    , sppVrf = spsVrf sps
    , sppLeiosKey = SNothing
    , sppPledge = spsPledge sps
    , sppCost = spsCost sps
    , sppMargin = spsMargin sps
    , sppAccountAddress =
        AccountAddress
          { aaNetworkId = networkId
          , aaId = spsAccountId sps
          }
    , sppOwners = spsOwners sps
    , sppRelays = spsRelays sps
    , sppMetadata = spsMetadata sps
    }

data PoolMetadata = PoolMetadata
  { pmUrl :: !Url
  , pmHash :: !ByteArray
  }
  deriving (Eq, Ord, Generic, Show)

deriving instance NFData PoolMetadata

instance ToJSON PoolMetadata where
  toJSON pmd =
    Aeson.object
      [ "url" .= pmUrl pmd
      , "hash" .= Text.decodeLatin1 (B16.encode $ SBS.fromShort $ byteArrayToShortByteString $ pmHash pmd)
      ]

instance FromJSON PoolMetadata where
  parseJSON =
    Aeson.withObject "PoolMetadata" $ \obj -> do
      url <- obj .: "url"
      hash <- explicitParseField parseJsonBase16 obj "hash"
      return $ PoolMetadata url hash

parseJsonBase16 :: Value -> Parser ByteArray
parseJsonBase16 v = do
  txt <- parseJSON v
  unless (Text.isAscii txt) $ fail $ "Supplied text contains non-ASCII characters: " <> show txt
  case B16.decode $ Text.encodeUtf8 txt of
    Right bs -> return $ byteArrayFromShortByteString $ SBS.toShort bs
    Left msg -> fail msg

-- | Check WHNF by pattern-matching on both fields, since they both are strict, and just contain ByteArray#
instance NoThunks PoolMetadata where
  wNoThunks _ (PoolMetadata _ _) = return Nothing
  showTypeOf _ = "PoolMetadata"

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
data StakePoolParams = StakePoolParams
  { sppId :: !(KeyHash StakePool)
  , sppVrf :: !(VRFVerKeyHash StakePoolVRF)
  , sppLeiosKey :: !(StrictMaybe LeiosKey)
  , sppPledge :: !Coin
  , sppCost :: !Coin
  , sppMargin :: !UnitInterval
  , sppAccountAddress :: !AccountAddress
  , sppOwners :: !(Set (KeyHash Staking))
  , sppRelays :: !(StrictSeq StakePoolRelay)
  , sppMetadata :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Generic, Eq, Ord)

data LeiosKey = LeiosKey
  { leiosPubKey :: !LeiosPubKey
  , leiosPossessionProof :: !LeiosPossessionProof
  }
  deriving (Show, Generic, Eq, Ord, NoThunks, NFData)

instance ToJSON LeiosKey where
  toJSON leiosKey =
    Aeson.object
      [ "leiosPubKey"
          .= Text.decodeLatin1 (B16.encode (rawSerialiseVerKeyDSIGN $ unLeiosPubKey $ leiosPubKey leiosKey))
      , "leiosPossessionProof"
          .= Text.decodeLatin1
            ( B16.encode
                (rawSerialisePossessionProofDSIGN $ unLeiosPossessionProof $ leiosPossessionProof leiosKey)
            )
      ]

instance FromJSON LeiosKey where
  parseJSON = Aeson.withObject "LeiosKey" $ \obj -> do
    pubKeyHex <- Text.encodeUtf8 <$> obj .: "leiosPubKey"
    leiosPossessionProofHex <- Text.encodeUtf8 <$> obj .: "leiosPossessionProof"
    leiosPubKey <-
      case rawDeserialiseVerKeyDSIGN =<< either (const Nothing) Just (B16.decode pubKeyHex) of
        Nothing -> fail "Invalid hex for LeiosPubKey"
        Just vk -> pure (LeiosPubKey vk)
    leiosPossessionProof <-
      case rawDeserialisePossessionProofDSIGN
        =<< either (const Nothing) Just (B16.decode leiosPossessionProofHex) of
        Nothing -> fail "Invalid hex for LeiosPossessionProof"
        Just p -> pure (LeiosPossessionProof p)
    pure $ LeiosKey {leiosPubKey, leiosPossessionProof}

-- TODO Should be moved to cardano-base
newtype LeiosPubKey = LeiosPubKey
  { unLeiosPubKey :: VerKeyDSIGN BLS12381MinSigDSIGN
  }
  deriving (Show, Generic, Eq, NoThunks, NFData)

instance Ord LeiosPubKey where
  compare a b =
    compare
      (rawSerialiseVerKeyDSIGN (unLeiosPubKey a))
      (rawSerialiseVerKeyDSIGN (unLeiosPubKey b))

-- TODO Should be moved to cardano-base
newtype LeiosPossessionProof = LeiosPossessionProof
  { unLeiosPossessionProof :: PossessionProofDSIGN BLS12381MinSigDSIGN
  }
  deriving (Show, Generic, Eq, NoThunks, NFData)

instance Ord LeiosPossessionProof where
  compare a b =
    compare
      (rawSerialisePossessionProofDSIGN (unLeiosPossessionProof a))
      (rawSerialisePossessionProofDSIGN (unLeiosPossessionProof b))

instance EncCBOR LeiosKey where
  encCBOR lk =
    encodeListLen 2
      <> encodeVerKeyDSIGN (unLeiosPubKey $ leiosPubKey lk)
      <> encodePossessionProofDSIGN (unLeiosPossessionProof $ leiosPossessionProof lk)

instance DecCBOR LeiosKey where
  decCBOR = decodeRecordNamed "LeiosKey" (const 2) $ do
    pubKey <- LeiosPubKey <$> decodeVerKeyDSIGN
    proof <- LeiosPossessionProof <$> decodePossessionProofDSIGN
    pure LeiosKey {leiosPubKey = pubKey, leiosPossessionProof = proof}

sppVrfL :: Lens' StakePoolParams (VRFVerKeyHash StakePoolVRF)
sppVrfL = lens sppVrf (\spp u -> spp {sppVrf = u})

sppCostL :: Lens' StakePoolParams Coin
sppCostL = lens sppCost (\spp u -> spp {sppCost = u})

sppMetadataL :: Lens' StakePoolParams (StrictMaybe PoolMetadata)
sppMetadataL = lens sppMetadata (\spp u -> spp {sppMetadata = u})

instance Default StakePoolParams where
  def = StakePoolParams def def def (Coin 0) (Coin 0) def def def def def

instance NoThunks StakePoolParams

deriving instance NFData StakePoolParams

instance ToJSON StakePoolParams where
  toJSON spp =
    Aeson.object
      [ "poolId" .= sppId spp
      , "vrf" .= sppVrf spp
      , "leiosKey" .= sppLeiosKey spp
      , "pledge" .= sppPledge spp
      , "cost" .= sppCost spp
      , "margin" .= sppMargin spp
      , "accountAddress" .= sppAccountAddress spp
      , "owners" .= sppOwners spp
      , "relays" .= sppRelays spp
      , "metadata" .= sppMetadata spp
      ]

instance FromJSON StakePoolParams where
  parseJSON =
    Aeson.withObject "StakePoolParams" $ \obj ->
      -- Preserved for backward-compatibility,
      -- "publicKey" and "rewardAccount" are old misnomers.
      StakePoolParams
        <$> ((obj .: "poolId") <|> (obj .: "publicKey"))
        <*> obj .: "vrf"
        <*> obj .:? "leiosKey" .!= SNothing
        <*> obj .: "pledge"
        <*> obj .: "cost"
        <*> obj .: "margin"
        <*> ((obj .: "accountAddress") <|> (obj .: "rewardAccount"))
        <*> obj .: "owners"
        <*> obj .: "relays"
        <*> obj .: "metadata"

type PoolParams = StakePoolParams

pattern PoolParams ::
  KeyHash StakePool ->
  VRFVerKeyHash StakePoolVRF ->
  StrictMaybe LeiosKey ->
  Coin ->
  Coin ->
  UnitInterval ->
  AccountAddress ->
  Set (KeyHash Staking) ->
  StrictSeq StakePoolRelay ->
  StrictMaybe PoolMetadata ->
  PoolParams
pattern PoolParams
  { ppId
  , ppVrf
  , ppLeiosKey
  , ppPledge
  , ppCost
  , ppMargin
  , ppAccountAddress
  , ppOwners
  , ppRelays
  , ppMetadata
  } =
  StakePoolParams
    ppId
    ppVrf
    ppLeiosKey
    ppPledge
    ppCost
    ppMargin
    ppAccountAddress
    ppOwners
    ppRelays
    ppMetadata

{-# COMPLETE PoolParams #-}

{-# DEPRECATED PoolParams "In favor of `StakePoolParams`" #-}

{-# DEPRECATED
  ppId
  , ppVrf
  , ppLeiosKey
  , ppPledge
  , ppCost
  , ppMargin
  , ppAccountAddress
  , ppOwners
  , ppRelays
  , ppMetadata
  "In favor of fields with `spp*` prefix"
  #-}

instance EncCBOR PoolMetadata where
  encCBOR (PoolMetadata u h) =
    encodeListLen 2
      <> encCBOR u
      <> encCBOR h

instance DecCBOR PoolMetadata where
  decCBOR = do
    decodeRecordNamed "PoolMetadata" (const 2) (PoolMetadata <$> decCBOR <*> decCBOR)

-- | The size of the 'sppOwners' 'Set'.  Only used to compute size of encoded
-- 'StakePoolParams'.
data SizeOfPoolOwners = SizeOfPoolOwners

instance EncCBOR SizeOfPoolOwners where
  encCBOR = error "The `SizeOfPoolOwners` type cannot be encoded!"

-- | The size of the 'sppRelays' 'Set'.  Only used to compute size of encoded
-- 'StakePoolParams'.
data SizeOfPoolRelays = SizeOfPoolRelays

instance EncCBOR SizeOfPoolRelays where
  encCBOR = error "The `SizeOfPoolRelays` type cannot be encoded!"

instance EncCBOR StakePoolParams where
  encCBOR poolParams =
    withStakePoolParamsFlatEncoding poolParams $ \stakePoolParamsListLen stakePoolParamsEncoding ->
      encodeListLen (fromIntegral stakePoolParamsListLen)
        <> stakePoolParamsEncoding

-- | Returns the 'StakePoolParams' in a flat structure such as `[operator, vrf,
-- pledge, ...]`. Useful for combining with other encoding functions like
-- 'encodePoolCert' so that those functions end up with a flat list encoding
-- instead of a nested list.
--
-- We could have used 'EncCBORGroup' to achieve something similar. However, the
-- `listLen` function is static and can't depend on the Encoding version.
-- Instead, we want `StakePoolParams` to have a dynamic list length based on the
-- protocol version.
withStakePoolParamsFlatEncoding ::
  StakePoolParams ->
  -- | Function with stake pool params flat encoded list length and actual flat
  -- encoded stake pool params
  (Int -> Encoding -> Encoding) ->
  Encoding
withStakePoolParamsFlatEncoding poolParams f =
  withCurrentEncodingVersion $ \v -> do
    let (extraLen, leiosKeyEncoding)
          | v >= natVersion @12 =
              case sppLeiosKey poolParams of
                SJust lk -> (1, encCBOR lk)
                SNothing -> (0, mempty)
          | otherwise = (0, mempty)
    f (9 + extraLen) $
      encCBOR (sppId poolParams)
        <> encCBOR (sppVrf poolParams)
        <> leiosKeyEncoding
        <> encCBOR (sppPledge poolParams)
        <> encCBOR (sppCost poolParams)
        <> encCBOR (sppMargin poolParams)
        <> encCBOR (sppAccountAddress poolParams)
        <> encCBOR (sppOwners poolParams)
        <> encCBOR (sppRelays poolParams)
        <> encodeNullStrictMaybe encCBOR (sppMetadata poolParams)

instance DecCBOR StakePoolParams where
  decCBOR = do
    snd <$> decodeRecordNamed "StakePoolParams" fst decodeStakePoolParamsFlat

decodeStakePoolParamsFlat :: Decoder s (Int, StakePoolParams)
decodeStakePoolParamsFlat = do
  sppId <- decCBOR
  sppVrf <- decCBOR
  (extraLeiosKeyLen, sppLeiosKey) <-
    ifDecoderVersionAtLeast
      (natVersion @12)
      decodeLeiosKeyBackwardsCompatible
      (pure (0, SNothing))
  sppPledge <- decCBOR
  sppCost <- decCBOR
  sppMargin <- decCBOR
  sppAccountAddress <- decCBOR
  sppOwners <- decCBOR
  sppRelays <- decCBOR
  sppMetadata <- decodeNullStrictMaybe decCBOR
  pure (9 + extraLeiosKeyLen, StakePoolParams {..})
  where
    decodeLeiosKeyBackwardsCompatible = do
      peekTokenType >>= \case
        TypeListLen -> (\lk -> (1, SJust lk)) <$> decCBOR
        TypeListLen64 -> (\lk -> (1, SJust lk)) <$> decCBOR
        TypeListLenIndef -> (\lk -> (1, SJust lk)) <$> decCBOR
        TypeNull -> do
          decodeNull
          pure (1, SNothing)
        _ -> pure (0, SNothing)
