{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Accounts.V0 (
  AccountsIn (..),
  AccountsOut (..),
  CanonicalStakePoolParams (..),
  mkCanonicalStakePoolParams,
  fromCanonicalStakePoolParams,
  mkCanonicalPState,
) where

import Cardano.Ledger.BaseTypes (UnitInterval, StrictMaybe, NonZero (..), EpochNo)
import Cardano.Ledger.Binary (EncCBOR, encCBOR, serialize')
import Cardano.Ledger.CanonicalState.LedgerCBOR
import Cardano.Ledger.Core (eraProtVerLow, fromEraShareCBOR)
import Cardano.Ledger.Keys
import Cardano.Ledger.CanonicalState.BasicTypes(
  CanonicalCoin (..),
  CanonicalRewardAccount (..),
  mkCanonicalRewardAccount,
  fromCanonicalVRFVerKeyHash,
  mkCanonicalVRFVerKeyHash,
  CanonicalVRFVerKeyHash (..),
  mkCanonicalRewardAccount,
  fromCanonicalRewardAccount,
  )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.State
import Cardano.Ledger.Coin (compactCoinOrError)
import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder, assumeCanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeMapLenCanonicalOf,
  decodeMapLenCanonical,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.SCLS.Versioned (Versioned (..))
import Control.Monad (unless)
import qualified Data.ByteString.Base16 as Base16
import Data.Foldable (toList)
import Data.MemPack (MemPack (..))
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)

data AccountsIn = AccountsInDState | AccountsInPState
  deriving (Eq, Ord, Show, Generic)

instance MemPack AccountsIn where
  packedByteCount _ = 1
  packM AccountsInDState = packM (0 :: Word8)
  packM AccountsInPState = packM (1 :: Word8)
  unpackM = do
    tag :: Word8 <- unpackM
    case tag of
      0 -> return AccountsInDState
      1 -> return AccountsInPState
      _ -> fail "Invalid AccountsIn tag"


instance IsKey AccountsIn where
  keySize = namespaceKeySize @"accounts/v0"
  packKeyM = packM
  unpackKeyM = unpackM

data AccountsOut era where
  AccountDState :: DState era -> AccountsOut era
  AccountPState :: CanonicalPState -> AccountsOut era
  deriving (Generic)

instance (Era era, EncCBOR (DState era)) => Eq (AccountsOut era) where
  AccountDState lhs == AccountDState rhs =
    serialize' (eraProtVerLow @era) (encCBOR lhs)
      == serialize' (eraProtVerLow @era) (encCBOR rhs)
  AccountPState lhs == AccountPState rhs = lhs == rhs
  _ == _ = False

instance (Era era, EncCBOR (DState era)) => Show (AccountsOut era) where
  show (AccountDState dState) =
    "AccountDState "
      ++ show (Base16.encode (serialize' (eraProtVerLow @era) (encCBOR dState)))
  show (AccountPState pState) = "AccountPState " ++ show pState

data CanonicalPState = CanonicalPState
  { cpsRetiring :: Map (KeyHash StakePool)  EpochNo
  , cpsVrfKeyHashes :: Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
  , cpsFutureStakePoolParams :: Map (KeyHash StakePool) CanonicalStakePoolParams
  }
  deriving (Eq, Show, Generic)

instance (Era era, NamespaceEra v ~ era, EncCBOR (Accounts era)) => ToCanonicalCBOR v (AccountsOut era) where
  toCanonicalCBOR v (AccountDState d) = toCanonicalCBOR v d
  toCanonicalCBOR v (AccountPState p) =
     encodeAsMap
      [ mkEncodablePair v ("retiring" :: Text) (cpsRetiring p)
      , mkEncodablePair v ("vrf_keyhash" :: Text) (cpsVrfKeyHashes p)
      , mkEncodablePair v ("future_stake_pool_params" :: Text) (cpsFutureStakePoolParams p)
      ]

instance (Era era, NamespaceEra v ~ era, EraAccounts era) => FromCanonicalCBOR v (AccountsOut era)where
  fromCanonicalCBOR = do
    l <- decodeMapLenCanonical
    case l of
      3 -> fmap AccountPState <$> (do
        Versioned cpsRetiring <- decodeField @v "retiring"
        Versioned cpsVrfKeyHashes <- decodeField @v "vrf_keyhash"
        Versioned cpsFutureStakePoolParams <- decodeField @v "future_stake_pool_params"
        pure $ Versioned CanonicalPState {..})
      _ -> fmap AccountDState <$> fromCanonicalCBOR

data CanonicalStakePoolParams = CanonicalStakePoolParams
  { sppCost :: !CanonicalCoin
  , sppPledge :: !CanonicalCoin
  , sppMargin :: !UnitInterval
  , sppRelays :: !(StrictSeq.StrictSeq StakePoolRelay)
  , sppId :: !(KeyHash StakePool)
  , sppOwners :: !(Set (KeyHash Staking))
  , sppVrf :: !(CanonicalVRFVerKeyHash StakePoolVRF)
  , sppMetadata :: !(StrictMaybe PoolMetadata)
  , sppRewardAccount :: !CanonicalRewardAccount
  }
  deriving (Eq, Show, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalStakePoolParams where
  toCanonicalCBOR v CanonicalStakePoolParams {..} =
    encodeAsMap
      [ mkEncodablePair v ("cost" :: Text) sppCost
      , mkEncodablePair v ("pledge" :: Text) sppPledge
      , mkEncodablePair v ("margin" :: Text) sppMargin
      , mkEncodablePair v ("relays" :: Text) (toList sppRelays)
      , mkEncodablePair v ("operator" :: Text) sppId
      , mkEncodablePair v ("pool_owners" :: Text) sppOwners
      , mkEncodablePair v ("vrf_keyhash" :: Text) sppVrf
      , mkEncodablePair v ("pool_metadata" :: Text) sppMetadata
      , mkEncodablePair v ("reward_account" :: Text) sppRewardAccount
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalStakePoolParams where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 9
    Versioned sppCost <- decodeField @v "cost"
    Versioned sppMargin <- decodeField @v "margin"
    Versioned sppPledge <- decodeField @v "pledge"
    Versioned relaysList <- decodeField @v "relays"
    let sppRelays = StrictSeq.fromList relaysList
    Versioned sppId <- decodeField @v "operator"
    Versioned sppOwners <- decodeField @v "pool_owners"
    Versioned sppVrf <- decodeField @v "vrf_keyhash"
    Versioned sppMetadata <- decodeField @v "pool_metadata"
    Versioned sppRewardAccount <- decodeField @v "reward_account"
    pure $ Versioned CanonicalStakePoolParams {..}


decodeField :: forall v s a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

mkCanonicalStakePoolParams :: StakePoolParams -> CanonicalStakePoolParams
mkCanonicalStakePoolParams StakePoolParams {..} =
  CanonicalStakePoolParams
    { sppVrf = mkCanonicalVRFVerKeyHash sppVrf
    , sppMetadata = sppMetadata
    , sppRewardAccount = mkCanonicalRewardAccount sppAccountAddress
    , sppRelays = sppRelays
    , sppCost = CanonicalCoin (compactCoinOrError sppCost)
    , sppPledge = CanonicalCoin (compactCoinOrError sppPledge)
    , ..
    }

fromCanonicalStakePoolParams :: CanonicalStakePoolParams -> StakePoolParams
fromCanonicalStakePoolParams CanonicalStakePoolParams {..} =
  StakePoolParams
    { sppCost = fromCompact (unCoin sppCost)
    , sppPledge = fromCompact (unCoin sppPledge)
    , sppVrf = fromCanonicalVRFVerKeyHash sppVrf
    , sppAccountAddress = fromCanonicalRewardAccount sppRewardAccount
    , ..
    }

deriving via LedgerCBOR v PoolMetadata instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v PoolMetadata

deriving via LedgerCBOR v PoolMetadata instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v PoolMetadata

deriving via LedgerCBOR v StakePoolRelay instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v StakePoolRelay

deriving via LedgerCBOR v StakePoolRelay instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v StakePoolRelay

deriving via
  LedgerCBOR v (VRFVerKeyHash StakePoolVRF)
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (VRFVerKeyHash StakePoolVRF)

deriving via
  LedgerCBOR v (VRFVerKeyHash StakePoolVRF)
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v (VRFVerKeyHash StakePoolVRF)

deriving via
  LedgerCBOR v (DState era)
  instance
    (Era era, NamespaceEra v ~ era, EncCBOR (Accounts era)) => ToCanonicalCBOR v (DState era)

instance
  (Era era, NamespaceEra v ~ era, EraAccounts era) =>
  FromCanonicalCBOR v (DState era)
  where
  fromCanonicalCBOR =
    Versioned <$> assumeCanonicalDecoder (fromEraShareCBOR @era)

deriving via
  LedgerCBOR v (NonZero Word64)
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (NonZero Word64)

deriving via
  LedgerCBOR v (NonZero Word64)
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v (NonZero Word64)

toCanonicalPState :: PState era -> CanonicalPState
toCanonicalPState PState {..} =
  CanonicalPState
    { cpsRetiring = psRetiring
    , cpsVrfKeyHashes = psVRFKeyHashes
    , cpsFutureStakePoolParams = fmap mkCanonicalStakePoolParams psFutureStakePoolParams
    }

mkCanonicalPState :: PState era -> CanonicalPState
mkCanonicalPState = toCanonicalPState

type instance NamespaceKeySize "accounts/v0" = 1

instance
  ( NamespaceEra "accounts/v0" ~ era
  , EraAccounts era
  ) => KnownNamespace "accounts/v0" where
  type NamespaceKey "accounts/v0" = AccountsIn
  type NamespaceEntry "accounts/v0" = AccountsOut (NamespaceEra "accounts/v0")

instance
  (NamespaceEra "accounts/v0" ~ era, Era era, EncCBOR (Accounts era)) =>
  CanonicalCBOREntryEncoder "accounts/v0" (AccountsOut era)
  where
  encodeEntry n = toCanonicalCBOR (Proxy @"accounts/v0") n

instance
  (NamespaceEra "accounts/v0" ~ era, EraAccounts era) =>
  CanonicalCBOREntryDecoder "accounts/v0" (AccountsOut era)
  where
  decodeEntry = fromCanonicalCBOR
