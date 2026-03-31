{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 (
  EntitiesAccountsIn (..),
  EntitiesAccountsOut (..),
  CanonicalAccountState (..),
) where

import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalCoin, decodeNamespacedField)
import Cardano.Ledger.CanonicalState.LedgerCBOR (LedgerCBOR (LedgerCBOR))
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Core (KeyHash, KeyRole (StakePool), Staking)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State (DRep)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeMapLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

instance (Era era, NamespaceEra "entities/accounts/v0" ~ era) => KnownNamespace "entities/accounts/v0" where
  type NamespaceKey "entities/accounts/v0" = EntitiesAccountsIn
  type NamespaceEntry "entities/accounts/v0" = EntitiesAccountsOut

instance
  (Era era, NamespaceEra "entities/accounts/v0" ~ era) =>
  CanonicalCBOREntryEncoder "entities/accounts/v0" EntitiesAccountsOut
  where
  encodeEntry (EntitiesAccountsOut n) = toCanonicalCBOR (Proxy @"entities/accounts/v0") n

instance
  (Era era, NamespaceEra "entities/accounts/v0" ~ era) =>
  CanonicalCBOREntryDecoder "entities/accounts/v0" EntitiesAccountsOut
  where
  decodeEntry = fmap EntitiesAccountsOut <$> fromCanonicalCBOR

newtype EntitiesAccountsIn = EntitiesAccountsIn (Credential Staking)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/accounts/v0" = 29

instance IsKey EntitiesAccountsIn where
  keySize = namespaceKeySize @"entities/accounts/v0"
  packKeyM (EntitiesAccountsIn accountCredential) =
    packM accountCredential
  unpackKeyM =
    EntitiesAccountsIn <$> unpackM

newtype EntitiesAccountsOut
  = EntitiesAccountsOut CanonicalAccountState
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/accounts/v0" CanonicalAccountState =>
  ToCanonicalCBOR "entities/accounts/v0" EntitiesAccountsOut

deriving instance
  FromCanonicalCBOR "entities/accounts/v0" CanonicalAccountState =>
  FromCanonicalCBOR "entities/accounts/v0" EntitiesAccountsOut

data CanonicalAccountState = CanonicalAccountState
  { casBalance :: CanonicalCoin
  , casDeposit :: CanonicalCoin
  , casDRepDelegation :: Maybe DRep
  , casStakePoolDelegation :: Maybe (KeyHash StakePool)
  }
  deriving (Eq, Show, Generic)

instance
  (Era era, NamespaceEra "entities/accounts/v0" ~ era) =>
  ToCanonicalCBOR "entities/accounts/v0" CanonicalAccountState
  where
  toCanonicalCBOR v CanonicalAccountState {..} =
    encodeAsMap
      [ mkEncodablePair v ("balance" :: Text) casBalance
      , mkEncodablePair v ("deposit" :: Text) casDeposit
      , mkEncodablePair v ("drep_delegation" :: Text) casDRepDelegation
      , mkEncodablePair v ("stake_pool_delegation" :: Text) casStakePoolDelegation
      ]

instance (Era era, NamespaceEra "entities/accounts/v0" ~ era) => FromCanonicalCBOR v CanonicalAccountState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 4
    Versioned casBalance <- decodeNamespacedField @"entities/accounts/v0" ("balance" :: Text)
    Versioned casDeposit <- decodeNamespacedField @"entities/accounts/v0" ("deposit" :: Text)
    Versioned casDRepDelegation <-
      decodeNamespacedField @"entities/accounts/v0" ("drep_delegation" :: Text)
    Versioned casStakePoolDelegation <-
      decodeNamespacedField @"entities/accounts/v0" ("stake_pool_delegation" :: Text)

    pure $ Versioned $ CanonicalAccountState {..}

deriving via
  LedgerCBOR v DRep
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v DRep

deriving via
  LedgerCBOR v DRep
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v DRep
