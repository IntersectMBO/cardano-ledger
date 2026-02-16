{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 (
  GovPParamsIn (..),
  GovPParamsOut (..),
  CanonicalPrices (..),
  mkCanonicalPrices,
  fromCanonicalPrices,
) where

import Cardano.Ledger.BaseTypes (NonNegativeInterval)
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.CanonicalState.LedgerCBOR (LedgerCBOR (..))
import Cardano.Ledger.CanonicalState.Namespace (NamespaceEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.CostModels (
  CostModels,
  flattenCostModels,
  mkCostModelsLenient,
 )
import Cardano.Ledger.Plutus.ExUnits (Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.MapExtras (boundedEnumMap, lookupMapFail)
import Data.MemPack (packByteStringM, unpackByteStringM)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

data GovPParamsIn
  = GovPParamsInPrev
  | GovPParamsInCurr
  | GovPParamsInPossibleFuture
  | GovPParamsInDefiniteFuture
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

keyGovPParamsIn :: GovPParamsIn -> ByteString
keyGovPParamsIn = \case
  GovPParamsInPrev -> "prev"
  GovPParamsInCurr -> "curr"
  GovPParamsInPossibleFuture -> "fut0"
  GovPParamsInDefiniteFuture -> "fut1"

mapGovPParamsIn :: Map ByteString GovPParamsIn
mapGovPParamsIn = boundedEnumMap keyGovPParamsIn

instance IsKey GovPParamsIn where
  keySize = namespaceKeySize @"gov/pparams/v0"
  packKeyM GovPParamsInPrev =
    packByteStringM "prev"
  packKeyM GovPParamsInCurr = packByteStringM "curr"
  packKeyM GovPParamsInPossibleFuture = packByteStringM "fut0"
  packKeyM GovPParamsInDefiniteFuture = packByteStringM "fut1"
  unpackKeyM = do
    tag :: ByteString <- unpackByteStringM 4
    lookupMapFail "GovPParamsIn tag" mapGovPParamsIn tag

instance ToCanonicalCBOR v CostModels where
  toCanonicalCBOR v = toCanonicalCBOR v . flattenCostModels

instance FromCanonicalCBOR v CostModels where
  fromCanonicalCBOR = do
    Versioned flattened <- fromCanonicalCBOR
    Versioned <$> mkCostModelsLenient flattened

data CanonicalPrices = CanonicalPrices
  { prMem :: !NonNegativeInterval
  , prSteps :: !NonNegativeInterval
  }
  deriving (Eq, Show, Generic)

mkCanonicalPrices :: Prices -> CanonicalPrices
mkCanonicalPrices Prices {..} = CanonicalPrices {..}

fromCanonicalPrices :: CanonicalPrices -> Prices
fromCanonicalPrices CanonicalPrices {..} = Prices {..}

instance (NamespaceEra v ~ era, Era era) => ToCanonicalCBOR v CanonicalPrices where
  toCanonicalCBOR v CanonicalPrices {..} = toCanonicalCBOR v (prMem, prSteps)

instance (NamespaceEra v ~ era, Era era) => FromCanonicalCBOR v CanonicalPrices where
  fromCanonicalCBOR = do
    Versioned (prMem, prSteps) <- fromCanonicalCBOR @v
    return $ Versioned CanonicalPrices {..}

deriving via
  LedgerCBOR v Language
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v Language

deriving via
  LedgerCBOR v Language
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v Language

newtype GovPParamsOut era = GovPParamsOut (PParams era)
  deriving (Generic)

deriving instance Eq (PParams era) => Eq (GovPParamsOut era)

deriving instance Show (PParams era) => Show (GovPParamsOut era)

deriving newtype instance
  ToCanonicalCBOR "gov/pparams/v0" (PParams era) =>
  ToCanonicalCBOR "gov/pparams/v0" (GovPParamsOut era)

deriving newtype instance
  FromCanonicalCBOR "gov/pparams/v0" (PParams era) =>
  FromCanonicalCBOR "gov/pparams/v0" (GovPParamsOut era)

type instance NamespaceKeySize "gov/pparams/v0" = 4

instance
  ( NamespaceEra "gov/pparams/v0" ~ era
  , Era era
  , FromCanonicalCBOR "gov/pparams/v0" (PParams era)
  , ToCanonicalCBOR "gov/pparams/v0" (PParams era)
  ) =>
  KnownNamespace "gov/pparams/v0"
  where
  type NamespaceKey "gov/pparams/v0" = GovPParamsIn
  type NamespaceEntry "gov/pparams/v0" = GovPParamsOut (NamespaceEra "gov/pparams/v0")

instance
  (NamespaceEra "gov/pparams/v0" ~ era, Era era, ToCanonicalCBOR "gov/pparams/v0" (PParams era)) =>
  CanonicalCBOREntryEncoder "gov/pparams/v0" (GovPParamsOut era)
  where
  encodeEntry (GovPParamsOut n) = toCanonicalCBOR (Proxy @"gov/pparams/v0") n

instance
  (NamespaceEra "gov/pparams/v0" ~ era, Era era, FromCanonicalCBOR "gov/pparams/v0" (PParams era)) =>
  CanonicalCBOREntryDecoder "gov/pparams/v0" (GovPParamsOut era)
  where
  decodeEntry = fmap GovPParamsOut <$> fromCanonicalCBOR
