{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovPParams (
  GovPParamsIn (..),
  GovPParamsOut (..),
) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits)
import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds,
  PoolVotingThresholds,
  THKD (..),
 )
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Core (PParams (..), PParamsUpdate (..))
import Cardano.Ledger.HKD
import Cardano.Ledger.Plutus.CostModels (CostModels, flattenCostModels, mkCostModelsLenient)
import Cardano.SCLS.CBOR.Canonical
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Cardano.SCLS.Versioned (Versioned (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.MemPack
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

data GovPParamsIn
  = GovPParamsInPrev
  | GovPParamsInCurr
  | GovPParamsInPossibleFuture
  | GovPParamsInDefiniteFuture
  deriving (Eq, Ord, Show)

instance IsKey GovPParamsIn where
  keySize = namespaceKeySize @"gov/pparams/v0"
  packKeyM GovPParamsInPrev =
    packByteStringM "prev"
  packKeyM GovPParamsInCurr = packByteStringM "curr"
  packKeyM GovPParamsInPossibleFuture = packByteStringM "fut0"
  packKeyM GovPParamsInDefiniteFuture = packByteStringM "fut1"
  unpackKeyM = do
    tag :: ByteString <- unpackByteStringM 4
    case tag of
      _
        | tag == "prev" -> return GovPParamsInPrev
        | tag == "curr" -> return GovPParamsInCurr
        | tag == "fut0" -> return GovPParamsInPossibleFuture
        | tag == "fut1" -> return GovPParamsInDefiniteFuture
        | otherwise -> fail "Invalid GovPParamsIn tag"

instance ToCanonicalCBOR v CostModels where
  toCanonicalCBOR v = toCanonicalCBOR v . flattenCostModels

instance FromCanonicalCBOR v CostModels where
  fromCanonicalCBOR = do
    Versioned v <- fromCanonicalCBOR
    Versioned <$> mkCostModelsLenient v

deriving via LedgerCBOR v PoolVotingThresholds instance ToCanonicalCBOR v PoolVotingThresholds

deriving via
  LedgerCBOR v PoolVotingThresholds
  instance
    FromCanonicalCBOR v PoolVotingThresholds

deriving via LedgerCBOR v DRepVotingThresholds instance ToCanonicalCBOR v DRepVotingThresholds

deriving via
  LedgerCBOR v DRepVotingThresholds
  instance
    FromCanonicalCBOR v DRepVotingThresholds

deriving via LedgerCBOR v CoinPerByte instance ToCanonicalCBOR v CoinPerByte

deriving via LedgerCBOR v CoinPerByte instance FromCanonicalCBOR v CoinPerByte

deriving via LedgerCBOR v OrdExUnits instance ToCanonicalCBOR v OrdExUnits

deriving via LedgerCBOR v OrdExUnits instance FromCanonicalCBOR v OrdExUnits

newtype GovPParamsOut = GovPParamsOut (PParams ConwayEra)
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v GovPParamsOut

deriving newtype instance FromCanonicalCBOR v GovPParamsOut

instance ToCanonicalCBOR v (PParams ConwayEra) where
  toCanonicalCBOR v (PParams ConwayPParams {..}) =
    encodeAsMap
      [ mkEncodablePair v ("a0" :: Text) (unTHKD cppA0)
      , mkEncodablePair v ("rho" :: Text) (unTHKD cppRho)
      , mkEncodablePair v ("tau" :: Text) (unTHKD cppTau)
      , mkEncodablePair v ("n_opt" :: Text) (unTHKD cppNOpt)
      , mkEncodablePair v ("prices" :: Text) (unTHKD cppPrices)
      , mkEncodablePair v ("epoch_max" :: Text) (unTHKD cppEMax)
      , mkEncodablePair v ("min_fee_a" :: Text) (unTHKD cppMinFeeA)
      , mkEncodablePair v ("min_fee_b" :: Text) (unTHKD cppMinFeeB)
      , mkEncodablePair v ("cost_models" :: Text) (unTHKD cppCostModels)
      , mkEncodablePair v ("key_deposit" :: Text) (unTHKD cppKeyDeposit)
      , mkEncodablePair v ("max_tx_size" :: Text) (unTHKD cppMaxTxSize)
      , mkEncodablePair v ("drep_deposit" :: Text) (unTHKD cppDRepDeposit)
      , mkEncodablePair v ("max_val_size" :: Text) (unTHKD cppMaxValSize)
      , mkEncodablePair v ("pool_deposit" :: Text) (unTHKD cppPoolDeposit)
      , mkEncodablePair v ("drep_activity" :: Text) (unTHKD cppDRepActivity)
      , mkEncodablePair v ("min_pool_cost" :: Text) (unTHKD cppMinPoolCost)
      , mkEncodablePair v ("max_block_size" :: Text) (unTHKD cppMaxBBSize)
      , mkEncodablePair v ("max_tx_ex_units" :: Text) (unTHKD cppMaxTxExUnits)
      , mkEncodablePair v ("protocol_version" :: Text) (cppProtocolVersion)
      , mkEncodablePair v ("coin_per_utxo_byte" :: Text) (unTHKD cppCoinsPerUTxOByte)
      , mkEncodablePair v ("gov_action_deposit" :: Text) (unTHKD cppGovActionDeposit)
      , mkEncodablePair v ("max_block_ex_units" :: Text) (unTHKD cppMaxBlockExUnits)
      , mkEncodablePair v ("min_committee_size" :: Text) (unTHKD cppCommitteeMinSize)
      , mkEncodablePair v ("gov_action_lifetime" :: Text) (unTHKD cppGovActionLifetime)
      , mkEncodablePair v ("committee_term_limit" :: Text) (unTHKD cppCommitteeMaxTermLength)
      , mkEncodablePair v ("collateral_percentage" :: Text) (unTHKD cppCollateralPercentage)
      , mkEncodablePair v ("max_block_header_size" :: Text) (unTHKD cppMaxBHSize)
      , mkEncodablePair v ("max_collateral_inputs" :: Text) (unTHKD cppMaxCollateralInputs)
      , mkEncodablePair v ("drep_voting_thresholds" :: Text) (unTHKD cppDRepVotingThresholds)
      , mkEncodablePair v ("pool_voting_thresholds" :: Text) (unTHKD cppPoolVotingThresholds)
      , mkEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          (unTHKD cppMinFeeRefScriptCostPerByte)
      ]

instance FromCanonicalCBOR v (PParams ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 31
    Versioned (THKD -> cppA0) <- decodeField "a0"
    Versioned (THKD -> cppRho) <- decodeField "rho"
    Versioned (THKD -> cppTau) <- decodeField "tau"
    Versioned (THKD -> cppNOpt) <- decodeField "n_opt"
    Versioned (THKD -> cppPrices) <- decodeField "prices"
    Versioned (THKD -> cppEMax) <- decodeField "epoch_max"
    Versioned (THKD -> cppMinFeeA) <- decodeField "min_fee_a"
    Versioned (THKD -> cppMinFeeB) <- decodeField "min_fee_b"
    Versioned (THKD -> cppCostModels) <- decodeField "cost_models"
    Versioned (THKD -> cppKeyDeposit) <- decodeField "key_deposit"
    Versioned (THKD -> cppMaxTxSize) <- decodeField "max_tx_size"
    Versioned (THKD -> cppDRepDeposit) <- decodeField "drep_deposit"
    Versioned (THKD -> cppMaxValSize) <- decodeField "max_val_size"
    Versioned (THKD -> cppPoolDeposit) <- decodeField "pool_deposit"
    Versioned (THKD -> cppDRepActivity) <- decodeField "drep_activity"
    Versioned (THKD -> cppMinPoolCost) <- decodeField "min_pool_cost"
    Versioned (THKD -> cppMaxBBSize) <- decodeField "max_block_size"
    Versioned (THKD -> cppMaxTxExUnits) <- decodeField "max_tx_ex_units"
    Versioned (cppProtocolVersion) <- decodeField "protocol_version"
    Versioned (THKD -> cppCoinsPerUTxOByte) <- decodeField "coin_per_utxo_byte"
    Versioned (THKD -> cppGovActionDeposit) <- decodeField "gov_action_deposit"
    Versioned (THKD -> cppMaxBlockExUnits) <- decodeField "max_block_ex_units"
    Versioned (THKD -> cppCommitteeMinSize) <- decodeField "min_committee_size"
    Versioned (THKD -> cppGovActionLifetime) <- decodeField "gov_action_lifetime"
    Versioned (THKD -> cppCommitteeMaxTermLength) <- decodeField "committee_term_limit"
    Versioned (THKD -> cppCollateralPercentage) <- decodeField "collateral_percentage"
    Versioned (THKD -> cppMaxBHSize) <- decodeField "max_block_header_size"
    Versioned (THKD -> cppMaxCollateralInputs) <- decodeField "max_collateral_inputs"
    Versioned (THKD -> cppDRepVotingThresholds) <- decodeField "drep_voting_thresholds"
    Versioned (THKD -> cppPoolVotingThresholds) <- decodeField "pool_voting_thresholds"
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_ref_script_cost_per_byte"
    pure $ Versioned $ PParams ConwayPParams {..}

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

instance ToCanonicalCBOR v (PParamsUpdate ConwayEra) where
  toCanonicalCBOR v (PParamsUpdate ConwayPParams {..}) =
    encodeAsMap
      [ mkEncodablePair v ("a0" :: Text) (unTHKD cppA0)
      , mkEncodablePair v ("rho" :: Text) (unTHKD cppRho)
      , mkEncodablePair v ("tau" :: Text) (unTHKD cppTau)
      , mkEncodablePair v ("n_opt" :: Text) (unTHKD cppNOpt)
      , mkEncodablePair v ("prices" :: Text) (unTHKD cppPrices)
      , mkEncodablePair v ("epoch_max" :: Text) (unTHKD cppEMax)
      , mkEncodablePair v ("min_fee_a" :: Text) (unTHKD cppMinFeeA)
      , mkEncodablePair v ("min_fee_b" :: Text) (unTHKD cppMinFeeB)
      , mkEncodablePair v ("cost_models" :: Text) (unTHKD cppCostModels)
      , mkEncodablePair v ("key_deposit" :: Text) (unTHKD cppKeyDeposit)
      , mkEncodablePair v ("max_tx_size" :: Text) (unTHKD cppMaxTxSize)
      , mkEncodablePair v ("drep_deposit" :: Text) (unTHKD cppDRepDeposit)
      , mkEncodablePair v ("pool_deposit" :: Text) (unTHKD cppPoolDeposit)
      , mkEncodablePair v ("max_val_size" :: Text) (unTHKD cppMaxValSize)
      , mkEncodablePair v ("drep_activity" :: Text) (unTHKD cppDRepActivity)
      , mkEncodablePair v ("min_pool_cost" :: Text) (unTHKD cppMinPoolCost)
      , mkEncodablePair v ("max_block_size" :: Text) (unTHKD cppMaxBBSize)
      , mkEncodablePair v ("max_tx_ex_units" :: Text) (unTHKD cppMaxTxExUnits)
      , mkEncodablePair v ("coin_per_utxo_byte" :: Text) (unTHKD cppCoinsPerUTxOByte)
      , mkEncodablePair v ("gov_action_deposit" :: Text) (unTHKD cppGovActionDeposit)
      , mkEncodablePair v ("max_block_ex_units" :: Text) (unTHKD cppMaxBlockExUnits)
      , mkEncodablePair v ("min_committee_size" :: Text) (unTHKD cppCommitteeMinSize)
      , mkEncodablePair v ("committee_term_limit" :: Text) (unTHKD cppCommitteeMaxTermLength)
      , mkEncodablePair v ("collateral_percentage" :: Text) (unTHKD cppCollateralPercentage)
      , mkEncodablePair v ("drep_voting_thresholds" :: Text) (unTHKD cppDRepVotingThresholds)
      , mkEncodablePair v ("gov_action_lifetime" :: Text) (unTHKD cppGovActionLifetime)
      , mkEncodablePair v ("max_block_header_size" :: Text) (unTHKD cppMaxBHSize)
      , mkEncodablePair v ("max_collateral_inputs" :: Text) (unTHKD cppMaxCollateralInputs)
      , mkEncodablePair v ("pool_voting_thresholds" :: Text) (unTHKD cppPoolVotingThresholds)
      , mkEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          (unTHKD cppMinFeeRefScriptCostPerByte)
      ]

instance FromCanonicalCBOR v (PParamsUpdate ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 30
    Versioned (THKD -> cppA0) <- decodeField "a0"
    Versioned (THKD -> cppRho) <- decodeField "rho"
    Versioned (THKD -> cppTau) <- decodeField "tau"
    Versioned (THKD -> cppNOpt) <- decodeField "n_opt"
    Versioned (THKD -> cppPrices) <- decodeField "prices"
    Versioned (THKD -> cppEMax) <- decodeField "epoch_max"
    Versioned (THKD -> cppMinFeeA) <- decodeField "min_fee_a"
    Versioned (THKD -> cppMinFeeB) <- decodeField "min_fee_b"
    Versioned (THKD -> cppCostModels) <- decodeField "cost_models"
    Versioned (THKD -> cppKeyDeposit) <- decodeField "key_deposit"
    Versioned (THKD -> cppMaxTxSize) <- decodeField "max_tx_size"
    Versioned (THKD -> cppDRepDeposit) <- decodeField "drep_deposit"
    Versioned (THKD -> cppMaxValSize) <- decodeField "max_val_size"
    Versioned (THKD -> cppPoolDeposit) <- decodeField "pool_deposit"
    Versioned (THKD -> cppDRepActivity) <- decodeField "drep_activity"
    Versioned (THKD -> cppMinPoolCost) <- decodeField "min_pool_cost"
    Versioned (THKD -> cppMaxBBSize) <- decodeField "max_block_size"
    Versioned (THKD -> cppMaxTxExUnits) <- decodeField "max_tx_ex_units"
    Versioned (THKD -> cppCoinsPerUTxOByte) <- decodeField "coin_per_utxo_byte"
    Versioned (THKD -> cppGovActionDeposit) <- decodeField "gov_action_deposit"
    Versioned (THKD -> cppMaxBlockExUnits) <- decodeField "max_block_ex_units"
    Versioned (THKD -> cppCommitteeMinSize) <- decodeField "min_committee_size"
    Versioned (THKD -> cppGovActionLifetime) <- decodeField "gov_action_lifetime"
    Versioned (THKD -> cppCommitteeMaxTermLength) <- decodeField "committee_term_limit"
    Versioned (THKD -> cppCollateralPercentage) <- decodeField "collateral_percentage"
    Versioned (THKD -> cppMaxBHSize) <- decodeField "max_block_header_size"
    Versioned (THKD -> cppMaxCollateralInputs) <- decodeField "max_collateral_inputs"
    Versioned (THKD -> cppDRepVotingThresholds) <- decodeField "drep_voting_thresholds"
    Versioned (THKD -> cppPoolVotingThresholds) <- decodeField "pool_voting_thresholds"
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_ref_script_cost_per_byte"
    let cppProtocolVersion = NoUpdate
    pure $ Versioned $ PParamsUpdate ConwayPParams {..}

type instance NamespaceKeySize "gov/pparams/v0" = 4

instance KnownNamespace "gov/pparams/v0" where
  type NamespaceKey "gov/pparams/v0" = GovPParamsIn
  type NamespaceEntry "gov/pparams/v0" = GovPParamsOut

instance CanonicalCBOREntryEncoder "gov/pparams/v0" GovPParamsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/pparams/v0") n

instance CanonicalCBOREntryDecoder "gov/pparams/v0" GovPParamsOut where
  decodeEntry = fromCanonicalCBOR
