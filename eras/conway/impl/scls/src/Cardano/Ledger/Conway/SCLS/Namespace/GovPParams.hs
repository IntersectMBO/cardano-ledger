{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
  CanonicalPParams (..),
  mkCanonicalPParams,
  fromCanonicalPParams,
  CanonicalCostModel (..),
  mkCanonicalCostModel,
  fromCanonicalCostModel,
  CanonicalCostModels (..),
  mkCanonicalCostModels,
  fromCanonicalCostModels,
  CanonicalPrices (..),
  mkCanonicalPrices,
  fromCanonicalPrices,
  CanonicalDRepVotingThresholds (..),
  mkCanonicalDRepVotingThresholds,
  fromCanonicalDRepVotingThresholds,
  CanonicalPoolVotingThresholds (..),
  mkCanonicalPoolVotingThresholds,
  fromCanonicalPoolVotingThresholds,
  CanonicalExUnits (..),
  mkCanonicalExUnits,
  fromCanonicalExUnits
) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
-- FIXME: CompactForm Coin -> Coin

import Cardano.Ledger.BaseTypes (NonNegativeInterval, ProtVer (..), UnitInterval)
import Cardano.Ledger.Coin (Coin, CoinPerByte (..))
import Cardano.Ledger.Compactible (CompactForm)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  THKD (..),
 )
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Core (PParams (..))
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  CostModels,
  costModelsUnknown,
  costModelsValid,
  flattenCostModels,
  getCostModelLanguage,
  getCostModelParams,
  mkCostModel,
  mkCostModels,
  mkCostModelsLenient,
 )
import Cardano.Ledger.Plutus.ExUnits (Prices (..), ExUnits (..), ExUnits' (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.SCLS.CBOR.Canonical (
  CanonicalDecoder (..),
  assumeCanonicalDecoder,
  assumeCanonicalEncoding,
 )
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
import Cardano.Slotting.Slot (EpochInterval)
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.MemPack (packByteStringM, unpackByteStringM)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)
import GHC.Natural

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

data CanonicalCostModel = CanonicalCostModel
  { cmLanguage :: !Language
  , cmValues :: ![Int64]
  }
  deriving (Eq, Show, Generic)

mkCanonicalCostModel :: CostModel -> CanonicalCostModel
mkCanonicalCostModel costModel =
  CanonicalCostModel
    { cmLanguage = getCostModelLanguage costModel
    , cmValues = getCostModelParams costModel
    }

fromCanonicalCostModel :: CanonicalCostModel -> CostModel
fromCanonicalCostModel CanonicalCostModel {..} =
  case mkCostModel cmLanguage cmValues of
    Right costModel -> costModel
    Left err -> error $ "CanonicalCostModel: " <> show err

instance ToCanonicalCBOR v CanonicalCostModel where
  toCanonicalCBOR v CanonicalCostModel {..} =
    toCanonicalCBOR v (cmLanguage, cmValues)

instance FromCanonicalCBOR v CanonicalCostModel where
  fromCanonicalCBOR = do
    Versioned (cmLanguage, cmValues) <- fromCanonicalCBOR
    return $! Versioned CanonicalCostModel {..}

data CanonicalCostModels = CanonicalCostModels
  { _costModelsValid :: !(Map.Map Language CostModel)
  , _costModelsUnknown :: !(Map.Map Word8 [Int64])
  }
  deriving (Eq, Show, Generic)

mkCanonicalCostModels :: CostModels -> CanonicalCostModels
mkCanonicalCostModels costModels =
  CanonicalCostModels
    { _costModelsValid = costModelsValid costModels
    , _costModelsUnknown = costModelsUnknown costModels
    }

fromCanonicalCostModels :: CanonicalCostModels -> CostModels
fromCanonicalCostModels CanonicalCostModels {..} = case mkCostModelsLenient flattened of
  Nothing -> error "fromCanonicalCostModels<CostModels> can't parse model"
  Just s -> s
  where
    flattened =
      Map.union
        (flattenCostModels (mkCostModels _costModelsValid))
        _costModelsUnknown

instance ToCanonicalCBOR v CanonicalCostModels where
  toCanonicalCBOR v CanonicalCostModels {..} =
    toCanonicalCBOR v flattened
    where
      flattened =
        Map.union
          (flattenCostModels (mkCostModels _costModelsValid))
          _costModelsUnknown

instance FromCanonicalCBOR v CanonicalCostModels where
  fromCanonicalCBOR = do
    Versioned flattened <- fromCanonicalCBOR
    costModels <- mkCostModelsLenient flattened
    return $ Versioned $ mkCanonicalCostModels costModels

data CanonicalPrices = CanonicalPrices
  { prMem :: !NonNegativeInterval
  , prSteps :: !NonNegativeInterval
  }
  deriving (Eq, Show, Generic)

mkCanonicalPrices :: Prices -> CanonicalPrices
mkCanonicalPrices Prices {..} = CanonicalPrices {..}

fromCanonicalPrices :: CanonicalPrices -> Prices
fromCanonicalPrices CanonicalPrices {..} = Prices {..}

instance ToCanonicalCBOR v CanonicalPrices where
  toCanonicalCBOR v CanonicalPrices {..} = toCanonicalCBOR v (prMem, prSteps)

instance FromCanonicalCBOR v CanonicalPrices where
  fromCanonicalCBOR = do
    Versioned (prMem, prSteps) <- fromCanonicalCBOR
    return $ Versioned CanonicalPrices {..}

data CanonicalPoolVotingThresholds
  = CanonicalPoolVotingThresholds
  { pvtMotionNoConfidence :: !UnitInterval
  , pvtCommitteeNormal :: !UnitInterval
  , pvtCommitteeNoConfidence :: !UnitInterval
  , pvtHardForkInitiation :: !UnitInterval
  , pvtPPSecurityGroup :: !UnitInterval
  }
  deriving (Show, Eq, Generic)

fromCanonicalPoolVotingThresholds :: CanonicalPoolVotingThresholds -> PoolVotingThresholds
fromCanonicalPoolVotingThresholds CanonicalPoolVotingThresholds {..} = PoolVotingThresholds {..}

mkCanonicalPoolVotingThresholds :: PoolVotingThresholds -> CanonicalPoolVotingThresholds
mkCanonicalPoolVotingThresholds PoolVotingThresholds {..} = CanonicalPoolVotingThresholds {..}

instance ToCanonicalCBOR v CanonicalPoolVotingThresholds where
  toCanonicalCBOR v CanonicalPoolVotingThresholds {..} =
    toCanonicalCBOR
      v
      ( pvtMotionNoConfidence
      , pvtCommitteeNormal
      , pvtCommitteeNoConfidence
      , pvtHardForkInitiation
      , pvtPPSecurityGroup
      )

instance FromCanonicalCBOR v CanonicalPoolVotingThresholds where
  fromCanonicalCBOR = do
    Versioned
      ( pvtMotionNoConfidence
        , pvtCommitteeNormal
        , pvtCommitteeNoConfidence
        , pvtHardForkInitiation
        , pvtPPSecurityGroup
        ) <-
      fromCanonicalCBOR
    return $ Versioned CanonicalPoolVotingThresholds {..}

data CanonicalDRepVotingThresholds = CanonicalDRepVotingThresholds
  { dvtMotionNoConfidence :: !UnitInterval
  , dvtCommitteeNormal :: !UnitInterval
  , dvtCommitteeNoConfidence :: !UnitInterval
  , dvtUpdateToConstitution :: !UnitInterval
  , dvtHardForkInitiation :: !UnitInterval
  , dvtPPNetworkGroup :: !UnitInterval
  , dvtPPEconomicGroup :: !UnitInterval
  , dvtPPTechnicalGroup :: !UnitInterval
  , dvtPPGovGroup :: !UnitInterval
  , dvtTreasuryWithdrawal :: !UnitInterval
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalDRepVotingThresholds where
  toCanonicalCBOR v CanonicalDRepVotingThresholds {..} = do
    assumeCanonicalEncoding (E.encodeListLen 10)
      <> toCanonicalCBOR v dvtMotionNoConfidence
      <> toCanonicalCBOR v dvtCommitteeNormal
      <> toCanonicalCBOR v dvtCommitteeNoConfidence
      <> toCanonicalCBOR v dvtUpdateToConstitution
      <> toCanonicalCBOR v dvtHardForkInitiation
      <> toCanonicalCBOR v dvtPPNetworkGroup
      <> toCanonicalCBOR v dvtPPEconomicGroup
      <> toCanonicalCBOR v dvtPPTechnicalGroup
      <> toCanonicalCBOR v dvtPPGovGroup
      <> toCanonicalCBOR v dvtTreasuryWithdrawal

-- toCanonicalCBOR v
--   ( dvtMotionNoConfidence
--   , dvtCommitteeNormal
--   , dvtCommitteeNoConfidence
--   , dvtUpdateToConstitution
--   , dvtHardForkInitiation
--   , dvtPPNetworkGroup
--   , dvtPPEconomicGroup
--   , dvtPPTechnicalGroup
--   , dvtPPGovGroup
--   , dvtTreasuryWithdrawal
--   )

instance FromCanonicalCBOR v CanonicalDRepVotingThresholds where
  fromCanonicalCBOR = do
    -- Versioned
    --   ( dvtMotionNoConfidence
    --     , dvtCommitteeNormal
    --     , dvtCommitteeNoConfidence
    --     , dvtUpdateToConstitution
    --     , dvtHardForkInitiation
    --     , dvtPPNetworkGroup
    --     , dvtPPEconomicGroup
    --     , dvtPPTechnicalGroup
    --     , dvtPPGovGroup
    --     , dvtTreasuryWithdrawal) <- fromCanonicalCBOR
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 10
    Versioned dvtMotionNoConfidence <- fromCanonicalCBOR @v
    Versioned dvtCommitteeNormal <- fromCanonicalCBOR @v
    Versioned dvtCommitteeNoConfidence <- fromCanonicalCBOR @v
    Versioned dvtUpdateToConstitution <- fromCanonicalCBOR @v
    Versioned dvtHardForkInitiation <- fromCanonicalCBOR @v
    Versioned dvtPPNetworkGroup <- fromCanonicalCBOR @v
    Versioned dvtPPEconomicGroup <- fromCanonicalCBOR @v
    Versioned dvtPPTechnicalGroup <- fromCanonicalCBOR @v
    Versioned dvtPPGovGroup <- fromCanonicalCBOR @v
    Versioned dvtTreasuryWithdrawal <- fromCanonicalCBOR @v
    return $ Versioned CanonicalDRepVotingThresholds {..}

mkCanonicalDRepVotingThresholds :: DRepVotingThresholds -> CanonicalDRepVotingThresholds
mkCanonicalDRepVotingThresholds DRepVotingThresholds {..} = CanonicalDRepVotingThresholds {..}

fromCanonicalDRepVotingThresholds :: CanonicalDRepVotingThresholds -> DRepVotingThresholds
fromCanonicalDRepVotingThresholds CanonicalDRepVotingThresholds {..} = DRepVotingThresholds {..}

deriving via LedgerCBOR v Language instance FromCanonicalCBOR v Language

deriving via LedgerCBOR v Language instance ToCanonicalCBOR v Language

newtype GovPParamsOut = GovPParamsOut (PParams ConwayEra)
  deriving (Eq, Show)

data CanonicalPParams = CanonicalPParams
  { ccppA0 :: NonNegativeInterval
  , ccppTxFeePerByte :: CompactForm Coin
  , ccppTxFeeFixed :: CompactForm Coin
  , ccppMaxBBSize :: Word32
  , ccppMaxTxSize :: Word32
  , ccppMaxBHSize :: Word16
  , ccppKeyDeposit :: CompactForm Coin
  , ccppPoolDeposit :: CompactForm Coin
  , ccppEMax :: EpochInterval
  , ccppNOpt :: Word16
  , ccppRho :: UnitInterval
  , ccppTau :: UnitInterval
  , ccppProtocolVersion :: ProtVer
  , ccppMinPoolCost :: CompactForm Coin
  , ccppCoinsPerUTxOByte :: CompactForm Coin
  , ccppCostModels :: CanonicalCostModels
  , ccppPrices :: CanonicalPrices
  , ccppMaxTxExUnits :: CanonicalExUnits
  , ccppMaxBlockExUnits :: CanonicalExUnits
  , ccppMaxValSize :: Word32
  , ccppCollateralPercentage :: Word16
  , ccppMaxCollateralInputs :: Word16
  , ccppPoolVotingThresholds :: CanonicalPoolVotingThresholds
  , ccppDRepVotingThresholds :: CanonicalDRepVotingThresholds
  , ccppCommitteeMinSize :: Word16
  , ccppCommitteeMaxTermLength :: EpochInterval
  , ccppGovActionLifetime :: EpochInterval
  , ccppGovActionDeposit :: CompactForm Coin
  , ccppDRepDeposit :: CompactForm Coin
  , ccppDRepActivity :: EpochInterval
  , ccppMinFeeRefScriptCostPerByte :: NonNegativeInterval
  }
  deriving (Eq, Show, Generic)

mkCanonicalPParams :: PParams ConwayEra -> CanonicalPParams
mkCanonicalPParams (PParams ConwayPParams {..}) =
  CanonicalPParams
    { ccppA0 = unTHKD cppA0
    , ccppTxFeePerByte = unCoinPerByte $ unTHKD cppTxFeePerByte
    , ccppTxFeeFixed = unTHKD cppTxFeeFixed
    , ccppMaxBBSize = unTHKD cppMaxBBSize
    , ccppMaxTxSize = unTHKD cppMaxTxSize
    , ccppMaxBHSize = unTHKD cppMaxBHSize
    , ccppKeyDeposit = unTHKD cppKeyDeposit
    , ccppPoolDeposit = unTHKD cppPoolDeposit
    , ccppEMax = unTHKD cppEMax
    , ccppNOpt = unTHKD cppNOpt
    , ccppRho = unTHKD cppRho
    , ccppTau = unTHKD cppTau
    , ccppProtocolVersion = cppProtocolVersion
    , ccppMinPoolCost = unTHKD cppMinPoolCost
    , ccppCoinsPerUTxOByte = unCoinPerByte $ unTHKD cppCoinsPerUTxOByte
    , ccppCostModels = mkCanonicalCostModels $ unTHKD cppCostModels
    , ccppPrices = mkCanonicalPrices $ unTHKD cppPrices
    , ccppMaxTxExUnits = unTHKD cppMaxTxExUnits & unOrdExUnits & mkCanonicalExUnits
    , ccppMaxBlockExUnits = unTHKD cppMaxBlockExUnits & unOrdExUnits & mkCanonicalExUnits
    , ccppMaxValSize = unTHKD cppMaxValSize
    , ccppCollateralPercentage = unTHKD cppCollateralPercentage
    , ccppMaxCollateralInputs = unTHKD cppMaxCollateralInputs
    , ccppPoolVotingThresholds = mkCanonicalPoolVotingThresholds $ unTHKD cppPoolVotingThresholds
    , ccppDRepVotingThresholds = mkCanonicalDRepVotingThresholds $ unTHKD cppDRepVotingThresholds
    , ccppCommitteeMinSize = unTHKD cppCommitteeMinSize
    , ccppCommitteeMaxTermLength = unTHKD cppCommitteeMaxTermLength
    , ccppGovActionLifetime = unTHKD cppGovActionLifetime
    , ccppGovActionDeposit = unTHKD cppGovActionDeposit
    , ccppDRepDeposit = unTHKD cppDRepDeposit
    , ccppDRepActivity = unTHKD cppDRepActivity
    , ccppMinFeeRefScriptCostPerByte = unTHKD cppMinFeeRefScriptCostPerByte
    }

fromCanonicalPParams :: CanonicalPParams -> PParams ConwayEra
fromCanonicalPParams CanonicalPParams {..} =
  PParams
    ConwayPParams
      { cppA0 = THKD ccppA0
      , cppTxFeePerByte = THKD (CoinPerByte ccppTxFeePerByte)
      , cppTxFeeFixed = THKD ccppTxFeeFixed
      , cppMaxBBSize = THKD ccppMaxBBSize
      , cppMaxTxSize = THKD ccppMaxTxSize
      , cppMaxBHSize = THKD ccppMaxBHSize
      , cppKeyDeposit = THKD ccppKeyDeposit
      , cppPoolDeposit = THKD ccppPoolDeposit
      , cppEMax = THKD ccppEMax
      , cppNOpt = THKD ccppNOpt
      , cppRho = THKD ccppRho
      , cppTau = THKD ccppTau
      , cppProtocolVersion = ccppProtocolVersion
      , cppMinPoolCost = THKD ccppMinPoolCost
      , cppCoinsPerUTxOByte = THKD (CoinPerByte ccppCoinsPerUTxOByte)
      , cppCostModels = THKD (fromCanonicalCostModels ccppCostModels)
      , cppPrices = THKD (fromCanonicalPrices ccppPrices)
      , cppMaxTxExUnits = THKD (ccppMaxTxExUnits & fromCanonicalExUnits & OrdExUnits)
      , cppMaxBlockExUnits = THKD (ccppMaxBlockExUnits & fromCanonicalExUnits & OrdExUnits)
      , cppMaxValSize = THKD ccppMaxValSize
      , cppCollateralPercentage = THKD ccppCollateralPercentage
      , cppMaxCollateralInputs = THKD ccppMaxCollateralInputs
      , cppPoolVotingThresholds = THKD (fromCanonicalPoolVotingThresholds ccppPoolVotingThresholds)
      , cppDRepVotingThresholds = THKD (fromCanonicalDRepVotingThresholds ccppDRepVotingThresholds)
      , cppCommitteeMinSize = THKD ccppCommitteeMinSize
      , cppCommitteeMaxTermLength = THKD ccppCommitteeMaxTermLength
      , cppGovActionLifetime = THKD ccppGovActionLifetime
      , cppGovActionDeposit = THKD ccppGovActionDeposit
      , cppDRepDeposit = THKD ccppDRepDeposit
      , cppDRepActivity = THKD ccppDRepActivity
      , cppMinFeeRefScriptCostPerByte = THKD ccppMinFeeRefScriptCostPerByte
      }

instance ToCanonicalCBOR v CanonicalPParams where
  toCanonicalCBOR v CanonicalPParams {..} =
    encodeAsMap
      [ mkEncodablePair v ("a0" :: Text) ccppA0
      , mkEncodablePair v ("rho" :: Text) ccppRho
      , mkEncodablePair v ("tau" :: Text) ccppTau
      , mkEncodablePair v ("n_opt" :: Text) ccppNOpt
      , mkEncodablePair v ("prices" :: Text) ccppPrices
      , mkEncodablePair v ("epoch_max" :: Text) ccppEMax
      , mkEncodablePair v ("min_fee_a" :: Text) ccppTxFeePerByte
      , mkEncodablePair v ("min_fee_b" :: Text) ccppTxFeeFixed
      , mkEncodablePair v ("cost_models" :: Text) ccppCostModels
      , mkEncodablePair v ("key_deposit" :: Text) ccppKeyDeposit
      , mkEncodablePair v ("max_tx_size" :: Text) ccppMaxTxSize
      , mkEncodablePair v ("drep_deposit" :: Text) ccppDRepDeposit
      , mkEncodablePair v ("max_val_size" :: Text) ccppMaxValSize
      , mkEncodablePair v ("pool_deposit" :: Text) ccppPoolDeposit
      , mkEncodablePair v ("drep_activity" :: Text) ccppDRepActivity
      , mkEncodablePair v ("min_pool_cost" :: Text) ccppMinPoolCost
      , mkEncodablePair v ("max_block_size" :: Text) ccppMaxBBSize
      , mkEncodablePair v ("max_tx_ex_units" :: Text) ccppMaxTxExUnits
      , mkEncodablePair v ("protocol_version" :: Text) ccppProtocolVersion
      , mkEncodablePair v ("coin_per_utxo_byte" :: Text) ccppCoinsPerUTxOByte
      , mkEncodablePair v ("gov_action_deposit" :: Text) ccppGovActionDeposit
      , mkEncodablePair v ("max_block_ex_units" :: Text) ccppMaxBlockExUnits
      , mkEncodablePair v ("min_committee_size" :: Text) ccppCommitteeMinSize
      , mkEncodablePair v ("gov_action_lifetime" :: Text) ccppGovActionLifetime
      , mkEncodablePair v ("committee_term_limit" :: Text) ccppCommitteeMaxTermLength
      , mkEncodablePair v ("collateral_percentage" :: Text) ccppCollateralPercentage
      , mkEncodablePair v ("max_block_header_size" :: Text) ccppMaxBHSize
      , mkEncodablePair v ("max_collateral_inputs" :: Text) ccppMaxCollateralInputs
      , mkEncodablePair v ("drep_voting_thresholds" :: Text) ccppDRepVotingThresholds
      , mkEncodablePair v ("pool_voting_thresholds" :: Text) ccppPoolVotingThresholds
      , mkEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          ccppMinFeeRefScriptCostPerByte
      ]

instance FromCanonicalCBOR v CanonicalPParams where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 31
    Versioned ccppA0 <- decodeField "a0"
    Versioned ccppRho <- decodeField "rho"
    Versioned ccppTau <- decodeField "tau"
    Versioned ccppNOpt <- decodeField "n_opt"
    Versioned ccppPrices <- decodeField "prices"
    Versioned ccppEMax <- decodeField "epoch_max"
    Versioned ccppTxFeePerByte <- decodeField "min_fee_a"
    Versioned ccppTxFeeFixed <- decodeField "min_fee_b"
    Versioned ccppCostModels <- decodeField "cost_models"
    Versioned ccppKeyDeposit <- decodeField "key_deposit"
    Versioned ccppMaxTxSize <- decodeField "max_tx_size"
    Versioned ccppDRepDeposit <- decodeField "drep_deposit"
    Versioned ccppMaxValSize <- decodeField "max_val_size"
    Versioned ccppPoolDeposit <- decodeField "pool_deposit"
    Versioned ccppDRepActivity <- decodeField "drep_activity"
    Versioned ccppMinPoolCost <- decodeField "min_pool_cost"
    Versioned ccppMaxBBSize <- decodeField "max_block_size"
    Versioned ccppMaxTxExUnits <- decodeField "max_tx_ex_units"
    Versioned ccppProtocolVersion <- decodeField "protocol_version"
    Versioned ccppCoinsPerUTxOByte <- decodeField "coin_per_utxo_byte"
    Versioned ccppGovActionDeposit <- decodeField "gov_action_deposit"
    Versioned ccppMaxBlockExUnits <- decodeField "max_block_ex_units"
    Versioned ccppCommitteeMinSize <- decodeField "min_committee_size"
    Versioned ccppGovActionLifetime <- decodeField "gov_action_lifetime"
    Versioned ccppCommitteeMaxTermLength <- decodeField "committee_term_limit"
    Versioned ccppCollateralPercentage <- decodeField "collateral_percentage"
    Versioned ccppMaxBHSize <- decodeField "max_block_header_size"
    Versioned ccppMaxCollateralInputs <- decodeField "max_collateral_inputs"
    Versioned ccppDRepVotingThresholds <- decodeField "drep_voting_thresholds"
    Versioned ccppPoolVotingThresholds <- decodeField "pool_voting_thresholds"
    Versioned ccppMinFeeRefScriptCostPerByte <- decodeField "min_fee_ref_script_cost_per_byte"
    pure $ Versioned CanonicalPParams {..}

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

type instance NamespaceKeySize "gov/pparams/v0" = 4

instance KnownNamespace "gov/pparams/v0" where
  type NamespaceKey "gov/pparams/v0" = GovPParamsIn
  type NamespaceEntry "gov/pparams/v0" = CanonicalPParams

instance CanonicalCBOREntryEncoder "gov/pparams/v0" CanonicalPParams where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/pparams/v0") n

instance CanonicalCBOREntryDecoder "gov/pparams/v0" CanonicalPParams where
  decodeEntry = fromCanonicalCBOR

data CanonicalExUnits = CanonicalExUnits
  { exUnitsMem' :: !Natural
  , exUnitsSteps' :: !Natural
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalExUnits where
  toCanonicalCBOR v CanonicalExUnits{..} = toCanonicalCBOR v (exUnitsMem', exUnitsSteps')

instance FromCanonicalCBOR v CanonicalExUnits where
  fromCanonicalCBOR = do
    Versioned (exUnitsMem', exUnitsSteps') <- fromCanonicalCBOR @v
    return $ Versioned CanonicalExUnits{..}

mkCanonicalExUnits :: ExUnits -> CanonicalExUnits
mkCanonicalExUnits (unWrapExUnits -> ExUnits'{..}) = CanonicalExUnits{..}

fromCanonicalExUnits :: CanonicalExUnits -> ExUnits
fromCanonicalExUnits CanonicalExUnits{..} = WrapExUnits ExUnits'{..}

-- TODO: remove

instance FromCanonicalCBOR v Natural where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v . fromIntegral <$> D.decodeIntegerCanonical

instance ToCanonicalCBOR v Natural where
  toCanonicalCBOR _v n = assumeCanonicalEncoding $ E.encodeInteger (fromIntegral n)