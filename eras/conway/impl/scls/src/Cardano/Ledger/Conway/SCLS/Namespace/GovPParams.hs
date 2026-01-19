{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
  ( module Cardano.Ledger.SCLS.Namespace.GovPParams.V0,

  ) where

import Cardano.Ledger.SCLS.Namespace.GovPParams.V0
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
import Cardano.Ledger.SCLS.Common (
  CoinPerByte (..),
  fromCanonicalCoin,
  mkCanonicalCoin,
 )

import Data.Function ((&))
import Cardano.Ledger.Core (PParams (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  THKD (..),
 )

instance IsCanonicalPParams (PParams ConwayEra) where
  mkCanonicalPParams (PParams ConwayPParams {..}) =
    CanonicalPParams
      { ccppA0 = unTHKD cppA0
      , ccppTxFeePerByte = mkCanonicalCoin $ unCoinPerByte $ unTHKD cppTxFeePerByte
      , ccppTxFeeFixed = mkCanonicalCoin $ unTHKD cppTxFeeFixed
      , ccppMaxBBSize = unTHKD cppMaxBBSize
      , ccppMaxTxSize = unTHKD cppMaxTxSize
      , ccppMaxBHSize = unTHKD cppMaxBHSize
      , ccppKeyDeposit = mkCanonicalCoin $ unTHKD cppKeyDeposit
      , ccppPoolDeposit = mkCanonicalCoin $ unTHKD cppPoolDeposit
      , ccppEMax = unTHKD cppEMax
      , ccppNOpt = unTHKD cppNOpt
      , ccppRho = unTHKD cppRho
      , ccppTau = unTHKD cppTau
      , ccppProtocolVersion = cppProtocolVersion
      , ccppMinPoolCost = mkCanonicalCoin $ unTHKD cppMinPoolCost
      , ccppCoinsPerUTxOByte =  mkCanonicalCoin $ unCoinPerByte $ unTHKD cppCoinsPerUTxOByte
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
      , ccppGovActionDeposit = mkCanonicalCoin $ unTHKD cppGovActionDeposit
      , ccppDRepDeposit = mkCanonicalCoin $ unTHKD cppDRepDeposit
      , ccppDRepActivity = unTHKD cppDRepActivity
      , ccppMinFeeRefScriptCostPerByte = unTHKD cppMinFeeRefScriptCostPerByte
      }

  fromCanonicalPParams CanonicalPParams {..} =
    PParams
      ConwayPParams
        { cppA0 = THKD ccppA0
        , cppTxFeePerByte = THKD (CoinPerByte $ fromCanonicalCoin ccppTxFeePerByte)
        , cppTxFeeFixed = THKD (fromCanonicalCoin ccppTxFeeFixed)
        , cppMaxBBSize = THKD ccppMaxBBSize
        , cppMaxTxSize = THKD ccppMaxTxSize
        , cppMaxBHSize = THKD ccppMaxBHSize
        , cppKeyDeposit = THKD (fromCanonicalCoin ccppKeyDeposit)
        , cppPoolDeposit = THKD (fromCanonicalCoin ccppPoolDeposit)
        , cppEMax = THKD ccppEMax
        , cppNOpt = THKD ccppNOpt
        , cppRho = THKD ccppRho
        , cppTau = THKD ccppTau
        , cppProtocolVersion = ccppProtocolVersion
        , cppMinPoolCost = THKD (fromCanonicalCoin ccppMinPoolCost)
        , cppCoinsPerUTxOByte = THKD (CoinPerByte $ fromCanonicalCoin ccppCoinsPerUTxOByte)
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
        , cppGovActionDeposit = THKD (fromCanonicalCoin ccppGovActionDeposit)
        , cppDRepDeposit = THKD (fromCanonicalCoin ccppDRepDeposit)
        , cppDRepActivity = THKD ccppDRepActivity
        , cppMinFeeRefScriptCostPerByte = THKD ccppMinFeeRefScriptCostPerByte
        }

instance IsCanonicalDRepVotingThresholds DRepVotingThresholds where
  mkCanonicalDRepVotingThresholds DRepVotingThresholds {..} = CanonicalDRepVotingThresholds {..}
  fromCanonicalDRepVotingThresholds CanonicalDRepVotingThresholds {..} = DRepVotingThresholds {..}

instance IsCanonicalPoolVotingThresholds PoolVotingThresholds where
  fromCanonicalPoolVotingThresholds CanonicalPoolVotingThresholds {..} = PoolVotingThresholds {..}
  mkCanonicalPoolVotingThresholds PoolVotingThresholds {..} = CanonicalPoolVotingThresholds {..}