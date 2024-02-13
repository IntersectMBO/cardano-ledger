{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.V2.Conway.PParams where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.PParams

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway

pparamsSpec :: ConwayUniverse fn => Spec fn (PParams (ConwayEra StandardCrypto))
pparamsSpec =
  constrained $ \pp ->
    match pp $ \cpp ->
      match cpp $
        \_cppMinFeeA
         _cppMinFeeB
         cppMaxBBSize
         cppMaxTxSize
         cppMaxBHSize
         _cppKeyDeposit
         cppPoolDeposit
         cppEMax
         _cppNOpt
         _cppA0
         _cppRho
         _cppTau
         _cppProtocolVersion
         _cppMinPoolCost
         _cppCoinsPerUTxOByte
         _cppCostModels
         _cppPrices
         _cppMaxTxExUnits
         _cppMaxBlockExUnits
         cppMaxValSize
         cppCollateralPercentage
         _cppMaxCollateralInputs
         _cppPoolVotingThresholds
         _cppDRepVotingThresholds
         _cppCommitteeMinSize
         cppCommitteeMaxTermLength
         cppGovActionLifetime
         cppGovActionDeposit
         cppDRepDeposit
         _cppDRepActivity
         _cppMinFeeRefScriptCoinsPerByte ->
            [ assert $ cppMaxBBSize /=. lit (THKD 0)
            , assert $ cppMaxTxSize /=. lit (THKD 0)
            , assert $ cppMaxBHSize /=. lit (THKD 0)
            , assert $ cppMaxValSize /=. lit (THKD 0)
            , assert $ cppCollateralPercentage /=. lit (THKD 0)
            , assert $ cppCommitteeMaxTermLength /=. lit (THKD $ EpochInterval 0)
            , assert $ cppGovActionLifetime /=. lit (THKD $ EpochInterval 0)
            , assert $ cppPoolDeposit /=. lit (THKD mempty)
            , assert $ cppGovActionDeposit /=. lit (THKD mempty)
            , assert $ cppDRepDeposit /=. lit (THKD mempty)
            , match cppEMax $ \epochInterval ->
                lit (EpochInterval 0) <. epochInterval
            ]
