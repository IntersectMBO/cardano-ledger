{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Conway.PParams where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)

pparamsSpec ::
  IsConwayUniv fn =>
  Specification fn (PParams (ConwayEra StandardCrypto))
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
         cppProtocolVersion
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
            [ assert $ cppProtocolVersion ==. lit (ProtVer (natVersion @10) 0)
            , assert $ cppMaxBBSize /=. lit (THKD 0)
            , assert $ cppMaxTxSize /=. lit (THKD 0)
            , assert $ cppMaxBHSize /=. lit (THKD 0)
            , assert $ cppMaxValSize /=. lit (THKD 0)
            , assert $ cppCollateralPercentage /=. lit (THKD 0)
            , assert $ cppCommitteeMaxTermLength /=. lit (THKD $ EpochInterval 0)
            , assert $ cppGovActionLifetime /=. lit (THKD $ EpochInterval 0)
            , assert $ cppPoolDeposit /=. lit (THKD mempty)
            , assert $ cppGovActionDeposit /=. lit (THKD mempty)
            , assert $ cppDRepDeposit /=. lit (THKD mempty)
            , assert $ cppProtocolVersion ==. lit (ProtVer (natVersion @10) 0)
            , match cppEMax $ \epochInterval ->
                lit (EpochInterval 5) <. epochInterval
            ]
