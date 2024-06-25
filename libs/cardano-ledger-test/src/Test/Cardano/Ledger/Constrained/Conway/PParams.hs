{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Conway.PParams where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.PParams
import Data.Maybe (fromJust)
import Data.Ratio ((%))

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Monad.Identity
import GHC.Word (Word32)
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn, IsConwayUniv)

pparamsSpec :: IsConwayUniv fn => Specification fn (PParams (ConwayEra StandardCrypto))
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
         cppPoolVotingThresholds
         cppDRepVotingThresholds
         cppCommitteeMinSize
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
            , assert $ cppPoolVotingThresholds ==. lit (THKD (majorityPool))
            , assert $ cppDRepVotingThresholds ==. lit (THKD (majorityDRep))
            , assert $ cppCommitteeMinSize ==. lit (THKD 5)
            ]

{-
committeeMaxTermLength_ ::
  Term ConwayFn (ConwayPParams Identity (ConwayEra StandardCrypto)) ->
  Term ConwayFn EpochInterval
committeeMaxTermLength_ = sel @25
-}

majorityPool :: PoolVotingThresholds
majorityPool = PoolVotingThresholds fiftyp fiftyp fiftyp fiftyp fiftyp

majorityDRep :: DRepVotingThresholds
majorityDRep = DRepVotingThresholds fiftyp fiftyp fiftyp fiftyp fiftyp fiftyp fiftyp fiftyp fiftyp fiftyp

-- | UnitInterval of 50% (0.5)
fiftyp :: UnitInterval
fiftyp = fromJust (boundRational @UnitInterval $ 1 % 2)
