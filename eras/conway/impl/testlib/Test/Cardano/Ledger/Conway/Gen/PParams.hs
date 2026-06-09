{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO
-- \* find examples of costmodels from plutus repo
-- \* also use costmodels that are in ledger
-- \* use mainnet values to derive realistic values for each field

module Test.Cardano.Ledger.Conway.Gen.PParams (
  genShelleyBasedPParams,
  genAlonzoBasedPParams,
  genBabbageEraPParams,
  genConwayBasedEraPParams,
) where

import Cardano.Ledger.Alonzo.PParams (
  AlonzoEraPParams,
  ppCollateralPercentageL,
  ppCostModelsL,
  ppMaxBlockExUnitsL,
  ppMaxCollateralInputsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
  ppPricesL,
 )
import Cardano.Ledger.Babbage.PParams (
  BabbageEraPParams,
  ppCoinsPerUTxOByteL,
 )
import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin (Coin (Coin), CompactForm (CompactCoin))
import Cardano.Ledger.Conway.Core (
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppCommitteeMaxTermLengthL,
  ppCommitteeMinSizeL,
  ppDRepActivityL,
  ppDRepDepositL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppMinFeeRefScriptCostPerByteL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Core (
  CoinPerByte (CoinPerByte),
  EraPParams,
  PParams,
  emptyPParams,
  ppA0L,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinPoolCostL,
  ppNOptL,
  ppPoolDepositL,
  ppRhoL,
  ppTauL,
  ppTxFeeFixedL,
  ppTxFeePerByteL,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Data.Function ((&))
import Data.Word (Word16, Word32)
import Lens.Micro ((.~), (<>~))
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common (
  Gen,
  NonNegative (NonNegative),
  Positive (Positive),
  arbitrary,
  choose,
  chooseInt,
 )
import Test.Cardano.Ledger.Plutus (testingCostModels)

genShelleyBasedPParams :: forall era. EraPParams era => Gen (PParams era)
genShelleyBasedPParams = do
  (NonNegative ppTxFeePerByte) <- arbitrary
  (NonNegative ppTxFeeFixed) <- arbitrary
  ppMaxTxSize <- choose (5_000, 100_000 :: Word32)
  ppMaxBHSize <- choose (1_000, 10_000 :: Word16)
  -- Enforce the validation rule: ppMaxTxSize + ppMaxBHSize < ppMaxBBSize
  ppBBExtra <- choose (1, 10_000 :: Word32)
  let ppMaxBBSize = ppMaxTxSize + fromIntegral ppMaxBHSize + ppBBExtra
  -- TODO We should use the genCoin statistical function when available.
  (NonNegative ppKeyDeposit) <- arbitrary
  -- TODO We should use the genCoin statistical function when available.
  (NonNegative ppPoolDeposit) <- arbitrary
  (Positive ppEMax) <- arbitrary
  (Positive ppNOpt) <- arbitrary
  -- Generate a `a0` value from [0; 100]
  ppA0Num <- choose (0, 1000)
  let ppA0 = ppA0Num %! 100
  -- Generate a `rho` value from [0; 1]
  ppRhoNum <- choose (0, 100)
  let ppRho = ppRhoNum %! 100
  -- Generate a `tau` value from [0; 1]
  ppTauNum <- choose (0, 100)
  let ppTau = ppTauNum %! 100
  -- TODO We should use the genCoin statistical function when available.
  (NonNegative ppMinPoolCost) <- arbitrary
  pure $
    emptyPParams @era
      & ppTxFeePerByteL .~ CoinPerByte (CompactCoin ppTxFeePerByte)
      & ppTxFeeFixedL .~ Coin ppTxFeeFixed
      & ppMaxTxSizeL .~ ppMaxTxSize
      & ppMaxBBSizeL .~ ppMaxBBSize
      & ppMaxBHSizeL .~ ppMaxBHSize
      & ppKeyDepositL .~ Coin ppKeyDeposit
      & ppPoolDepositL .~ Coin ppPoolDeposit
      & ppEMaxL .~ EpochInterval ppEMax
      & ppNOptL .~ ppNOpt
      & ppA0L .~ ppA0
      & ppRhoL .~ ppRho
      & ppTauL .~ ppTau
      & ppMinPoolCostL .~ Coin ppMinPoolCost

genAlonzoBasedPParams ::
  forall era.
  AlonzoEraPParams era =>
  Gen (PParams era)
genAlonzoBasedPParams = do
  basePParams <- genShelleyBasedPParams
  -- Generate a `prMem` value from [0; 100]
  prMemNum <- choose (0, 1000)
  let prMem = prMemNum %! 100
  -- Generate a `prSteps` value from [0; 100]
  prStepsNum <- choose (0, 1000)
  let prSteps = prStepsNum %! 100
  ppMaxTxExUnitsMem <- fromIntegral <$> chooseInt (1_000_000, 14_000_000)
  ppMaxTxExUnitsSteps <- fromIntegral <$> chooseInt (1_000_000_000, 10_000_000_000)
  -- Block ExUnits must be >= tx ExUnits (a valid tx must be able to fit in a block)
  blockMemFactor <- chooseInt (1, 10)
  blockStepsFactor <- chooseInt (1, 10)
  let ppMaxBlockExUnitsMem = ppMaxTxExUnitsMem * fromIntegral blockMemFactor
      ppMaxBlockExUnitsSteps = ppMaxTxExUnitsSteps * fromIntegral blockStepsFactor
  ppMaxValSize <- choose (4_000, 40_000)
  ppCollateralPercentage <- fromIntegral <$> chooseInt (1, 10_000)
  ppMaxCollateralInputs <- choose (1, 10)
  pure $
    basePParams
      & ppCostModelsL <>~ testingCostModels [PlutusV1]
      & ppPricesL .~ Prices {prMem, prSteps}
      & ppMaxTxExUnitsL .~ ExUnits ppMaxTxExUnitsMem ppMaxTxExUnitsSteps
      & ppMaxBlockExUnitsL .~ ExUnits ppMaxBlockExUnitsMem ppMaxBlockExUnitsSteps
      & ppMaxValSizeL .~ ppMaxValSize
      & ppCollateralPercentageL .~ ppCollateralPercentage
      & ppMaxCollateralInputsL .~ ppMaxCollateralInputs

genBabbageEraPParams ::
  forall era.
  BabbageEraPParams era =>
  Gen (PParams era)
genBabbageEraPParams = do
  basePParams <- genAlonzoBasedPParams
  ppCoinsPerUTxOByte <- choose (4_000, 40_000)
  pure $
    basePParams
      & ppCostModelsL <>~ testingCostModels [PlutusV1, PlutusV2]
      & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin ppCoinsPerUTxOByte)

genConwayBasedEraPParams ::
  forall era.
  ConwayEraPParams era =>
  Gen (PParams era)
genConwayBasedEraPParams = do
  basePParams <- genBabbageEraPParams
  -- PoolVotingThresholds: UnitInterval [0,1]
  pvtMotionNoConfidence <- (%! 100) <$> choose (0, 100)
  pvtCommitteeNormal <- (%! 100) <$> choose (0, 100)
  pvtCommitteeNoConfidence <- (%! 100) <$> choose (0, 100)
  pvtHardForkInitiation <- (%! 100) <$> choose (0, 100)
  pvtPPSecurityGroup <- (%! 100) <$> choose (0, 100)
  -- DRepVotingThresholds: UnitInterval [0,1]
  dvtMotionNoConfidence <- (%! 100) <$> choose (0, 100)
  dvtCommitteeNormal <- (%! 100) <$> choose (0, 100)
  dvtCommitteeNoConfidence <- (%! 100) <$> choose (0, 100)
  dvtUpdateToConstitution <- (%! 100) <$> choose (0, 100)
  dvtHardForkInitiation <- (%! 100) <$> choose (0, 100)
  dvtPPNetworkGroup <- (%! 100) <$> choose (0, 100)
  dvtPPEconomicGroup <- (%! 100) <$> choose (0, 100)
  dvtPPTechnicalGroup <- (%! 100) <$> choose (0, 100)
  dvtPPGovGroup <- (%! 100) <$> choose (0, 100)
  dvtTreasuryWithdrawal <- (%! 100) <$> choose (0, 100)
  -- Committee size: small positive integer
  ppCommitteeMinSize <- choose (1, 20)
  -- Term lengths in epochs
  ppCommitteeMaxTermLength <- EpochInterval <$> choose (36, 146)
  ppGovActionLifetime <- EpochInterval <$> choose (1, 30)
  -- Deposits in lovelace
  ppGovActionDeposit <- Coin <$> choose (100_000_000, 1_000_000_000_000)
  ppDRepDeposit <- Coin <$> choose (100_000_000, 1_000_000_000)
  -- DRep inactivity window in epochs
  ppDRepActivity <- EpochInterval <$> choose (10, 100)
  -- Reference script fee: NonNegativeInterval [0,+∞)
  ppMinFeeRefScriptCostPerByteNum <- choose (0, 1000 :: Integer)
  let ppMinFeeRefScriptCostPerByte = ppMinFeeRefScriptCostPerByteNum %! 100
  pure $
    basePParams
      & ppCostModelsL <>~ testingCostModels [PlutusV3]
      & ppPoolVotingThresholdsL
        .~ PoolVotingThresholds
          { pvtMotionNoConfidence
          , pvtCommitteeNormal
          , pvtCommitteeNoConfidence
          , pvtHardForkInitiation
          , pvtPPSecurityGroup
          }
      & ppDRepVotingThresholdsL
        .~ DRepVotingThresholds
          { dvtMotionNoConfidence
          , dvtCommitteeNormal
          , dvtCommitteeNoConfidence
          , dvtUpdateToConstitution
          , dvtHardForkInitiation
          , dvtPPNetworkGroup
          , dvtPPEconomicGroup
          , dvtPPTechnicalGroup
          , dvtPPGovGroup
          , dvtTreasuryWithdrawal
          }
      & ppCommitteeMinSizeL .~ ppCommitteeMinSize
      & ppCommitteeMaxTermLengthL .~ ppCommitteeMaxTermLength
      & ppGovActionLifetimeL .~ ppGovActionLifetime
      & ppGovActionDepositL .~ ppGovActionDeposit
      & ppDRepDepositL .~ ppDRepDeposit
      & ppDRepActivityL .~ ppDRepActivity
      & ppMinFeeRefScriptCostPerByteL .~ ppMinFeeRefScriptCostPerByte
