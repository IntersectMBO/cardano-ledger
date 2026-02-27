{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Conway (
  mkCanonicalConstitution,
) where

import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCoin (..),
  fromCanonicalExUnits,
  mkCanonicalExUnits,
 )
import Cardano.Ledger.CanonicalState.Namespace
import Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 ()
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (Constitution (..))
import Cardano.Ledger.Conway.PParams
import Cardano.SCLS.CBOR.Canonical (
  CanonicalDecoder,
  assumeCanonicalDecoder,
  assumeCanonicalEncoding,
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeMapLenCanonicalOf,
  decodeWordCanonicalOf,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.NamespaceCodec
import Cardano.SCLS.Versioned (Versioned (..))
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Lens.Micro

type instance NamespaceEra "blocks/v0" = ConwayEra

type instance NamespaceEra "gov/committee/v0" = ConwayEra

type instance NamespaceEra "utxo/v0" = ConwayEra

type instance NamespaceEra "gov/constitution/v0" = ConwayEra

type instance NamespaceEra "gov/pparams/v0" = ConwayEra

type instance NamespaceEra "utxo/v0" = ConwayEra

type instance NamespaceEra "pool_stake/v0" = ConwayEra

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoIn
  type NamespaceEntry "utxo/v0" = UtxoOut ConwayEra

mkCanonicalConstitution :: Constitution era -> CanonicalConstitution
mkCanonicalConstitution Constitution {..} = CanonicalConstitution {..}

instance ToCanonicalCBOR "gov/pparams/v0" (PParams ConwayEra) where
  toCanonicalCBOR v pp =
    encodeAsMap
      [ mkEncodablePair v (0 :: Int) (pp ^. ppTxFeePerByteL . to (CanonicalCoin . unCoinPerByte))
      , mkEncodablePair v (1 :: Int) (pp ^. ppTxFeeFixedCompactL . to (CanonicalCoin))
      , mkEncodablePair v (2 :: Int) (pp ^. ppMaxBBSizeL)
      , mkEncodablePair v (3 :: Int) (pp ^. ppMaxTxSizeL)
      , mkEncodablePair v (4 :: Int) (pp ^. ppMaxBHSizeL)
      , mkEncodablePair v (5 :: Int) (pp ^. ppKeyDepositCompactL . to (CanonicalCoin))
      , mkEncodablePair v (6 :: Int) (pp ^. ppPoolDepositCompactL . to (CanonicalCoin))
      , mkEncodablePair v (7 :: Int) (pp ^. ppEMaxL)
      , mkEncodablePair v (8 :: Int) (pp ^. ppNOptL)
      , mkEncodablePair v (9 :: Int) (pp ^. ppA0L)
      , mkEncodablePair v (10 :: Int) (pp ^. ppRhoL)
      , mkEncodablePair v (11 :: Int) (pp ^. ppTauL)
      , mkEncodablePair v (14 :: Int) (pp ^. ppProtocolVersionL)
      , mkEncodablePair v (16 :: Int) (pp ^. ppMinPoolCostCompactL . to (CanonicalCoin))
      , mkEncodablePair v (17 :: Int) (pp ^. ppCoinsPerUTxOByteL . to (CanonicalCoin . unCoinPerByte))
      , mkEncodablePair v (18 :: Int) (pp ^. ppCostModelsL)
      , mkEncodablePair v (19 :: Int) (pp ^. ppPricesL . to (mkCanonicalPrices))
      , mkEncodablePair v (20 :: Int) (pp ^. ppMaxTxExUnitsL . to (mkCanonicalExUnits))
      , mkEncodablePair v (21 :: Int) (pp ^. ppMaxBlockExUnitsL . to (mkCanonicalExUnits))
      , mkEncodablePair v (22 :: Int) (pp ^. ppMaxValSizeL)
      , mkEncodablePair v (23 :: Int) (pp ^. ppCollateralPercentageL)
      , mkEncodablePair v (24 :: Int) (pp ^. ppMaxCollateralInputsL)
      , mkEncodablePair v (25 :: Int) (pp ^. ppPoolVotingThresholdsL)
      , mkEncodablePair v (26 :: Int) (pp ^. ppDRepVotingThresholdsL)
      , mkEncodablePair v (27 :: Int) (pp ^. ppCommitteeMinSizeL)
      , mkEncodablePair v (28 :: Int) (pp ^. ppCommitteeMaxTermLengthL)
      , mkEncodablePair v (29 :: Int) (pp ^. ppGovActionLifetimeL)
      , mkEncodablePair v (30 :: Int) (pp ^. ppGovActionDepositCompactL . to (CanonicalCoin))
      , mkEncodablePair v (31 :: Int) (pp ^. ppDRepDepositCompactL . to (CanonicalCoin))
      , mkEncodablePair v (32 :: Int) (pp ^. ppDRepActivityL)
      , mkEncodablePair v (33 :: Int) (pp ^. ppMinFeeRefScriptCostPerByteL)
      ]

instance FromCanonicalCBOR "gov/pparams/v0" (PParams ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 31
    txFeePerByte <- decodeField @"gov/pparams/v0" @CanonicalCoin 0
    txFeeFixedCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 1
    maxBBSize <- decodeField @"gov/pparams/v0" 2
    maxTxSize <- decodeField @"gov/pparams/v0" 3
    maxBHSize <- decodeField @"gov/pparams/v0" 4
    keyDepositCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 5
    poolDepositCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 6
    eMax <- decodeField @"gov/pparams/v0" 7
    nOpt <- decodeField @"gov/pparams/v0" 8
    a0 <- decodeField @"gov/pparams/v0" 9
    rho <- decodeField @"gov/pparams/v0" 10
    tau <- decodeField @"gov/pparams/v0" 11
    protVer <- decodeField @"gov/pparams/v0" 14
    minPoolCostCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 16
    coinsPerUTxOByte <- decodeField @"gov/pparams/v0" @CanonicalCoin 17
    costModels <- decodeField @"gov/pparams/v0" 18
    prices <- decodeField @"gov/pparams/v0" 19
    maxTxExUnits <- decodeField @"gov/pparams/v0" 20
    maxBlockExUnits <- decodeField @"gov/pparams/v0" 21
    maxValSize <- decodeField @"gov/pparams/v0" 22
    collateralPercentage <- decodeField @"gov/pparams/v0" 23
    maxCollateralInputs <- decodeField @"gov/pparams/v0" 24
    poolVotingThresholds <- decodeField @"gov/pparams/v0" 25
    dRepVotingThresholds <- decodeField @"gov/pparams/v0" 26
    committeeMinSize <- decodeField @"gov/pparams/v0" 27
    committeeMaxTermLength <- decodeField @"gov/pparams/v0" 28
    govActionLifetime <- decodeField @"gov/pparams/v0" 29
    govActionDepositCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 30
    dRepDepositCompact <- decodeField @"gov/pparams/v0" @CanonicalCoin 31
    dRepActivity <- decodeField @"gov/pparams/v0" 32
    minFeeRefScriptCostPerByte <- decodeField @"gov/pparams/v0" 33

    return $
      Versioned $
        emptyPParams @ConwayEra
          & ppTxFeePerByteL .~ CoinPerByte (unCoin txFeePerByte)
          & ppTxFeeFixedCompactL .~ unCoin txFeeFixedCompact
          & ppMaxBBSizeL .~ maxBBSize
          & ppMaxTxSizeL .~ maxTxSize
          & ppMaxBHSizeL .~ maxBHSize
          & ppKeyDepositCompactL .~ unCoin keyDepositCompact
          & ppPoolDepositCompactL .~ unCoin poolDepositCompact
          & ppEMaxL .~ eMax
          & ppNOptL .~ nOpt
          & ppA0L .~ a0
          & ppRhoL .~ rho
          & ppTauL .~ tau
          & ppMinPoolCostCompactL .~ unCoin minPoolCostCompact
          & ppCoinsPerUTxOByteL .~ CoinPerByte (unCoin coinsPerUTxOByte)
          & ppCostModelsL .~ costModels
          & ppPricesL .~ fromCanonicalPrices prices
          & ppMaxTxExUnitsL .~ fromCanonicalExUnits maxTxExUnits
          & ppMaxBlockExUnitsL .~ fromCanonicalExUnits maxBlockExUnits
          & ppMaxValSizeL .~ maxValSize
          & ppCollateralPercentageL .~ collateralPercentage
          & ppMaxCollateralInputsL .~ maxCollateralInputs
          & ppPoolVotingThresholdsL .~ poolVotingThresholds
          & ppDRepVotingThresholdsL .~ dRepVotingThresholds
          & ppCommitteeMinSizeL .~ committeeMinSize
          & ppCommitteeMaxTermLengthL .~ committeeMaxTermLength
          & ppGovActionLifetimeL .~ govActionLifetime
          & ppGovActionDepositCompactL .~ unCoin govActionDepositCompact
          & ppDRepDepositCompactL .~ unCoin dRepDepositCompact
          & ppDRepActivityL .~ dRepActivity
          & ppMinFeeRefScriptCostPerByteL .~ minFeeRefScriptCostPerByte
          & ppProtocolVersionL .~ protVer

decodeField :: forall v a s. FromCanonicalCBOR v a => Word -> CanonicalDecoder s a
decodeField expectedTag = do
  decodeWordCanonicalOf expectedTag
  unVer <$> fromCanonicalCBOR @v

instance ToCanonicalCBOR "gov/pparams/v0" DRepVotingThresholds where
  toCanonicalCBOR v dvt =
    assumeCanonicalEncoding (E.encodeListLen 10)
      <> toCanonicalCBOR v (dvt ^. dvtMotionNoConfidenceL)
      <> toCanonicalCBOR v (dvt ^. dvtCommitteeNormalL)
      <> toCanonicalCBOR v (dvt ^. dvtCommitteeNoConfidenceL)
      <> toCanonicalCBOR v (dvt ^. dvtUpdateToConstitutionL)
      <> toCanonicalCBOR v (dvt ^. dvtHardForkInitiationL)
      <> toCanonicalCBOR v (dvt ^. dvtPPNetworkGroupL)
      <> toCanonicalCBOR v (dvt ^. dvtPPEconomicGroupL)
      <> toCanonicalCBOR v (dvt ^. dvtPPTechnicalGroupL)
      <> toCanonicalCBOR v (dvt ^. dvtPPGovGroupL)
      <> toCanonicalCBOR v (dvt ^. dvtTreasuryWithdrawalL)

instance FromCanonicalCBOR "gov/pparams/v0" DRepVotingThresholds where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 10
    Versioned dvtMotionNoConfidence <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtCommitteeNormal <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtCommitteeNoConfidence <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtUpdateToConstitution <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtHardForkInitiation <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtPPNetworkGroup <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtPPEconomicGroup <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtPPTechnicalGroup <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtPPGovGroup <- fromCanonicalCBOR @"gov/pparams/v0"
    Versioned dvtTreasuryWithdrawal <- fromCanonicalCBOR @"gov/pparams/v0"
    return $ Versioned DRepVotingThresholds {..}

instance ToCanonicalCBOR "gov/pparams/v0" PoolVotingThresholds where
  toCanonicalCBOR v pvt =
    toCanonicalCBOR
      v
      ( pvt ^. pvtMotionNoConfidenceL
      , pvt ^. pvtCommitteeNormalL
      , pvt ^. pvtCommitteeNoConfidenceL
      , pvt ^. pvtHardForkInitiationL
      , pvt ^. pvtPPSecurityGroupL
      )

instance FromCanonicalCBOR "gov/pparams/v0" PoolVotingThresholds where
  fromCanonicalCBOR = do
    Versioned
      ( pvtMotionNoConfidence
        , pvtCommitteeNormal
        , pvtCommitteeNoConfidence
        , pvtHardForkInitiation
        , pvtPPSecurityGroup
        ) <-
      fromCanonicalCBOR @"gov/pparams/v0"
    return $ Versioned PoolVotingThresholds {..}
