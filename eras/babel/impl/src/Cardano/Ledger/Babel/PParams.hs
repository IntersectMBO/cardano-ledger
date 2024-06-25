{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Babel.PParams (
  ppPoolVotingThresholdsL,
  ppDRepVotingThresholdsL,
  ppCommitteeMinSizeL,
  ppCommitteeMaxTermLengthL,
  ppGovActionLifetimeL,
  ppGovActionDepositL,
  ppDRepDepositL,
  ppDRepActivityL,
  ppuPoolVotingThresholdsL,
  ppuDRepVotingThresholdsL,
  ppuCommitteeMinSizeL,
  ppuCommitteeMaxTermLengthL,
  ppuGovActionLifetimeL,
  ppuGovActionDepositL,
  ppuDRepDepositL,
  ppuDRepActivityL,
  PoolVotingThresholds (..),
  DRepVotingThresholds (..),
  dvtPPNetworkGroupL,
  dvtPPGovGroupL,
  dvtPPTechnicalGroupL,
  dvtPPEconomicGroupL,
  dvtUpdateToConstitutionL,
  THKD (..),
)
where

import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams (..), OrdExUnits (..))
import Cardano.Ledger.Alonzo.Scripts (
  ExUnits (..),
 )
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ConwayPParams (..),
  THKD (THKD, unTHKD),
  UpgradeConwayPParams (..),
  conwayApplyPPUpdates,
  conwayModifiedPPGroups,
  conwayPParamsPairs,
  emptyConwayPParams,
  emptyConwayPParamsUpdate,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (
  HKD,
  HKDApplicative,
  HKDFunctor (..),
 )
import Cardano.Ledger.Val (Val (..))
import Data.Aeson hiding (Encoding, Value, decode, encode)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity)
import Data.Proxy
import Lens.Micro

instance Crypto c => EraPParams (BabelEra c) where
  type PParamsHKD f (BabelEra c) = ConwayPParams f (BabelEra c)

  type UpgradePParams f (BabelEra c) = UpgradeConwayPParams f
  type DowngradePParams f (BabelEra c) = ()

  applyPPUpdates (PParams pp) (PParamsUpdate ppu) =
    PParams $ conwayApplyPPUpdates pp ppu

  emptyPParamsIdentity = emptyConwayPParams
  emptyPParamsStrictMaybe = emptyConwayPParamsUpdate

  upgradePParamsHKD ::
    forall f.
    HKDApplicative f =>
    UpgradePParams f (BabelEra c) ->
    PParamsHKD f (PreviousEra (BabelEra c)) ->
    PParamsHKD f (BabelEra c)
  upgradePParamsHKD = upgradeBabelPParamsHKD
  downgradePParamsHKD () = coerce

  hkdMinFeeAL = lens (unTHKD . cppMinFeeA) $ \pp x -> pp {cppMinFeeA = THKD x}
  hkdMinFeeBL = lens (unTHKD . cppMinFeeB) $ \pp x -> pp {cppMinFeeB = THKD x}
  hkdMaxBBSizeL = lens (unTHKD . cppMaxBBSize) $ \pp x -> pp {cppMaxBBSize = THKD x}
  hkdMaxTxSizeL = lens (unTHKD . cppMaxTxSize) $ \pp x -> pp {cppMaxTxSize = THKD x}
  hkdMaxBHSizeL = lens (unTHKD . cppMaxBHSize) $ \pp x -> pp {cppMaxBHSize = THKD x}
  hkdKeyDepositL = lens (unTHKD . cppKeyDeposit) $ \pp x -> pp {cppKeyDeposit = THKD x}
  hkdPoolDepositL = lens (unTHKD . cppPoolDeposit) $ \pp x -> pp {cppPoolDeposit = THKD x}
  hkdEMaxL = lens (unTHKD . cppEMax) $ \pp x -> pp {cppEMax = THKD x}
  hkdNOptL = lens (unTHKD . cppNOpt) $ \pp x -> pp {cppNOpt = THKD x}
  hkdA0L = lens (unTHKD . cppA0) $ \pp x -> pp {cppA0 = THKD x}
  hkdRhoL = lens (unTHKD . cppRho) $ \pp x -> pp {cppRho = THKD x}
  hkdTauL = lens (unTHKD . cppTau) $ \pp x -> pp {cppTau = THKD x}
  hkdProtocolVersionL = notSupportedInThisEraL
  hkdMinPoolCostL = lens (unTHKD . cppMinPoolCost) $ \pp x -> pp {cppMinPoolCost = THKD x}
  ppProtocolVersionL = ppLens . lens cppProtocolVersion (\pp x -> pp {cppProtocolVersion = x})

  ppDG = to (const minBound)
  ppuProtocolVersionL = notSupportedInThisEraL
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance Crypto c => AlonzoEraPParams (BabelEra c) where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens (unTHKD . cppCostModels) $ \pp x -> pp {cppCostModels = THKD x}
  hkdPricesL = lens (unTHKD . cppPrices) $ \pp x -> pp {cppPrices = THKD x}

  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (BabelEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxTxExUnits) $ \pp x ->
      pp {cppMaxTxExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (BabelEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxBlockExUnits) $ \pp x ->
      pp {cppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens (unTHKD . cppMaxValSize) $ \pp x -> pp {cppMaxValSize = THKD x}
  hkdCollateralPercentageL =
    lens (unTHKD . cppCollateralPercentage) $ \pp x -> pp {cppCollateralPercentage = THKD x}
  hkdMaxCollateralInputsL =
    lens (unTHKD . cppMaxCollateralInputs) $ \pp x -> pp {cppMaxCollateralInputs = THKD x}

instance Crypto c => BabbageEraPParams (BabelEra c) where
  hkdCoinsPerUTxOByteL =
    lens (unTHKD . cppCoinsPerUTxOByte) $ \pp x -> pp {cppCoinsPerUTxOByte = THKD x}

instance Crypto c => ConwayEraPParams (BabelEra c) where
  modifiedPPGroups (PParamsUpdate ppu) = conwayModifiedPPGroups ppu
  ppuWellFormed ppu =
    and
      [ -- Numbers
        isValid (/= 0) ppuMaxBBSizeL
      , isValid (/= 0) ppuMaxTxSizeL
      , isValid (/= 0) ppuMaxBHSizeL
      , isValid (/= 0) ppuMaxValSizeL
      , isValid (/= 0) ppuCollateralPercentageL
      , isValid (/= EpochInterval 0) ppuCommitteeMaxTermLengthL
      , isValid (/= EpochInterval 0) ppuGovActionLifetimeL
      , -- Coins
        isValid (/= zero) ppuPoolDepositL
      , isValid (/= zero) ppuGovActionDepositL
      , isValid (/= zero) ppuDRepDepositL
      , ppu /= emptyPParamsUpdate
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate (BabelEra c)) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True
  hkdPoolVotingThresholdsL =
    lens (unTHKD . cppPoolVotingThresholds) $ \pp x -> pp {cppPoolVotingThresholds = THKD x}
  hkdDRepVotingThresholdsL =
    lens (unTHKD . cppDRepVotingThresholds) $ \pp x -> pp {cppDRepVotingThresholds = THKD x}
  hkdCommitteeMinSizeL =
    lens (unTHKD . cppCommitteeMinSize) $ \pp x -> pp {cppCommitteeMinSize = THKD x}
  hkdCommitteeMaxTermLengthL =
    lens (unTHKD . cppCommitteeMaxTermLength) $ \pp x -> pp {cppCommitteeMaxTermLength = THKD x}
  hkdGovActionLifetimeL =
    lens (unTHKD . cppGovActionLifetime) $ \pp x -> pp {cppGovActionLifetime = THKD x}
  hkdGovActionDepositL =
    lens (unTHKD . cppGovActionDeposit) $ \pp x -> pp {cppGovActionDeposit = THKD x}
  hkdDRepDepositL =
    lens (unTHKD . cppDRepDeposit) $ \pp x -> pp {cppDRepDeposit = THKD x}
  hkdDRepActivityL =
    lens (unTHKD . cppDRepActivity) $ \pp x -> pp {cppDRepActivity = THKD x}
  hkdMinFeeRefScriptCostPerByteL =
    lens (unTHKD . cppMinFeeRefScriptCostPerByte) $ \pp x -> pp {cppMinFeeRefScriptCostPerByte = THKD x}

instance Crypto c => ToJSON (ConwayPParams Identity (BabelEra c)) where
  toJSON = object . conwayPParamsPairs
  toEncoding = pairs . mconcat . conwayPParamsPairs

instance Crypto c => EraGov (BabelEra c) where
  type GovState (BabelEra c) = ConwayGovState (BabelEra c)

  curPParamsGovStateL :: Lens' (GovState (BabelEra c)) (PParams (BabelEra c))
  curPParamsGovStateL = cgsCurPParamsL

  prevPParamsGovStateL = cgsPrevPParamsL

  obligationGovState st =
    Obligations
      { oblProposal = foldMap' gasDeposit $ proposalsActions (st ^. cgsProposalsL)
      , oblDRep = Coin 0
      , oblStake = Coin 0
      , oblPool = Coin 0
      }

upgradeBabelPParamsHKD ::
  forall f c.
  ( HKDApplicative f
  , Crypto c
  ) =>
  UpgradeConwayPParams f ->
  PParamsHKD f (PreviousEra (BabelEra c)) ->
  ConwayPParams f (BabelEra c)
upgradeBabelPParamsHKD UpgradeConwayPParams {..} prevPParams =
  ConwayPParams
    { cppMinFeeA = THKD $ prevPParams ^. hkdMinFeeAL
    , cppMinFeeB = THKD $ prevPParams ^. hkdMinFeeBL
    , cppMaxBBSize = THKD $ prevPParams ^. hkdMaxBBSizeL
    , cppMaxTxSize = THKD $ prevPParams ^. hkdMaxTxSizeL
    , cppMaxBHSize = THKD $ prevPParams ^. hkdMaxBHSizeL
    , cppKeyDeposit = THKD $ prevPParams ^. hkdKeyDepositL
    , cppPoolDeposit = THKD $ prevPParams ^. hkdPoolDepositL
    , cppEMax = THKD $ prevPParams ^. hkdEMaxL
    , cppNOpt = THKD $ prevPParams ^. hkdNOptL
    , cppA0 = THKD $ prevPParams ^. hkdA0L
    , cppRho = THKD $ prevPParams ^. hkdRhoL
    , cppTau = THKD $ prevPParams ^. hkdTauL
    , cppProtocolVersion = cppProtocolVersion prevPParams
    , cppMinPoolCost = THKD $ prevPParams ^. hkdMinPoolCostL
    , cppCoinsPerUTxOByte = THKD $ prevPParams ^. hkdCoinsPerUTxOByteL
    , cppCostModels = THKD $ prevPParams ^. hkdCostModelsL
    , cppPrices = THKD $ prevPParams ^. hkdPricesL
    , cppMaxTxExUnits =
        THKD $ hkdMap (Proxy @f) OrdExUnits $ prevPParams ^. hkdMaxTxExUnitsL
    , cppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits $ prevPParams ^. hkdMaxBlockExUnitsL
    , cppMaxValSize = THKD $ prevPParams ^. hkdMaxValSizeL
    , cppCollateralPercentage = THKD $ prevPParams ^. hkdCollateralPercentageL
    , cppMaxCollateralInputs = THKD $ prevPParams ^. hkdMaxCollateralInputsL
    , -- New for Babel
      cppPoolVotingThresholds = THKD ucppPoolVotingThresholds
    , cppDRepVotingThresholds = THKD ucppDRepVotingThresholds
    , cppCommitteeMinSize = THKD ucppCommitteeMinSize
    , cppCommitteeMaxTermLength = THKD ucppCommitteeMaxTermLength
    , cppGovActionLifetime = THKD ucppGovActionLifetime
    , cppGovActionDeposit = THKD ucppGovActionDeposit
    , cppDRepDeposit = THKD ucppDRepDeposit
    , cppDRepActivity = THKD ucppDRepActivity
    , cppMinFeeRefScriptCostPerByte = THKD ucppMinFeeRefScriptCostPerByte
    }