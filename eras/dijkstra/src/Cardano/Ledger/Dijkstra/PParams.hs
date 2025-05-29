{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.PParams () where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..))
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.HKD (HKDFunctor (..))
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Val (Val (..))
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Word (Word16, Word32)
import Lens.Micro (Lens', lens, to, (^.))
import Numeric.Natural (Natural)

instance EraPParams DijkstraEra where
  type PParamsHKD f DijkstraEra = ConwayPParams f DijkstraEra
  type UpgradePParams f DijkstraEra = ()
  type DowngradePParams f DijkstraEra = ()

  applyPPUpdates (PParams pp) (PParamsUpdate ppu) =
    PParams $ conwayApplyPPUpdates pp ppu

  emptyPParamsIdentity = emptyConwayPParams
  emptyPParamsStrictMaybe = emptyConwayPParamsUpdate

  upgradePParamsHKD () = coerce
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
  ppProtocolVersionL = ppLensHKD . lens cppProtocolVersion (\pp x -> pp {cppProtocolVersion = x})

  ppDG = to (const minBound)
  ppuProtocolVersionL = notSupportedInThisEraL
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL
  eraPParams =
    [ ppMinFeeA
    , ppMinFeeB
    , ppMaxBBSize
    , ppMaxTxSize
    , ppMaxBHSize
    , ppKeyDeposit
    , ppPoolDeposit
    , ppEMax
    , ppNOpt
    , ppA0
    , ppRho
    , ppTau
    , ppGovProtocolVersion
    , ppMinPoolCost
    , ppCoinsPerUTxOByte
    , ppCostModels
    , ppPrices
    , ppMaxTxExUnits
    , ppMaxBlockExUnits
    , ppMaxValSize
    , ppCollateralPercentage
    , ppMaxCollateralInputs
    , ppPoolVotingThresholds
    , ppDRepVotingThresholds
    , ppCommitteeMinSize
    , ppCommitteeMaxTermLength
    , ppGovActionLifetime
    , ppGovActionDeposit
    , ppDRepDeposit
    , ppDRepActivity
    , ppMinFeeRefScriptCostPerByte
    ]

instance AlonzoEraPParams DijkstraEra where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens (unTHKD . cppCostModels) $ \pp x -> pp {cppCostModels = THKD x}
  hkdPricesL = lens (unTHKD . cppPrices) $ \pp x -> pp {cppPrices = THKD x}

  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxTxExUnits) $ \pp x ->
      pp {cppMaxTxExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxBlockExUnits) $ \pp x ->
      pp {cppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdMaxValSizeL =
    lens (asNaturalHKD @f @Word32 . (unTHKD . cppMaxValSize)) $
      \pp x -> pp {cppMaxValSize = THKD (asBoundedIntegralHKD @f @Natural @Word32 x)}
  hkdCollateralPercentageL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdCollateralPercentageL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppCollateralPercentage)) $
      \pp x -> pp {cppCollateralPercentage = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
  hkdMaxCollateralInputsL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdMaxCollateralInputsL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppMaxCollateralInputs)) $
      \pp x -> pp {cppMaxCollateralInputs = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}

instance BabbageEraPParams DijkstraEra where
  hkdCoinsPerUTxOByteL =
    lens (unTHKD . cppCoinsPerUTxOByte) $ \pp x -> pp {cppCoinsPerUTxOByte = THKD x}

instance ConwayEraPParams DijkstraEra where
  modifiedPPGroups (PParamsUpdate ppu) = conwayModifiedPPGroups ppu
  ppuWellFormed _pv ppu =
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
      , isValid ((/= zero) . unCoinPerByte) ppuCoinsPerUTxOByteL
      , ppu /= emptyPParamsUpdate
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate DijkstraEra) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True
  hkdPoolVotingThresholdsL =
    lens (unTHKD . cppPoolVotingThresholds) $ \pp x -> pp {cppPoolVotingThresholds = THKD x}
  hkdDRepVotingThresholdsL =
    lens (unTHKD . cppDRepVotingThresholds) $ \pp x -> pp {cppDRepVotingThresholds = THKD x}
  hkdCommitteeMinSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdCommitteeMinSizeL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppCommitteeMinSize)) $
      \pp x -> pp {cppCommitteeMinSize = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
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
