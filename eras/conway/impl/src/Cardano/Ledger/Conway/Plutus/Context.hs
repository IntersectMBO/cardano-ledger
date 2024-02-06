{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Plutus.Context (
  pparamUpdateToData,
  pparamUpdateFromData,
  conwayPParam,
  conwayPParamMap,
  ConwayEraPlutusTxInfo (..),
) where

import Cardano.Ledger.Alonzo.PParams (
  ppuCollateralPercentageL,
  ppuCostModelsL,
  ppuMaxBlockExUnitsL,
  ppuMaxCollateralInputsL,
  ppuMaxTxExUnitsL,
  ppuMaxValSizeL,
  ppuPricesL,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Babbage.PParams (CoinPerByte (..), ppuCoinsPerUTxOByteL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  ppuCommitteeMaxTermLengthL,
  ppuCommitteeMinSizeL,
  ppuDRepActivityL,
  ppuDRepDepositL,
  ppuDRepVotingThresholdsL,
  ppuGovActionDepositL,
  ppuGovActionLifetimeL,
  ppuMinFeeRefScriptCostPerByteL,
  ppuPoolVotingThresholdsL,
 )
import Cardano.Ledger.Core (
  EraPParams (..),
  PParam (..),
  PParamsUpdate,
  emptyPParamsUpdate,
  makePParamMap,
  ppuA0L,
  ppuEMaxL,
  ppuKeyDepositL,
  ppuMaxBBSizeL,
  ppuMaxBHSizeL,
  ppuMaxTxSizeL,
  ppuMinFeeAL,
  ppuMinFeeBL,
  ppuMinPoolCostL,
  ppuNOptL,
  ppuPoolDepositL,
  ppuRhoL,
  ppuTauL,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro ((&), (.~), (^.))
import PlutusLedgerApi.Common (Data (..))
import qualified PlutusLedgerApi.V3 as PV3

-- ====================================================================
-- Generic, Table (Map Word (PParam era)) driven translators for
-- PParamUpdate, A table for some era, will make instances for (PParamUpdate era) for that Era.

-- | Translate a PParamUpdate to Data, using the PParamMap.
-- The PParamUpdate is Basically (Map (tag t) (Some (StrictMaybe t)))
-- The idea is to only store (Map (tag t) (Some t)) for just the components
-- that had (SJust t), and to leave out the other tags. To transform this
-- back to a (Map (tag t) (Some t)), we start with a full map where everything is
-- SNothing, and then override the tags we find in the data with (SJust t)
-- That is what happens in pparamUpdateFromData below
pparamUpdateToData :: Map Word (PParam era) -> PParamsUpdate era -> Data
pparamUpdateToData pparamMap ppu = Map (Map.foldrWithKey' toMapPair [] pparamMap)
  where
    toMapPair tag (PParam _ ppuL) ans =
      case ppu ^. ppuL of
        SNothing -> ans
        SJust t -> (I (toInteger @Word tag), toPlutusData t) : ans

-- | Translate Data to a PParamUpdate, using the PParamMap.
pparamUpdateFromData :: EraPParams era => Map Word (PParam era) -> Data -> Maybe (PParamsUpdate era)
pparamUpdateFromData pparamMap (Map pairs) =
  fst
    <$> foldlM accum (emptyPParamsUpdate, pparamMap) pairs
  where
    accum (!ppu, !leftOverMap) (key, value) = do
      tg <- fromPlutusData key
      PParam _ ppuL <- Map.lookup tg leftOverMap
      val <- fromPlutusData value
      pure (ppu & ppuL .~ SJust val, Map.delete tg leftOverMap)
pparamUpdateFromData _ _ = Nothing

-- ===================================================================
-- ToPlutusData instances necessary for (PParamUpdate (CowayEra c))

instance ToPlutusData PoolVotingThresholds where
  toPlutusData x =
    List
      [ toPlutusData (pvtMotionNoConfidence x)
      , toPlutusData (pvtCommitteeNormal x)
      , toPlutusData (pvtCommitteeNoConfidence x)
      , toPlutusData (pvtHardForkInitiation x)
      , toPlutusData (pvtPPSecurityGroup x)
      ]
  fromPlutusData (List [a, b, c, d, e]) =
    PoolVotingThresholds
      <$> fromPlutusData a
      <*> fromPlutusData b
      <*> fromPlutusData c
      <*> fromPlutusData d
      <*> fromPlutusData e
  fromPlutusData _ = Nothing

instance ToPlutusData DRepVotingThresholds where
  toPlutusData x =
    List
      [ toPlutusData (dvtMotionNoConfidence x)
      , toPlutusData (dvtCommitteeNormal x)
      , toPlutusData (dvtCommitteeNoConfidence x)
      , toPlutusData (dvtUpdateToConstitution x)
      , toPlutusData (dvtHardForkInitiation x)
      , toPlutusData (dvtPPNetworkGroup x)
      , toPlutusData (dvtPPEconomicGroup x)
      , toPlutusData (dvtPPTechnicalGroup x)
      , toPlutusData (dvtPPGovGroup x)
      , toPlutusData (dvtTreasuryWithdrawal x)
      ]
  fromPlutusData (List [a, b, c, d, e, f, g, h, i, j]) =
    DRepVotingThresholds
      <$> fromPlutusData a
      <*> fromPlutusData b
      <*> fromPlutusData c
      <*> fromPlutusData d
      <*> fromPlutusData e
      <*> fromPlutusData f
      <*> fromPlutusData g
      <*> fromPlutusData h
      <*> fromPlutusData i
      <*> fromPlutusData j
  fromPlutusData _ = Nothing

instance ToPlutusData CoinPerByte where
  toPlutusData (CoinPerByte c) = toPlutusData @Coin c
  fromPlutusData x = CoinPerByte <$> fromPlutusData @Coin x

-- ==========================================================

-- | A Map for the Conway era
conwayPParamMap :: ConwayEraPParams era => Map Word (PParam era)
conwayPParamMap = makePParamMap conwayPParam

-- | A list for the Conway era, other eras may have different tags and lenses.
conwayPParam :: ConwayEraPParams era => [PParam era]
conwayPParam =
  [ PParam 0 ppuMinFeeAL
  , PParam 1 ppuMinFeeBL
  , PParam 2 ppuMaxBBSizeL
  , PParam 3 ppuMaxTxSizeL
  , PParam 4 ppuMaxBHSizeL
  , PParam 5 ppuKeyDepositL
  , PParam 6 ppuPoolDepositL
  , PParam 7 ppuEMaxL
  , PParam 8 ppuNOptL
  , PParam 9 ppuA0L
  , PParam 10 ppuRhoL
  , PParam 11 ppuTauL
  , -- Missing in Conway [12,13,14,15]
    PParam 16 ppuMinPoolCostL
  , PParam 17 ppuCoinsPerUTxOByteL
  , PParam 18 ppuCostModelsL
  , PParam 19 ppuPricesL
  , PParam 20 ppuMaxTxExUnitsL
  , PParam 21 ppuMaxBlockExUnitsL
  , PParam 22 ppuMaxValSizeL
  , PParam 23 ppuCollateralPercentageL
  , PParam 24 ppuMaxCollateralInputsL
  , -- New to Conway [25 .. 33]
    PParam 25 ppuPoolVotingThresholdsL
  , PParam 26 ppuDRepVotingThresholdsL
  , PParam 27 ppuCommitteeMinSizeL
  , PParam 28 ppuCommitteeMaxTermLengthL
  , PParam 29 ppuGovActionLifetimeL
  , PParam 30 ppuGovActionDepositL
  , PParam 31 ppuDRepDepositL
  , PParam 32 ppuDRepActivityL
  , PParam 33 ppuMinFeeRefScriptCostPerByteL
  ]

-- ===========================================================
-- A class to compute the changed parameters in the TxInfo
-- given a ToPlutusData instance for PParamsUpdate

class
  ( ToPlutusData (PParamsUpdate era)
  , EraPlutusTxInfo l era
  ) =>
  ConwayEraPlutusTxInfo (l :: Language) era
  where
  toPlutusChangedParameters :: proxy l -> PParamsUpdate era -> PV3.ChangedParameters
