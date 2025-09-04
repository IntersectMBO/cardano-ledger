{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides the necessary instances of `HasSpec`
--   and `HasSimpleRep` for the components of PParams. It hides
--   the fact that (PParams era) can have different underlying 'data' types
--   in each era, and provides Term selector functions
--   (e.g. minFeeA_, minFeeB_, etc.) for every PParam field (in every era).
--   The class EraSpecPParams provides this era parametric abstraction.
--   and instances of EraSpecPParams are defined here.
module Test.Cardano.Ledger.Constrained.Conway.Instances.PParams (
  EraSpecPParams (..),
  SimplePParams (..),
  SimplePPUpdate (..),
  simplePParamsSpec,
  cSNothing_,
  cSJust_,
  succV_,
  minFeeA_,
  minFeeB_,
  maxBBSize_,
  maxTxSize_,
  maxBHSize_,
  keyDeposit_,
  poolDeposit_,
  eMax_,
  nOpt_,
  a0_,
  rho_,
  tau_,
  decentral_,
  protocolVersion_,
  minUTxOValue_,
  minPoolCost_,
  coinsPerUTxOWord_,
  costModels_,
  prices_,
  maxTxExUnits_,
  maxBlockExUnits_,
  maxValSize_,
  collateralPercentage_,
  maxCollateralInputs_,
  coinsPerUTxOByte_,
  poolVotingThresholds_,
  drepVotingThresholds_,
  committeeMinSize_,
  committeeMaxTermLength_,
  govActionLifetime_,
  govActionDeposit_,
  dRepDeposit_,
  dRepActivity_,
  minFeeRefScriptCostPerByte_,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Shelley (ShelleyEra)
import Constrained.API
import Data.Word
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Constrained.Conway.Instances.Basic
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()

-- ============================================

instance EraSpecPParams ShelleyEra where
  subsetToPP = liftShelley
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraSpecPParams AllegraEra where
  subsetToPP = liftShelley
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraSpecPParams MaryEra where
  subsetToPP x = liftShelley x
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraSpecPParams AlonzoEra where
  subsetToPP x = (liftAlonzo x . liftShelley) x
  ppToSubset x = dropAlonzo x $ dropAtMost6 x $ dropShelley x
  updateToPPU x = (uLiftAlonzo x . uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropAlonzo x $ uDropProtVer x $ uDropShelley x

instance EraSpecPParams BabbageEra where
  subsetToPP x = (liftBabbage x . liftAlonzo x . liftShelley) x
  ppToSubset x = dropBabbage x $ dropAlonzo x $ dropShelley x
  updateToPPU x = (uLiftBabbage x . uLiftAlonzo x . uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropBabbage x $ uDropAlonzo x $ uDropProtVer x $ uDropShelley x

instance EraSpecPParams ConwayEra where
  subsetToPP x = (liftConway x . liftBabbage x . liftAlonzo x . liftShelley) x
  ppToSubset x = dropConway x $ dropBabbage x $ dropAlonzo x $ dropShelley x
  updateToPPU x = (uLiftConway x . uLiftBabbage x . uLiftAlonzo x . uLiftShelley) x
  ppuToUpdate x = uDropConway x $ uDropBabbage x $ uDropAlonzo x $ uDropShelley x

-- ====================================================================================
-- Since the transition from one Era to the next Era, we add or drop some of the
-- parameters. This we need some functions that lift and drop from one era to another
-- which add (or drop) the appropriate parameters.

unitI :: UnitInterval
unitI = makeUnitInterval 0 1

dropAtMost6 ::
  (EraPParams era, AtMostEra "Alonzo" era) => PParams era -> SimplePParams era -> SimplePParams era
dropAtMost6 pp x = x {decentral = pp ^. ppDL}

dropAtMost4 ::
  (EraPParams era, AtMostEra "Mary" era, AtMostEra "Alonzo" era) =>
  PParams era ->
  SimplePParams era ->
  SimplePParams era
dropAtMost4 pp x =
  x
    { minUTxOValue = pp ^. ppMinUTxOValueL
    , decentral = pp ^. ppDL
    }

-- Magic functions used to implement (EraSpecPParams era). Example use for Conway
--  subsetToPP x = (toPP . liftConway x . liftBabbage x . liftAlonzo x . liftShelley) x
--  ppToSubset x = dropConway x $ dropAlonzo x $ dropAlonzo x $ dropShelley x
dropShelley :: EraPParams era => PParams era -> SimplePParams era
dropShelley pp =
  SimplePParams
    { minFeeA = pp ^. ppMinFeeAL
    , minFeeB = pp ^. ppMinFeeBL
    , maxBBSize = pp ^. ppMaxBBSizeL
    , maxTxSize = pp ^. ppMaxTxSizeL
    , maxBHSize = fromIntegral (pp ^. ppMaxBHSizeL)
    , keyDeposit = pp ^. ppKeyDepositL
    , poolDeposit = pp ^. ppPoolDepositL
    , eMax = pp ^. ppEMaxL
    , nOpt = pp ^. ppNOptL
    , a0 = pp ^. ppA0L
    , rho = pp ^. ppRhoL
    , tau = pp ^. ppTauL
    , protocolVersion = pp ^. ppProtocolVersionL
    , minPoolCost = pp ^. ppMinPoolCostL
    , -- \^ In Shelley these are given default values
      decentral = unitI -- in some Eras, dropAtMost6 will over ride this default
    , minUTxOValue = Coin 0 -- in some Eras, dropAtMost4 will over ride this default
    , coinsPerUTxOWord = Coin 0
    , costModels = mempty
    , prices = makePrices 0 0
    , maxTxExUnits = mempty
    , maxBlockExUnits = mempty
    , maxValSize = 0
    , collateralPercentage = 0
    , maxCollateralInputs = 0
    , coinsPerUTxOByte = Coin 0
    , poolVotingThresholds = PoolVotingThresholds unitI unitI unitI unitI unitI
    , drepVotingThresholds =
        DRepVotingThresholds unitI unitI unitI unitI unitI unitI unitI unitI unitI unitI
    , committeeMinSize = 0
    , committeeMaxTermLength = EpochInterval 0
    , govActionLifetime = EpochInterval 0
    , govActionDeposit = Coin 0
    , dRepDeposit = Coin 0
    , dRepActivity = EpochInterval 0
    , minFeeRefScriptCostPerByte = makeNonNegativeInterval 0 1
    }

dropAlonzo :: AlonzoEraPParams era => PParams era -> SimplePParams era -> SimplePParams era
dropAlonzo pp psub =
  psub
    { coinsPerUTxOWord = Coin 0
    , costModels = pp ^. ppCostModelsL
    , prices = pp ^. ppPricesL
    , maxTxExUnits = pp ^. ppMaxTxExUnitsL
    , maxBlockExUnits = pp ^. ppMaxBlockExUnitsL
    , maxValSize = pp ^. ppMaxValSizeL
    , collateralPercentage = pp ^. ppCollateralPercentageL
    }

dropBabbage :: BabbageEraPParams era => PParams era -> SimplePParams era -> SimplePParams era
dropBabbage pp psub =
  psub {coinsPerUTxOByte = unCoinPerByte (pp ^. ppCoinsPerUTxOByteL)}

dropConway :: ConwayEraPParams era => PParams era -> SimplePParams era -> SimplePParams era
dropConway pp psub =
  psub
    { poolVotingThresholds = pp ^. ppPoolVotingThresholdsL
    , drepVotingThresholds = pp ^. ppDRepVotingThresholdsL
    , committeeMinSize = pp ^. ppCommitteeMinSizeL
    , committeeMaxTermLength = pp ^. ppCommitteeMaxTermLengthL
    , govActionLifetime = pp ^. ppGovActionLifetimeL
    , govActionDeposit = pp ^. ppGovActionDepositL
    , dRepDeposit = pp ^. ppDRepDepositL
    , dRepActivity = pp ^. ppDRepActivityL
    , minFeeRefScriptCostPerByte = pp ^. ppMinFeeRefScriptCostPerByteL
    }

-- ========================

liftShelley :: EraPParams era => SimplePParams era -> PParams era
liftShelley pps =
  emptyPParams
    & ppMinFeeAL .~ (minFeeA pps)
    & ppMinFeeBL .~ (minFeeB pps)
    & ppMaxBBSizeL .~ (maxBBSize pps)
    & ppMaxTxSizeL .~ (maxTxSize pps)
    & ppMaxBHSizeL .~ (fromIntegral (maxBHSize pps))
    & ppKeyDepositL .~ (keyDeposit pps)
    & ppPoolDepositL .~ (poolDeposit pps)
    & ppEMaxL .~ (eMax pps)
    & ppNOptL .~ (nOpt pps)
    & ppA0L .~ (a0 pps)
    & ppRhoL .~ (rho pps)
    & ppTauL .~ (tau pps)
    -- & ppDL .~ (decentral pps)
    & ppProtocolVersionL .~ (protocolVersion pps)
    -- & ppMinUTxOValueL .~ (minUTxOValue pps)
    & ppMinPoolCostL .~ (minPoolCost pps)

liftAlonzo :: AlonzoEraPParams era => SimplePParams era -> PParams era -> PParams era
liftAlonzo pps pp =
  pp -- & ppCoinsPerUTxOWordL .~  CoinPerWord (coinsPerUTxOWord pps)
    & ppCostModelsL .~ (costModels pps)
    & ppPricesL .~ (prices pps)
    & ppMaxTxExUnitsL .~ (maxTxExUnits pps)
    & ppMaxBlockExUnitsL .~ (maxBlockExUnits pps)
    & ppMaxValSizeL .~ (maxValSize pps)
    & ppCollateralPercentageL .~ (collateralPercentage pps)
    & ppMaxCollateralInputsL .~ (maxCollateralInputs pps)

liftBabbage :: BabbageEraPParams era => SimplePParams era -> PParams era -> PParams era
liftBabbage pps pp = pp & ppCoinsPerUTxOByteL .~ CoinPerByte (coinsPerUTxOByte pps)

liftConway :: ConwayEraPParams era => SimplePParams era -> PParams era -> PParams era
liftConway pps pp =
  pp
    & ppPoolVotingThresholdsL .~ (poolVotingThresholds pps)
    & ppDRepVotingThresholdsL .~ (drepVotingThresholds pps)
    & ppCommitteeMinSizeL .~ (committeeMinSize pps)
    & ppCommitteeMaxTermLengthL .~ (committeeMaxTermLength pps)
    & ppGovActionLifetimeL .~ (govActionLifetime pps)
    & ppGovActionDepositL .~ (govActionDeposit pps)
    & ppDRepDepositL .~ (dRepDeposit pps)
    & ppDRepActivityL .~ (dRepActivity pps)
    & ppMinFeeRefScriptCostPerByteL .~ (minFeeRefScriptCostPerByte pps)

-- ================================================================

uDropShelley :: EraPParams era => PParamsUpdate era -> SimplePPUpdate
uDropShelley pp =
  SimplePPUpdate
    { uminFeeA = pp ^. ppuMinFeeAL
    , uminFeeB = pp ^. ppuMinFeeBL
    , umaxBBSize = pp ^. ppuMaxBBSizeL
    , umaxTxSize = pp ^. ppuMaxTxSizeL
    , umaxBHSize = fromIntegral <$> (pp ^. ppuMaxBHSizeL)
    , ukeyDeposit = pp ^. ppuKeyDepositL
    , upoolDeposit = pp ^. ppuPoolDepositL
    , ueMax = pp ^. ppuEMaxL
    , unOpt = pp ^. ppuNOptL
    , ua0 = pp ^. ppuA0L
    , urho = pp ^. ppuRhoL
    , utau = pp ^. ppuTauL
    , uminPoolCost = pp ^. ppuMinPoolCostL
    , -- In Shelley these are given SNothing values
      udecentral = SNothing -- in some Eras, dropAtMost6 will over ride this default
    , uprotocolVersion = SNothing
    , uminUTxOValue = SNothing -- in some Eras, dropAtMost4 will over ride this default
    , ucoinsPerUTxOWord = SNothing
    , ucostModels = SNothing
    , uprices = SNothing
    , umaxTxExUnits = SNothing
    , umaxBlockExUnits = SNothing
    , umaxValSize = SNothing
    , ucollateralPercentage = SNothing
    , umaxCollateralInputs = SNothing
    , ucoinsPerUTxOByte = SNothing
    , upoolVotingThresholds = SNothing
    , udrepVotingThresholds = SNothing
    , ucommitteeMinSize = SNothing
    , ucommitteeMaxTermLength = SNothing
    , ugovActionLifetime = SNothing
    , ugovActionDeposit = SNothing
    , udRepDeposit = SNothing
    , udRepActivity = SNothing
    , uminFeeRefScriptCostPerByte = SNothing
    }

uDropProtVer ::
  (EraPParams era, AtMostEra "Babbage" era) => PParamsUpdate era -> SimplePPUpdate -> SimplePPUpdate
uDropProtVer pp psub = psub {uprotocolVersion = pp ^. ppuProtocolVersionL}

uDropAlonzo :: AlonzoEraPParams era => PParamsUpdate era -> SimplePPUpdate -> SimplePPUpdate
uDropAlonzo pp psub =
  psub
    { -- ucoinsPerUTxOWord = unCoinPerWord <$> pp ^. ppuCoinsPerUTxOWordL
      ucostModels = pp ^. ppuCostModelsL
    , uprices = pp ^. ppuPricesL
    , umaxTxExUnits = pp ^. ppuMaxTxExUnitsL
    , umaxBlockExUnits = pp ^. ppuMaxBlockExUnitsL
    , umaxValSize = pp ^. ppuMaxValSizeL
    , ucollateralPercentage = pp ^. ppuCollateralPercentageL
    , umaxCollateralInputs = pp ^. ppuMaxCollateralInputsL
    }

uDropBabbage :: BabbageEraPParams era => PParamsUpdate era -> SimplePPUpdate -> SimplePPUpdate
uDropBabbage pp psub =
  psub {ucoinsPerUTxOByte = unCoinPerByte <$> (pp ^. ppuCoinsPerUTxOByteL)}

uDropConway :: ConwayEraPParams era => PParamsUpdate era -> SimplePPUpdate -> SimplePPUpdate
uDropConway pp psub =
  psub
    { upoolVotingThresholds = pp ^. ppuPoolVotingThresholdsL
    , udrepVotingThresholds = pp ^. ppuDRepVotingThresholdsL
    , ucommitteeMinSize = pp ^. ppuCommitteeMinSizeL
    , ucommitteeMaxTermLength = pp ^. ppuCommitteeMaxTermLengthL
    , ugovActionLifetime = pp ^. ppuGovActionLifetimeL
    , ugovActionDeposit = pp ^. ppuGovActionDepositL
    , udRepDeposit = pp ^. ppuDRepDepositL
    , udRepActivity = pp ^. ppuDRepActivityL
    , uminFeeRefScriptCostPerByte = pp ^. ppuMinFeeRefScriptCostPerByteL
    }

uLiftShelley :: EraPParams era => SimplePPUpdate -> PParamsUpdate era
uLiftShelley pps =
  emptyPParamsUpdate
    & ppuMinFeeAL .~ (uminFeeA pps)
    & ppuMinFeeBL .~ (uminFeeB pps)
    & ppuMaxBBSizeL .~ (umaxBBSize pps)
    & ppuMaxTxSizeL .~ (umaxTxSize pps)
    & ppuMaxBHSizeL .~ (fromIntegral <$> (umaxBHSize pps))
    & ppuKeyDepositL .~ (ukeyDeposit pps)
    & ppuPoolDepositL .~ (upoolDeposit pps)
    & ppuEMaxL .~ (ueMax pps)
    & ppuNOptL .~ (unOpt pps)
    & ppuA0L .~ (ua0 pps)
    & ppuRhoL .~ (urho pps)
    & ppuTauL .~ (utau pps)
    & ppuMinPoolCostL .~ (uminPoolCost pps)
    & ppuMinPoolCostL .~ (uminPoolCost pps)

uLiftProtVer ::
  (EraPParams era, AtMostEra "Babbage" era) =>
  SimplePPUpdate -> PParamsUpdate era -> PParamsUpdate era
uLiftProtVer pps pp = pp & ppuProtocolVersionL .~ (uprotocolVersion pps)

uLiftAlonzo :: AlonzoEraPParams era => SimplePPUpdate -> PParamsUpdate era -> PParamsUpdate era
uLiftAlonzo pps pp =
  pp
    & ppuCostModelsL .~ (ucostModels pps)
    & ppuPricesL .~ (uprices pps)
    & ppuMaxTxExUnitsL .~ (umaxTxExUnits pps)
    & ppuMaxBlockExUnitsL .~ (umaxBlockExUnits pps)
    & ppuMaxValSizeL .~ (umaxValSize pps)
    & ppuCollateralPercentageL .~ (ucollateralPercentage pps)
    & ppuMaxCollateralInputsL .~ (umaxCollateralInputs pps)

uLiftBabbage :: BabbageEraPParams era => SimplePPUpdate -> PParamsUpdate era -> PParamsUpdate era
uLiftBabbage pps pp = pp & ppuCoinsPerUTxOByteL .~ (CoinPerByte <$> (ucoinsPerUTxOByte pps))

uLiftConway :: ConwayEraPParams era => SimplePPUpdate -> PParamsUpdate era -> PParamsUpdate era
uLiftConway pps pp =
  pp
    & ppuPoolVotingThresholdsL .~ (upoolVotingThresholds pps)
    & ppuDRepVotingThresholdsL .~ (udrepVotingThresholds pps)
    & ppuCommitteeMinSizeL .~ (ucommitteeMinSize pps)
    & ppuCommitteeMaxTermLengthL .~ (ucommitteeMaxTermLength pps)
    & ppuGovActionLifetimeL .~ (ugovActionLifetime pps)
    & ppuGovActionDepositL .~ (ugovActionDeposit pps)
    & ppuDRepDepositL .~ (udRepDeposit pps)
    & ppuDRepActivityL .~ (udRepActivity pps)
    & ppuMinFeeRefScriptCostPerByteL .~ (uminFeeRefScriptCostPerByte pps)

-- ============================================================================
-- Term Selectors for SimplePParams

minFeeA_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
minFeeA_ simplepp = sel @0 simplepp

minFeeB_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
minFeeB_ simplepp = sel @1 simplepp

maxBBSize_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Word32
maxBBSize_ simplepp = sel @2 simplepp

maxTxSize_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Word32
maxTxSize_ simplepp = sel @3 simplepp

maxBHSize_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Word32
maxBHSize_ simplepp = sel @4 simplepp

keyDeposit_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
keyDeposit_ simplepp = sel @5 simplepp

poolDeposit_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
poolDeposit_ simplepp = sel @6 simplepp

eMax_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term EpochInterval
eMax_ simplepp = sel @7 simplepp

nOpt_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Word16
nOpt_ simplepp = sel @8 simplepp

a0_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term NonNegativeInterval
a0_ simplepp = sel @9 simplepp

rho_ :: EraSpecPParams era => Term (SimplePParams era) -> Term UnitInterval
rho_ simplepp = sel @10 simplepp

tau_ :: EraSpecPParams era => Term (SimplePParams era) -> Term UnitInterval
tau_ simplepp = sel @11 simplepp

decentral_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term UnitInterval
decentral_ simplepp = sel @12 simplepp

protocolVersion_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term ProtVer
protocolVersion_ simplepp = sel @13 simplepp

minUTxOValue_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Coin
minUTxOValue_ simplepp = sel @14 simplepp

minPoolCost_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
minPoolCost_ simplepp = sel @15 simplepp

coinsPerUTxOWord_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Coin
coinsPerUTxOWord_ simplepp = sel @16 simplepp

costModels_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term CostModels
costModels_ simplepp = sel @17 simplepp

prices_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Prices
prices_ simplepp = sel @18 simplepp

maxTxExUnits_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term ExUnits
maxTxExUnits_ simplepp = sel @19 simplepp

maxBlockExUnits_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term ExUnits
maxBlockExUnits_ simplepp = sel @20 simplepp

maxValSize_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Natural
maxValSize_ simplepp = sel @21 simplepp

collateralPercentage_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Natural
collateralPercentage_ simplepp = sel @22 simplepp

maxCollateralInputs_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Natural
maxCollateralInputs_ simplepp = sel @23 simplepp

coinsPerUTxOByte_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Coin
coinsPerUTxOByte_ simplepp = sel @24 simplepp

poolVotingThresholds_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term PoolVotingThresholds
poolVotingThresholds_ simplepp = sel @25 simplepp

drepVotingThresholds_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term DRepVotingThresholds
drepVotingThresholds_ simplepp = sel @26 simplepp

committeeMinSize_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Natural
committeeMinSize_ simplepp = sel @27 simplepp

committeeMaxTermLength_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term EpochInterval
committeeMaxTermLength_ simplepp = sel @28 simplepp

govActionLifetime_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term EpochInterval
govActionLifetime_ simplepp = sel @29 simplepp

govActionDeposit_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term Coin
govActionDeposit_ simplepp = sel @30 simplepp

dRepDeposit_ :: EraSpecPParams era => Term (SimplePParams era) -> Term Coin
dRepDeposit_ simplepp = sel @31 simplepp

dRepActivity_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term EpochInterval
dRepActivity_ simplepp = sel @32 simplepp

minFeeRefScriptCostPerByte_ ::
  EraSpecPParams era => Term (SimplePParams era) -> Term NonNegativeInterval
minFeeRefScriptCostPerByte_ simplepp = sel @33 simplepp

-- =======================================================================

-- | A sample of how to constrain (PParams era) for every Era, by writing a specification for SimplePParams.
--   Constrained but not applicable fields (for that era) are elided in the result.
--   Missing fields are left unconstrained and will appear as random values in the result.
--   This can easily be lifted to PParams: see Test.Cardano.Ledger.Constrained.Conway.PParams(pparamsSpec)
simplePParamsSpec ::
  forall era. EraSpecPParams era => Specification (SimplePParams era)
simplePParamsSpec = constrained $ \pp ->
  [ assert $ protocolVersion_ pp ==. lit (ProtVer (natVersion @10) 0)
  , assert $ maxBBSize_ pp /=. lit 0
  , assert $ maxTxSize_ pp /=. lit 0
  , assert $ maxBHSize_ pp /=. lit 0
  , assert $ maxValSize_ pp /=. lit 0
  , assert $ collateralPercentage_ pp /=. lit 0
  , assert $ committeeMaxTermLength_ pp /=. lit (EpochInterval 0)
  , assert $ govActionLifetime_ pp /=. lit (EpochInterval 0)
  , assert $ poolDeposit_ pp /=. lit mempty
  , assert $ govActionDeposit_ pp /=. lit mempty
  , assert $ dRepDeposit_ pp /=. lit mempty
  , match (eMax_ pp) (\epochInterval -> lit 0 <. epochInterval)
  , assert $ costModels_ pp ==. lit mempty -- This makes examples soo much more readable.
  ]
