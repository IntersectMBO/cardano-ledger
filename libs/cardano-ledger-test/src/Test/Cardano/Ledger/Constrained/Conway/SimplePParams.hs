{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- RecordWildCards cause name shadowing warnings in ghc-8.10.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

-- | This module provides the necessary instances of `HasSpec`
--   and `HasSimpleRep` for the components of PParams.
module Test.Cardano.Ledger.Constrained.Conway.SimplePParams where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Rules
import Constrained hiding (Value)
import Constrained.Univ ()
import Control.Monad.Trans.Fail.String
import Data.Maybe
import Data.Ratio ((%))
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import Numeric.Natural (Natural)
import System.Random
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.Cardano.Ledger.Generic.Proof (Reflect (..))
import Test.QuickCheck hiding (Args, Fun, forAll)

-- =============================================================
-- Making Intervals based on Ratios, These can fail be careful

makePrices :: Integer -> Integer -> Prices
makePrices x y = Prices (fromJust (boundRational (x % 1))) (fromJust (boundRational (y % 1)))

makeUnitInterval :: Integer -> Integer -> UnitInterval
makeUnitInterval i j = fromJust (boundRational (i % j))

makeNonNegativeInterval :: Integer -> Integer -> NonNegativeInterval
makeNonNegativeInterval i j = fromJust (boundRational (i % j))

-- =============================================================
-- HasSpec instances for types that appear in PParams

instance HasSimpleRep Coin where
  type SimpleRep Coin = Word64
  toSimpleRep (Coin i) = case integerToWord64 i of
    Nothing -> error "The impossible happened in toSimpleRep for `Coin`"
    Just w -> w
  fromSimpleRep = word64ToCoin
instance BaseUniverse fn => HasSpec fn Coin
instance BaseUniverse fn => OrdLike fn Coin
instance BaseUniverse fn => NumLike fn Coin
instance BaseUniverse fn => Foldy fn Coin where
  genList s s' = map fromSimpleRep <$> genList @fn @Word64 (toSimpleRepSpec s) (toSimpleRepSpec s')
  theAddFn = addFn
  theZero = Coin 0

-- TODO: This is hack to get around the need for `Num` in `NumLike`. We should possibly split
-- this up so that `NumLike` has its own addition etc. instead?
deriving via Integer instance Num Coin

instance HasSimpleRep (StrictMaybe a)
instance (HasSpec fn a, IsNormalType a) => HasSpec fn (StrictMaybe a)

cSNothing_ :: (HasSpec fn a, IsNormalType a) => Term fn (StrictMaybe a)
cSNothing_ = con @"SNothing" (lit ())

cSJust_ :: (HasSpec fn a, IsNormalType a) => Term fn a -> Term fn (StrictMaybe a)
cSJust_ = con @"SJust"

instance HasSimpleRep EpochInterval
instance BaseUniverse fn => OrdLike fn EpochInterval
instance BaseUniverse fn => HasSpec fn EpochInterval

-- TODO: once you start adding interesting functions on these to the
-- function universe this instance has to become more complicated
instance BaseUniverse fn => HasSpec fn UnitInterval where
  type TypeSpec fn UnitInterval = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

-- TODO: once you start adding interesting functions on these to the
-- function universe this instance has to become more complicated
instance BaseUniverse fn => HasSpec fn NonNegativeInterval where
  type TypeSpec fn NonNegativeInterval = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep CostModels
instance BaseUniverse fn => HasSpec fn CostModels where
  type TypeSpec fn CostModels = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep Prices
instance BaseUniverse fn => HasSpec fn Prices

instance HasSimpleRep ExUnits where
  type SimpleRep ExUnits = SimpleRep (Natural, Natural)
  fromSimpleRep = uncurry ExUnits . fromSimpleRep
  toSimpleRep (ExUnits a b) = toSimpleRep (a, b)
instance BaseUniverse fn => HasSpec fn ExUnits

instance HasSimpleRep OrdExUnits where
  type SimpleRep OrdExUnits = SimpleRep ExUnits
  fromSimpleRep = OrdExUnits . fromSimpleRep
  toSimpleRep = toSimpleRep . unOrdExUnits
instance BaseUniverse fn => HasSpec fn OrdExUnits

instance HasSimpleRep PoolVotingThresholds
instance BaseUniverse fn => HasSpec fn PoolVotingThresholds

instance HasSimpleRep DRepVotingThresholds
instance BaseUniverse fn => HasSpec fn DRepVotingThresholds

instance HasSimpleRep ProtVer
instance BaseUniverse fn => HasSpec fn ProtVer

-- We do this like this to get the right bounds for `VersionRep`
-- while ensuring that we don't have to add instances for e.g. `Num`
-- to version.
newtype VersionRep = VersionRep Word8
  deriving (Show, Eq, Ord, Num, Random, Arbitrary, Integral, Real, Enum) via Word8
instance BaseUniverse fn => HasSpec fn VersionRep where
  type TypeSpec fn VersionRep = NumSpec fn VersionRep
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
instance Bounded VersionRep where
  minBound = VersionRep $ getVersion minBound
  maxBound = VersionRep $ getVersion maxBound
instance MaybeBounded VersionRep

instance HasSimpleRep Version where
  type SimpleRep Version = VersionRep
  fromSimpleRep (VersionRep rep) = case runFail $ mkVersion rep of
    Left err ->
      error $
        unlines
          [ "fromSimpleRep @Version:"
          , show rep
          , err
          ]
    Right a -> a
  toSimpleRep = VersionRep . getVersion
instance BaseUniverse fn => HasSpec fn Version
instance BaseUniverse fn => OrdLike fn Version

succV_ :: BaseUniverse fn => Term fn Version -> Term fn Version
succV_ = fromGeneric_ . (+ 1) . toGeneric_

instance (BaseUniverse fn, Typeable r, Crypto c) => HasSpec fn (KeyHash r c) where
  type TypeSpec fn (KeyHash r c) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

-- =============================================================================
-- So now we define very simple types to serve as the SimpleRep for PParams and
-- its related types  (PParamsUpdate, FuturePParams etc.)

-- | Use this as the SimpleRep of (PParams era). It is polymorphic enough to
--   encode PParams in EVERY Era. The EraPParams instances remove the fields
--   that do not appear in that Era.
data SimplePParams era = SimplePParams
  { minFeeA :: Coin
  , minFeeB :: Coin
  , maxBBSize :: Word32
  , maxTxSize :: Word32
  , maxBHSize :: Word32 -- Need to be downsized inside reify to Word16
  , keyDeposit :: Coin
  , poolDeposit :: Coin
  , eMax :: EpochInterval
  , nOpt :: Natural
  , a0 :: NonNegativeInterval
  , rho :: UnitInterval
  , tau :: UnitInterval
  , decentral :: UnitInterval
  , protocolVersion :: ProtVer
  , minUTxOValue :: Coin
  , minPoolCost :: Coin
  -- ^ Alonzo
  , coinsPerUTxOWord :: Coin
  , costModels :: CostModels
  , prices :: Prices
  , maxTxExUnits :: ExUnits
  , maxBlockExUnits :: ExUnits
  , maxValSize :: Natural
  , collateralPercentage :: Natural
  , maxCollateralInputs :: Natural
  -- ^  Babbage
  , coinsPerUTxOByte :: Coin
  -- ^ Conway
  , poolVotingThresholds :: PoolVotingThresholds
  , drepVotingThresholds :: DRepVotingThresholds
  , committeeMinSize :: Natural
  , committeeMaxTermLength :: EpochInterval
  , govActionLifetime :: EpochInterval
  , govActionDeposit :: Coin
  , dRepDeposit :: Coin
  , dRepActivity :: EpochInterval
  , minFeeRefScriptCostPerByte :: NonNegativeInterval
  }
  deriving (Eq, Generic)

instance (EraPP era, Reflect era) => Show (SimplePParams era) where
  show x = show (prettyA (subsetToPP @era x))

-- | Use then generic HasSimpleRep and HasSpec instances for SimplePParams
instance HasSimpleRep (SimplePParams era)

instance (EraPP era, Reflect era, BaseUniverse fn) => HasSpec fn (SimplePParams era)

-- | Use this as the SimpleRep of (PParamsUpdate era)
data SimplePPUpdate = SimplePPUpdate
  { uminFeeA :: StrictMaybe Coin
  , uminFeeB :: StrictMaybe Coin
  , umaxBBSize :: StrictMaybe Word32
  , umaxTxSize :: StrictMaybe Word32
  , umaxBHSize :: StrictMaybe Word32 -- Need to be downsized inside reify to Word16
  , ukeyDeposit :: StrictMaybe Coin
  , upoolDeposit :: StrictMaybe Coin
  , ueMax :: StrictMaybe EpochInterval
  , unOpt :: StrictMaybe Natural
  , ua0 :: StrictMaybe NonNegativeInterval
  , urho :: StrictMaybe UnitInterval
  , utau :: StrictMaybe UnitInterval
  , udecentral :: StrictMaybe UnitInterval
  , uprotocolVersion :: StrictMaybe ProtVer
  , uminUTxOValue :: StrictMaybe Coin
  , uminPoolCost :: StrictMaybe Coin
  , -- Alonzo
    ucoinsPerUTxOWord :: StrictMaybe Coin
  , ucostModels :: StrictMaybe CostModels
  , uprices :: StrictMaybe Prices
  , umaxTxExUnits :: StrictMaybe ExUnits
  , umaxBlockExUnits :: StrictMaybe ExUnits
  , umaxValSize :: StrictMaybe Natural
  , ucollateralPercentage :: StrictMaybe Natural
  , umaxCollateralInputs :: StrictMaybe Natural
  , -- Babbage
    ucoinsPerUTxOByte :: StrictMaybe Coin
  , -- Conway
    upoolVotingThresholds :: StrictMaybe PoolVotingThresholds
  , udrepVotingThresholds :: StrictMaybe DRepVotingThresholds
  , ucommitteeMinSize :: StrictMaybe Natural
  , ucommitteeMaxTermLength :: StrictMaybe EpochInterval
  , ugovActionLifetime :: StrictMaybe EpochInterval
  , ugovActionDeposit :: StrictMaybe Coin
  , udRepDeposit :: StrictMaybe Coin
  , udRepActivity :: StrictMaybe EpochInterval
  , uminFeeRefScriptCostPerByte :: StrictMaybe NonNegativeInterval
  }
  deriving (Eq, Show, Generic)

-- ============================================

-- \| EraPP era means we can go back and forth between (SimplePParams era) and (PParams era)
--   This allow us to use (SimplePParams era) as the (SimpleRep (PParams era))
--   Much easier to constrain (SimplePParams era) than (PParams era) with all the THKD stuff.
class
  ( Reflect era
  , Eq (PParamsHKD Identity era)
  , Show (PParamsHKD Identity era)
  , Eq (PParamsHKD StrictMaybe era)
  , Show (PParamsHKD StrictMaybe era)
  , EraPParams era
  ) =>
  EraPP era
  where
  subsetToPP :: SimplePParams era -> PParams era
  ppToSubset :: PParams era -> SimplePParams era
  updateToPPU :: SimplePPUpdate -> PParamsUpdate era
  ppuToUpdate :: PParamsUpdate era -> SimplePPUpdate

instance EraPP Shelley where
  subsetToPP = liftShelley
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraPP Allegra where
  subsetToPP = liftShelley
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraPP Mary where
  subsetToPP x = liftShelley x
  ppToSubset x = dropAtMost4 x $ dropShelley x
  updateToPPU x = (uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropProtVer x $ uDropShelley x

instance EraPP Alonzo where
  subsetToPP x = (liftAlonzo x . liftShelley) x
  ppToSubset x = dropAlonzo x $ dropAtMost6 x $ dropShelley x
  updateToPPU x = (uLiftAlonzo x . uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropAlonzo x $ uDropProtVer x $ uDropShelley x

instance EraPP Babbage where
  subsetToPP x = (liftBabbage x . liftAlonzo x . liftShelley) x
  ppToSubset x = dropBabbage x $ dropAlonzo x $ dropShelley x
  updateToPPU x = (uLiftBabbage x . uLiftAlonzo x . uLiftProtVer x . uLiftShelley) x
  ppuToUpdate x = uDropBabbage x $ uDropAlonzo x $ uDropProtVer x $ uDropShelley x

instance EraPP Conway where
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
  (EraPParams era, ProtVerAtMost era 6) => PParams era -> SimplePParams era -> SimplePParams era
dropAtMost6 pp x = x {decentral = pp ^. ppDL}

dropAtMost4 ::
  (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) =>
  PParams era ->
  SimplePParams era ->
  SimplePParams era
dropAtMost4 pp x =
  x
    { minUTxOValue = pp ^. ppMinUTxOValueL
    , decentral = pp ^. ppDL
    }

-- Magic functions used to implement (EraPP era). Example use for Conway
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
  (EraPParams era, ProtVerAtMost era 8) => PParamsUpdate era -> SimplePPUpdate -> SimplePPUpdate
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
  (EraPParams era, ProtVerAtMost era 8) => SimplePPUpdate -> PParamsUpdate era -> PParamsUpdate era
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

-- ==============================================================================

-- | Use the generic HasSimpleRep and HasSpec instances for SimplePParams
instance HasSimpleRep SimplePPUpdate

instance BaseUniverse fn => HasSpec fn SimplePPUpdate

-- | SimpleRep instance for PParamsUpdate
instance EraPP era => HasSimpleRep (PParamsUpdate era) where
  type SimpleRep (PParamsUpdate era) = SimplePPUpdate
  toSimpleRep = ppuToUpdate
  fromSimpleRep = updateToPPU

-- | HasSpec instance for PParams
instance (BaseUniverse fn, EraPP era) => HasSpec fn (PParamsUpdate era) where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- ===============================================================

-- | SimpleRep instance for PParams
instance EraPP era => HasSimpleRep (PParams era) where
  type SimpleRep (PParams era) = SimplePParams era
  toSimpleRep = ppToSubset
  fromSimpleRep = subsetToPP

-- | HasSpec instance for PParams
instance (BaseUniverse fn, EraPP era, HasSpec fn Coin) => HasSpec fn (PParams era) where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- =======================================

instance EraPP era => HasSimpleRep (ProposedPPUpdates era)
instance (EraPP era, BaseUniverse fn) => HasSpec fn (ProposedPPUpdates era)

instance EraPP era => HasSimpleRep (FuturePParams era)
instance (EraPP era, BaseUniverse fn) => HasSpec fn (FuturePParams era)

-- ============================================================================
-- Term Selectors for SimplePParams

minFeeA_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
minFeeA_ simplepp = sel @0 simplepp

minFeeB_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
minFeeB_ simplepp = sel @1 simplepp

maxBBSize_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Word32
maxBBSize_ simplepp = sel @2 simplepp

maxTxSize_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Word32
maxTxSize_ simplepp = sel @3 simplepp

maxBHSize_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Word32
maxBHSize_ simplepp = sel @4 simplepp

keyDeposit_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
keyDeposit_ simplepp = sel @5 simplepp

poolDeposit_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
poolDeposit_ simplepp = sel @6 simplepp

eMax_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn EpochInterval
eMax_ simplepp = sel @7 simplepp

nOpt_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Natural
nOpt_ simplepp = sel @8 simplepp

a0_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn NonNegativeInterval
a0_ simplepp = sel @9 simplepp

rho_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn UnitInterval
rho_ simplepp = sel @10 simplepp

tau_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn UnitInterval
tau_ simplepp = sel @11 simplepp

decentral_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn UnitInterval
decentral_ simplepp = sel @12 simplepp

protocolVersion_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn ProtVer
protocolVersion_ simplepp = sel @13 simplepp

minUTxOValue_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
minUTxOValue_ simplepp = sel @14 simplepp

minPoolCost_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
minPoolCost_ simplepp = sel @15 simplepp

coinsPerUTxOWord_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
coinsPerUTxOWord_ simplepp = sel @16 simplepp

costModels_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn CostModels
costModels_ simplepp = sel @17 simplepp

prices_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Prices
prices_ simplepp = sel @18 simplepp

maxTxExUnits_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn ExUnits
maxTxExUnits_ simplepp = sel @19 simplepp

maxBlockUnits_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn ExUnits
maxBlockUnits_ simplepp = sel @20 simplepp

maxValSize_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Natural
maxValSize_ simplepp = sel @21 simplepp

collateralPercentage_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Natural
collateralPercentage_ simplepp = sel @22 simplepp

maxCollateralInputs_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Natural
maxCollateralInputs_ simplepp = sel @23 simplepp

coinsPerUTxOByte_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
coinsPerUTxOByte_ simplepp = sel @24 simplepp

poolVotingThresholds_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn PoolVotingThresholds
poolVotingThresholds_ simplepp = sel @25 simplepp

drepVotingThresholds_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn DRepVotingThresholds
drepVotingThresholds_ simplepp = sel @26 simplepp

committeeMinSize_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Natural
committeeMinSize_ simplepp = sel @27 simplepp

committeeMaxTermLength_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn EpochInterval
committeeMaxTermLength_ simplepp = sel @28 simplepp

govActionLifetime_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn EpochInterval
govActionLifetime_ simplepp = sel @29 simplepp

govActionDeposit_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
govActionDeposit_ simplepp = sel @30 simplepp

dRepDeposit_ :: (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn Coin
dRepDeposit_ simplepp = sel @31 simplepp

dRepActivity_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn EpochInterval
dRepActivity_ simplepp = sel @32 simplepp

minFeeRefScriptCostPerByte_ ::
  (EraPP era, BaseUniverse fn) => Term fn (SimplePParams era) -> Term fn NonNegativeInterval
minFeeRefScriptCostPerByte_ simplepp = sel @33 simplepp

-- =======================================================================

-- | A sample of how to constrain (PParams era) for every Era, by writing a specification for SimplePParams.
--   Constrained but not applicable fields (for that era) are elided in the result.
--   Missing fields are left unconstrained and will appear as random values in the result.
--   This can easily be lifted to PParams: see Test.Cardano.Ledger.Constrained.Conway.PParams(pparamsSpec)
simplePParamsSpec ::
  forall fn era. (EraPP era, BaseUniverse fn) => Specification fn (SimplePParams era)
simplePParamsSpec = constrained $ \pp ->
  -- pp :: SimplePParams
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

exampleSimplePParams :: IO Bool
exampleSimplePParams = do
  let thespec = simplePParamsSpec @BaseFn @Conway
  pp <- generate $ genFromSpec thespec
  putStrLn (show pp)
  pure (conformsToSpec pp thespec)
