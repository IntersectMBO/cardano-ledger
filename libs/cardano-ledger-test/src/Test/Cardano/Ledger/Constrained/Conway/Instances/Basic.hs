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
{-# OPTIONS_GHC -O0 #-}
#endif

-- | This module provides `HasSpec` and `HasSimpleRep` instances for
--   Basic types. A type is 'Basic' if it is used to define PParams.
--   See Test.Cardano.Ledger.Constrained.Conway.SimplePParams
--   We divide these `HasSpec` and `HasSimpleRep` instances into two files
--   because SimplePParams, needs these instances but not the 100's of other
--   ones defined in Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
--   And too many instances causes GHC 8.10.7 to blow up.
module Test.Cardano.Ledger.Constrained.Conway.Instances.Basic (
  cSNothing_,
  cSJust_,
  succV_,
  makePrices,
  makeUnitInterval,
  makeNonNegativeInterval,
  SimplePParams (..),
  SimplePPUpdate (..),
  EraSpecPParams (..),
) where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Constrained hiding (Value)
import Constrained.Univ ()
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Fail.String
import Data.Maybe
import Data.Ratio ((%))
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Random
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.Cardano.Ledger.Generic.Proof (Reflect (..))
import Test.QuickCheck hiding (Args, Fun, forAll)

-- ============================================================================
-- Making Intervals based on Ratios, These can fail, so be careful using them.

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

instance BaseUniverse fn => HasSpec fn UnitInterval where
  type TypeSpec fn UnitInterval = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

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

-- =========================================================================================

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

instance (EraSpecPParams era, Reflect era) => Show (SimplePParams era) where
  show x = show (prettyA (subsetToPP @era x))

-- | Use then generic HasSimpleRep and HasSpec instances for SimplePParams
instance HasSimpleRep (SimplePParams era)

instance (EraSpecPParams era, Reflect era, BaseUniverse fn) => HasSpec fn (SimplePParams era)

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

-- ==============================================================================

-- | Use the generic HasSimpleRep and HasSpec instances for SimplePParams
instance HasSimpleRep SimplePPUpdate

instance BaseUniverse fn => HasSpec fn SimplePPUpdate

-- | SimpleRep instance for PParamsUpdate
instance EraSpecPParams era => HasSimpleRep (PParamsUpdate era) where
  type SimpleRep (PParamsUpdate era) = SimplePPUpdate
  toSimpleRep = ppuToUpdate
  fromSimpleRep = updateToPPU

-- | HasSpec instance for PParams
instance (BaseUniverse fn, EraSpecPParams era) => HasSpec fn (PParamsUpdate era) where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- ===============================================================

-- | SimpleRep instance for PParams
instance EraSpecPParams era => HasSimpleRep (PParams era) where
  type SimpleRep (PParams era) = SimplePParams era
  toSimpleRep = ppToSubset
  fromSimpleRep = subsetToPP

-- | HasSpec instance for PParams
instance (BaseUniverse fn, EraSpecPParams era, HasSpec fn Coin) => HasSpec fn (PParams era) where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- =======================================

instance EraSpecPParams era => HasSimpleRep (ProposedPPUpdates era)
instance (EraSpecPParams era, BaseUniverse fn) => HasSpec fn (ProposedPPUpdates era)

instance EraSpecPParams era => HasSimpleRep (FuturePParams era)
instance (EraSpecPParams era, BaseUniverse fn) => HasSpec fn (FuturePParams era)

-- =============================================================

-- \| EraSpecPParams era means we can go back and forth between (SimplePParams era) and (PParams era)
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
  EraSpecPParams era
  where
  subsetToPP :: SimplePParams era -> PParams era
  ppToSubset :: PParams era -> SimplePParams era
  updateToPPU :: SimplePPUpdate -> PParamsUpdate era
  ppuToUpdate :: PParamsUpdate era -> SimplePPUpdate
