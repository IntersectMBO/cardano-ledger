{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  prettyE,
) where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Constrained.API.Extend
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Fail.String
import Data.Maybe
import Data.Ratio ((%))
import Data.TreeDiff
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Random
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Era ()
import Test.QuickCheck hiding (Args, Fun, NonZero, forAll)
import Text.PrettyPrint.HughesPJ (Doc)

-- ===============================================
-- Pretty printing via TreeDiff Expr

prettyE :: ToExpr x => x -> Doc
prettyE x = prettyExpr (toExpr x)

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
    Nothing -> error $ "The impossible happened in toSimpleRep for (Coin " ++ show i ++ ")"
    Just w -> w
  fromSimpleRep = word64ToCoin

instance HasSpec Coin

instance MaybeBounded Coin where
  lowerBound = Just (word64ToCoin 0)
  upperBound = Just (word64ToCoin (maxBound @Word64))

instance OrdLike Coin

instance NumLike Coin

instance Foldy Coin where
  noNegativeValues = True

-- TODO: This is hack to get around the need for `Num` in `NumLike`. We should possibly split
-- this up so that `NumLike` has its own addition etc. instead?
deriving via Integer instance Num Coin

instance Typeable a => HasSimpleRep (StrictMaybe a)

instance (HasSpec a, IsNormalType a) => HasSpec (StrictMaybe a)

cSNothing_ :: (HasSpec a, IsNormalType a) => Term (StrictMaybe a)
cSNothing_ = con @"SNothing" (lit ())

cSJust_ :: (HasSpec a, IsNormalType a) => Term a -> Term (StrictMaybe a)
cSJust_ = con @"SJust"

instance HasSimpleRep EpochInterval

instance OrdLike EpochInterval

instance HasSpec EpochInterval

instance HasSpec UnitInterval where
  type TypeSpec UnitInterval = ()
  emptySpec = ()
  combineSpec _ _ = trueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = trueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSpec NonNegativeInterval where
  type TypeSpec NonNegativeInterval = ()
  emptySpec = ()
  combineSpec _ _ = trueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = trueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep CostModels

instance HasSpec CostModels where
  type TypeSpec CostModels = ()
  emptySpec = ()
  combineSpec _ _ = trueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = trueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep Prices

instance HasSpec Prices

instance HasSimpleRep ExUnits where
  type SimpleRep ExUnits = SimpleRep (Natural, Natural)
  fromSimpleRep = uncurry ExUnits . fromSimpleRep
  toSimpleRep (ExUnits a b) = toSimpleRep (a, b)

instance HasSpec ExUnits

instance HasSimpleRep OrdExUnits where
  type SimpleRep OrdExUnits = SimpleRep ExUnits
  fromSimpleRep = OrdExUnits . fromSimpleRep
  toSimpleRep = toSimpleRep . unOrdExUnits

instance HasSpec OrdExUnits

instance HasSimpleRep PoolVotingThresholds

instance HasSpec PoolVotingThresholds

instance HasSimpleRep DRepVotingThresholds

instance HasSpec DRepVotingThresholds

instance HasSimpleRep ProtVer

instance HasSpec ProtVer

-- We do this like this to get the right bounds for `VersionRep`
-- while ensuring that we don't have to add instances for e.g. `Num`
-- to version.
newtype VersionRep = VersionRep Word8
  deriving (Show, Eq, Ord, Num, Random, Arbitrary, Integral, Real, Enum) via Word8

instance HasSpec VersionRep where
  type TypeSpec VersionRep = NumSpec VersionRep
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

instance HasSpec Version

instance OrdLike Version

succV_ :: Term Version -> Term Version
succV_ = fromGeneric_ . (+ 1) . toGeneric_

instance Typeable r => HasSpec (KeyHash r) where
  type TypeSpec (KeyHash r) = ()
  emptySpec = ()
  combineSpec _ _ = trueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = trueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

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
  , nOpt :: Word16
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
  deriving (Eq, Generic, ToExpr)

instance ToExpr PoolVotingThresholds

instance ToExpr DRepVotingThresholds

instance (EraSpecPParams era, EraGov era, EraTxOut era) => Show (SimplePParams era) where
  show x = show (toExpr (subsetToPP @era x))

-- | Use then generic HasSimpleRep and HasSpec instances for SimplePParams
instance HasSimpleRep (SimplePParams era)

instance
  ( EraSpecPParams era
  , EraGov era
  , EraTxOut era
  ) =>
  HasSpec (SimplePParams era)

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
  , unOpt :: StrictMaybe Word16
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

instance HasSpec SimplePPUpdate

-- | SimpleRep instance for PParamsUpdate
instance EraSpecPParams era => HasSimpleRep (PParamsUpdate era) where
  type SimpleRep (PParamsUpdate era) = SimplePPUpdate
  toSimpleRep = ppuToUpdate
  fromSimpleRep = updateToPPU

-- | HasSpec instance for PParams
instance EraSpecPParams era => HasSpec (PParamsUpdate era) where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- ===============================================================

-- | SimpleRep instance for PParams
instance EraSpecPParams era => HasSimpleRep (PParams era) where
  type SimpleRep (PParams era) = SimplePParams era
  toSimpleRep = ppToSubset
  fromSimpleRep = subsetToPP

-- | HasSpec instance for PParams
instance
  ( EraSpecPParams era
  , EraTxOut era
  , EraGov era
  ) =>
  HasSpec (PParams era)
  where
  genFromTypeSpec x = fromSimpleRep <$> genFromTypeSpec x

-- =======================================

instance EraSpecPParams era => HasSimpleRep (ProposedPPUpdates era)

instance EraSpecPParams era => HasSpec (ProposedPPUpdates era)

instance EraSpecPParams era => HasSimpleRep (FuturePParams era)

instance
  ( EraGov era
  , EraTxOut era
  , EraSpecPParams era
  ) =>
  HasSpec (FuturePParams era)

-- =============================================================

-- \| EraSpecPParams era means we can go back and forth between (SimplePParams era) and (PParams era)
--   This allow us to use (SimplePParams era) as the (SimpleRep (PParams era))
--   Much easier to constrain (SimplePParams era) than (PParams era) with all the THKD stuff.
class
  ( Eq (PParamsHKD Identity era)
  , Show (PParamsHKD Identity era)
  , ToExpr (PParamsHKD Identity era)
  , Eq (PParamsHKD StrictMaybe era)
  , Show (PParamsHKD StrictMaybe era)
  , EraPParams era
  , EraTxOut era
  , EraGov era
  , EraTx era
  ) =>
  EraSpecPParams era
  where
  subsetToPP :: SimplePParams era -> PParams era
  ppToSubset :: PParams era -> SimplePParams era
  updateToPPU :: SimplePPUpdate -> PParamsUpdate era
  ppuToUpdate :: PParamsUpdate era -> SimplePPUpdate

instance HasSimpleRep (NonZero Word64) where
  type SimpleRep (NonZero Word64) = Word64
  toSimpleRep = unNonZero
  fromSimpleRep = unsafeNonZero

instance HasSpec (NonZero Word64)
