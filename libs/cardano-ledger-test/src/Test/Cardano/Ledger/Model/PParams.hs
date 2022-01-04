{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Model.PParams where

import Cardano.Ledger.Alonzo.Scripts (Prices (..))
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    ProtVer (..),
    UnitInterval,
  )
import Cardano.Ledger.Coin (Coin (..))
import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)
import Data.Functor.Identity (Identity (..))
import Data.HKD
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import qualified GHC.Records as GHC
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    KnownRequiredFeatures,
    MintSupported (..),
    ScriptFeature,
    ValueFeature,
    bifoldMapSupportsFeature,
    ifSupportsMint,
    ifSupportsPlutus,
  )
import Test.Cardano.Ledger.Rational (unsafeFromRational)

type UsesModelPP pparams =
  ( GHC.HasField "_d" pparams UnitInterval,
    GHC.HasField "_tau" pparams UnitInterval,
    GHC.HasField "_a0" pparams NonNegativeInterval,
    GHC.HasField "_rho" pparams UnitInterval,
    GHC.HasField "_nOpt" pparams Natural,
    GHC.HasField "_protocolVersion" pparams ProtVer
  )

-- | convenient initial pparams
modelPParams :: forall era f. (KnownRequiredFeatures era, Applicative f) => ModelPParamsF era f
modelPParams =
  ModelPParams
    { _modelPParams_protocolVersion = pure $ ProtVer 5 0,
      _modelPParams_minfeeA = pure 1,
      _modelPParams_minfeeB = pure 1,
      _modelPParams_collateralPercent = pure $ sp () 150,
      _modelPParams_d = pure maxBound,
      _modelPParams_maxTxSize = pure 1_000_000,
      _modelPParams_nOpt = pure 10,
      _modelPParams_rho = pure $ unsafeFromRational "modelPParams::rho" 0.02,
      _modelPParams_tau = pure minBound,
      _modelPParams_a0 = pure minBound,
      _modelPParams_keyDeposit = pure (Coin 0),
      _modelPParams_poolDeposit = pure (Coin 0),
      _modelPParams_prices = pure $ sp () (Prices minBound minBound),
      _modelPParams_coinsPerUTxOWord = pure $ sm (Coin 1) (Coin 1),
      _modelPParams_maxCollateralInputs = pure $ sp () 5,
      _modelPParams_minPoolCost = pure (Coin 0)
    }
  where
    sp :: forall a b. a -> b -> IfSupportsPlutus a b (ScriptFeature era)
    sp = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era))

    sm :: forall a b. a -> b -> IfSupportsMint a b (ValueFeature era)
    sm = ifSupportsMint (Proxy :: Proxy (ValueFeature era))

data ModelPParamsF (era :: FeatureSet) f = ModelPParams
  { _modelPParams_minfeeA :: !(f Natural),
    _modelPParams_minfeeB :: !(f Natural),
    _modelPParams_collateralPercent :: !(f (IfSupportsPlutus () Natural (ScriptFeature era))),
    _modelPParams_d :: !(f UnitInterval),
    _modelPParams_tau :: !(f UnitInterval),
    _modelPParams_a0 :: !(f NonNegativeInterval),
    _modelPParams_rho :: !(f UnitInterval),
    _modelPParams_nOpt :: !(f Natural),
    _modelPParams_protocolVersion :: !(f ProtVer),
    _modelPParams_keyDeposit :: !(f Coin),
    _modelPParams_poolDeposit :: !(f Coin),
    _modelPParams_maxCollateralInputs :: !(f (IfSupportsPlutus () Natural (ScriptFeature era))),
    _modelPParams_prices :: !(f (IfSupportsPlutus () Prices (ScriptFeature era))),
    -- | mixes shelley era minUTxOValue with alonzo coinsPerUTxOWord.
    -- intermediate (shelley-ma : fig7[GL-D1]) is not handled.
    _modelPParams_coinsPerUTxOWord :: !(f (IfSupportsMint Coin Coin (ValueFeature era))),
    _modelPParams_maxTxSize :: !(f Natural),
    _modelPParams_minPoolCost :: !(f Coin)
  }
  deriving (Generic)

type TransModelPParams :: forall k. (k -> Constraint) -> (Type -> k) -> FeatureSet -> Constraint
type TransModelPParams c f era =
  ( c (f (IfSupportsPlutus () Natural (ScriptFeature era))),
    c (f (IfSupportsPlutus () Prices (ScriptFeature era))),
    c (f (IfSupportsMint Coin Coin (ValueFeature era))),
    c (f UnitInterval),
    c (f NonNegativeInterval),
    c (f Natural),
    c (f ProtVer),
    c (f Coin),
    c (f (IfSupportsPlutus () Natural (ScriptFeature era)))
  )

deriving instance TransModelPParams Eq f era => Eq (ModelPParamsF era f)

deriving instance TransModelPParams Show f era => Show (ModelPParamsF era f)

instance TransModelPParams NFData f era => NFData (ModelPParamsF era f)

instance FFunctor (ModelPParamsF era) where ffmap = ffmapDefault

instance FZip (ModelPParamsF era) where fzipWith = gfzipWith

instance FRepeat (ModelPParamsF era) where frepeat = gfrepeat

instance FFoldable (ModelPParamsF era) where ffoldMap = ffoldMapDefault

instance FTraversable (ModelPParamsF era) where ftraverse = gftraverse

type ModelPParams era = ModelPParamsF era Identity

type ModelPParamsUpdate era = ModelPParamsF era Maybe

instance GHC.HasField "_minfeeA" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_minfeeA

instance GHC.HasField "_minfeeB" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_minfeeB

instance GHC.HasField "_d" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_d

instance GHC.HasField "_tau" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_tau

instance GHC.HasField "_a0" (ModelPParams era) NonNegativeInterval where
  getField = runIdentity . _modelPParams_a0

instance GHC.HasField "_rho" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_rho

instance GHC.HasField "_nOpt" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_nOpt

instance GHC.HasField "_protocolVersion" (ModelPParams era) ProtVer where
  getField = runIdentity . _modelPParams_protocolVersion

instance GHC.HasField "_keyDeposit" (ModelPParams era) Coin where
  getField = runIdentity . _modelPParams_keyDeposit

instance GHC.HasField "_poolDeposit" (ModelPParams era) Coin where
  getField = runIdentity . _modelPParams_poolDeposit

instance GHC.HasField "_maxCollateralInputs" (ModelPParams era) Natural where
  getField = bifoldMapSupportsFeature (\() -> 0) id . runIdentity . _modelPParams_maxCollateralInputs

instance GHC.HasField "_prices" (ModelPParams era) Prices where
  getField = bifoldMapSupportsFeature (\() -> Prices minBound minBound) id . runIdentity . _modelPParams_prices

instance GHC.HasField "_maxTxSize" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_maxTxSize

instance MintSupported (ValueFeature era) => GHC.HasField "_coinsPerUTxOWord" (ModelPParams era) Coin where
  getField = supportsMint . runIdentity . _modelPParams_coinsPerUTxOWord

instance GHC.HasField "_minPoolCost" (ModelPParams era) Coin where
  getField = runIdentity . _modelPParams_minPoolCost

class HasModelPParams era a | a -> era where
  getModelPParams :: a -> ModelPParams era

modelPParams_keyDeposit :: Lens' (ModelPParamsF era f) (f Coin)
modelPParams_keyDeposit = lens _modelPParams_keyDeposit $ \s b -> s {_modelPParams_keyDeposit = b}

modelPParams_poolDeposit :: Lens' (ModelPParamsF era f) (f Coin)
modelPParams_poolDeposit = lens _modelPParams_poolDeposit $ \s b -> s {_modelPParams_poolDeposit = b}
