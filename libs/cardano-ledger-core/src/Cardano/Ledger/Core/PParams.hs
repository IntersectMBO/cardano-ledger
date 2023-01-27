{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Core.PParams (
  EraPParams (..),
  PParams (..),
  emptyPParams,
  PParamsUpdate (..),
  emptyPParamsUpdate,

  -- * PParams lens helpers
  ppMinFeeAL,
  ppMinFeeBL,
  ppMaxBBSizeL,
  ppMaxTxSizeL,
  ppMaxBHSizeL,
  ppKeyDepositL,
  ppPoolDepositL,
  ppEMaxL,
  ppNOptL,
  ppA0L,
  ppRhoL,
  ppTauL,
  ppDL,
  ppExtraEntropyL,
  ppProtocolVersionL,
  ppMinUTxOValueL,
  ppMinPoolCostL,

  -- * PParamsUpdate lens helpers
  ppuMinFeeAL,
  ppuMinFeeBL,
  ppuMaxBBSizeL,
  ppuMaxTxSizeL,
  ppuMaxBHSizeL,
  ppuKeyDepositL,
  ppuPoolDepositL,
  ppuEMaxL,
  ppuNOptL,
  ppuA0L,
  ppuRhoL,
  ppuTauL,
  ppuDL,
  ppuExtraEntropyL,
  ppuProtocolVersionL,
  ppuMinUTxOValueL,
  ppuMinPoolCostL,

  -- * Utility
  ppLens,
  ppuLens,
  mapPParams,
  mapPParamsUpdate,
  upgradePParams,
  downgradePParams,
  upgradePParamsUpdate,
  downgradePParamsUpdate,
)
where

import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  NonNegativeInterval,
  Nonce (..),
  ProtVer,
  StrictMaybe (..),
  UnitInterval,
 )
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, FromCBOR, ToCBOR)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core.Era (Era (..), PreviousEra, ProtVerAtMost)
import Cardano.Ledger.HKD (HKD, HKDFunctor)
import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Default.Class (Default (..))
import Data.Kind (Type)
import Data.TreeDiff.Class (ToExpr)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1, V1, type (:*:) (..))
import GHC.Natural (Natural)
import Lens.Micro (Lens', SimpleGetter, lens)
import NoThunks.Class (NoThunks)

-- | Protocol parameters
newtype PParams era = PParams (PParamsHKD Identity era)

instance EraPParams era => Default (PParams era) where
  def = emptyPParams

deriving newtype instance
  Eq (PParamsHKD Identity era) => Eq (PParams era)

deriving newtype instance
  Ord (PParamsHKD Identity era) => Ord (PParams era)

deriving newtype instance
  NFData (PParamsHKD Identity era) => NFData (PParams era)

deriving newtype instance
  NoThunks (PParamsHKD Identity era) => NoThunks (PParams era)

deriving newtype instance
  Show (PParamsHKD Identity era) => Show (PParams era)

deriving newtype instance
  ToJSON (PParamsHKD Identity era) => ToJSON (PParams era)

deriving newtype instance
  FromJSON (PParamsHKD Identity era) => FromJSON (PParams era)

deriving newtype instance
  (Typeable era, ToCBOR (PParamsHKD Identity era)) => ToCBOR (PParams era)

deriving newtype instance
  (Typeable era, FromCBOR (PParamsHKD Identity era)) => FromCBOR (PParams era)

deriving newtype instance
  ToExpr (PParamsHKD Identity era) => ToExpr (PParams era)

deriving instance Generic (PParams era)

-- | The type of updates to Protocol parameters
newtype PParamsUpdate era = PParamsUpdate (PParamsHKD StrictMaybe era)

instance EraPParams era => Default (PParamsUpdate era) where
  def = emptyPParamsUpdate

deriving newtype instance
  Eq (PParamsHKD StrictMaybe era) => Eq (PParamsUpdate era)

deriving newtype instance
  Ord (PParamsHKD StrictMaybe era) => Ord (PParamsUpdate era)

deriving newtype instance
  NFData (PParamsHKD StrictMaybe era) => NFData (PParamsUpdate era)

deriving newtype instance
  NoThunks (PParamsHKD StrictMaybe era) => NoThunks (PParamsUpdate era)

deriving newtype instance
  Show (PParamsHKD StrictMaybe era) => Show (PParamsUpdate era)

deriving newtype instance
  (Typeable era, ToCBOR (PParamsHKD StrictMaybe era)) => ToCBOR (PParamsUpdate era)

deriving newtype instance
  (Typeable era, FromCBOR (PParamsHKD StrictMaybe era)) => FromCBOR (PParamsUpdate era)

deriving newtype instance
  (Typeable era, EncCBOR (PParamsHKD StrictMaybe era)) => EncCBOR (PParamsUpdate era)

deriving newtype instance
  (Typeable era, DecCBOR (PParamsHKD StrictMaybe era)) => DecCBOR (PParamsUpdate era)

deriving newtype instance
  ToJSON (PParamsHKD StrictMaybe era) => ToJSON (PParamsUpdate era)

deriving newtype instance
  FromJSON (PParamsHKD StrictMaybe era) => FromJSON (PParamsUpdate era)

deriving newtype instance
  ToExpr (PParamsHKD StrictMaybe era) => ToExpr (PParamsUpdate era)

deriving instance Generic (PParamsUpdate era)

-- Generic derivation of `applyPPUpdates`

class Updatable a u where
  applyUpdate :: a -> u -> a

instance Updatable (U1 a) (U1 u) where
  applyUpdate x _ = x

instance Updatable (V1 a) (V1 u) where
  applyUpdate x _ = case x of {}

instance (Updatable (a1 a) (u1 u), Updatable (a2 a) (u2 u)) => Updatable ((a1 :*: a2) a) ((u1 :*: u2) u) where
  applyUpdate (x1 :*: x2) (u1 :*: u2) = applyUpdate x1 u1 :*: applyUpdate x2 u2

instance Updatable (a x) (a' x') => Updatable (M1 i c a x) (M1 i' c' a' x') where
  applyUpdate (M1 x) (M1 y) = M1 $ applyUpdate x y

instance Updatable (K1 t x a) (K1 t (StrictMaybe x) u) where
  applyUpdate (K1 x') (K1 sm) = K1 $ case sm of
    SJust x -> x
    SNothing -> x'

class
  ( Era era
  , Eq (PParamsHKD Identity era)
  , Ord (PParamsHKD Identity era)
  , Show (PParamsHKD Identity era)
  , NFData (PParamsHKD Identity era)
  , ToCBOR (PParamsHKD Identity era)
  , FromCBOR (PParamsHKD Identity era)
  , NoThunks (PParamsHKD Identity era)
  , Eq (PParamsHKD StrictMaybe era)
  , Ord (PParamsHKD StrictMaybe era)
  , Show (PParamsHKD StrictMaybe era)
  , NFData (PParamsHKD StrictMaybe era)
  , ToCBOR (PParamsHKD StrictMaybe era)
  , FromCBOR (PParamsHKD StrictMaybe era)
  , NoThunks (PParamsHKD StrictMaybe era)
  ) =>
  EraPParams era
  where
  -- | Protocol parameters where the fields are represented with a HKD
  type PParamsHKD (f :: Type -> Type) era = (r :: Type) | r -> era

  -- | Applies a protocol parameters update
  applyPPUpdates ::
    PParams era ->
    PParamsUpdate era ->
    PParams era
  default applyPPUpdates ::
    forall a u.
    ( Generic (PParamsHKD Identity era)
    , Generic (PParamsHKD StrictMaybe era)
    , Updatable (Rep (PParamsHKD Identity era) a) (Rep (PParamsHKD StrictMaybe era) u)
    ) =>
    PParams era ->
    PParamsUpdate era ->
    PParams era
  applyPPUpdates (PParams a) (PParamsUpdate u) =
    PParams . to $ applyUpdate (from @_ @a a) (from @_ @u u)

  emptyPParamsIdentity :: PParamsHKD Identity era
  emptyPParamsStrictMaybe :: PParamsHKD StrictMaybe era

  -- |
  type UpgradePParams (f :: Type -> Type) era :: Type

  type DowngradePParams (f :: Type -> Type) era :: Type

  -- | Upgrade PParams from previous era to the current one
  upgradePParamsHKD ::
    (HKDFunctor f, EraPParams (PreviousEra era)) =>
    UpgradePParams f era ->
    PParamsHKD f (PreviousEra era) ->
    PParamsHKD f era

  -- | Downgrade PParams from the current era to the previous one
  downgradePParamsHKD ::
    (HKDFunctor f, EraPParams (PreviousEra era)) =>
    DowngradePParams f era ->
    PParamsHKD f era ->
    PParamsHKD f (PreviousEra era)

  -- HKD Versions of lenses

  -- | The linear factor for the minimum fee calculation
  hkdMinFeeAL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | The constant factor for the minimum fee calculation
  hkdMinFeeBL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | Maximal block body size
  hkdMaxBBSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Maximal transaction size
  hkdMaxTxSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Maximal block header size
  hkdMaxBHSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  -- | The amount of a key registration deposit
  hkdKeyDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | The amount of a pool registration deposit
  hkdPoolDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | epoch bound on pool retirement
  hkdEMaxL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochNo)

  -- | Desired number of pools
  hkdNOptL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Pool influence
  hkdA0L :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f NonNegativeInterval)

  -- | Monetary expansion
  hkdRhoL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Treasury expansion
  hkdTauL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Decentralization parameter
  hkdDL :: (HKDFunctor f, ProtVerAtMost era 6) => Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Decentralization parameter getter
  ppDG :: SimpleGetter (PParams era) UnitInterval
  default ppDG :: ProtVerAtMost era 6 => SimpleGetter (PParams era) UnitInterval
  ppDG = ppLens . hkdDL @era @Identity

  -- | Extra entropy
  hkdExtraEntropyL :: (HKDFunctor f, ProtVerAtMost era 6) => Lens' (PParamsHKD f era) (HKD f Nonce)

  -- | Protocol version
  hkdProtocolVersionL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f ProtVer)

  -- | Minimum UTxO value
  hkdMinUTxOValueL :: HKDFunctor f => ProtVerAtMost era 4 => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | Minimum Stake Pool Cost
  hkdMinPoolCostL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

emptyPParams :: EraPParams era => PParams era
emptyPParams = PParams emptyPParamsIdentity

emptyPParamsUpdate :: EraPParams era => PParamsUpdate era
emptyPParamsUpdate = PParamsUpdate emptyPParamsStrictMaybe

ppLens :: Lens' (PParams era) (PParamsHKD Identity era)
ppLens = lens (\(PParams x) -> x) (\_ pp -> PParams pp)

ppuLens :: Lens' (PParamsUpdate era) (PParamsHKD StrictMaybe era)
ppuLens = lens (\(PParamsUpdate x) -> x) (\_ pp -> PParamsUpdate pp)

-- PParams versions of lenses

-- | The linear factor for the minimum fee calculation
ppMinFeeAL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinFeeAL = ppLens . hkdMinFeeAL @era @Identity

-- | The constant factor for the minimum fee calculation
ppMinFeeBL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinFeeBL = ppLens . hkdMinFeeBL @era @Identity

-- | Maximal block body size
ppMaxBBSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxBBSizeL = ppLens . hkdMaxBBSizeL @era @Identity

-- | Maximal transaction size
ppMaxTxSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxTxSizeL = ppLens . hkdMaxTxSizeL @era @Identity

-- | Maximal block header size
ppMaxBHSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxBHSizeL = ppLens . hkdMaxBHSizeL @era @Identity

-- | The amount of a key registration deposit
ppKeyDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppKeyDepositL = ppLens . hkdKeyDepositL @era @Identity

-- | The amount of a pool registration deposit
ppPoolDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppPoolDepositL = ppLens . hkdPoolDepositL @era @Identity

-- | epoch bound on pool retirement
ppEMaxL :: forall era. EraPParams era => Lens' (PParams era) EpochNo
ppEMaxL = ppLens . hkdEMaxL @era @Identity

-- | Desired number of pools
ppNOptL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppNOptL = ppLens . hkdNOptL @era @Identity

-- | Pool influence
ppA0L :: forall era. EraPParams era => Lens' (PParams era) NonNegativeInterval
ppA0L = ppLens . hkdA0L @era @Identity

-- | Monetary expansion
ppRhoL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppRhoL = ppLens . hkdRhoL @era @Identity

-- | Treasury expansion
ppTauL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppTauL = ppLens . hkdTauL @era @Identity

-- | Decentralization parameter
ppDL :: forall era. (EraPParams era, ProtVerAtMost era 6) => Lens' (PParams era) UnitInterval
ppDL = ppLens . hkdDL @era @Identity

-- | Extra entropy
ppExtraEntropyL :: forall era. (EraPParams era, ProtVerAtMost era 6) => Lens' (PParams era) Nonce
ppExtraEntropyL = ppLens . hkdExtraEntropyL @era @Identity

-- | Protocol version
ppProtocolVersionL :: forall era. EraPParams era => Lens' (PParams era) ProtVer
ppProtocolVersionL = ppLens . hkdProtocolVersionL @era @Identity

-- | Minimum UTxO value
ppMinUTxOValueL :: forall era. (EraPParams era, ProtVerAtMost era 4) => Lens' (PParams era) Coin
ppMinUTxOValueL = ppLens . hkdMinUTxOValueL @era @Identity

-- | Minimum Stake Pool Cost
ppMinPoolCostL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinPoolCostL = ppLens . hkdMinPoolCostL @era @Identity

-- PParamsUpdate versions of lenses

-- | The linear factor for the minimum fee calculation
ppuMinFeeAL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinFeeAL = ppuLens . hkdMinFeeAL @era @StrictMaybe

-- | The constant factor for the minimum fee calculation
ppuMinFeeBL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinFeeBL = ppuLens . hkdMinFeeBL @era @StrictMaybe

-- | Maximal block body size
ppuMaxBBSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxBBSizeL = ppuLens . hkdMaxBBSizeL @era @StrictMaybe

-- | Maximal transaction size
ppuMaxTxSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxTxSizeL = ppuLens . hkdMaxTxSizeL @era @StrictMaybe

-- | Maximal block header size
ppuMaxBHSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxBHSizeL = ppuLens . hkdMaxBHSizeL @era @StrictMaybe

-- | The amount of a key registration deposit
ppuKeyDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuKeyDepositL = ppuLens . hkdKeyDepositL @era @StrictMaybe

-- | The amount of a pool registration deposit
ppuPoolDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuPoolDepositL = ppuLens . hkdPoolDepositL @era @StrictMaybe

-- | epoch bound on pool retirement
ppuEMaxL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
ppuEMaxL = ppuLens . hkdEMaxL @era @StrictMaybe

-- | Desired number of pools
ppuNOptL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuNOptL = ppuLens . hkdNOptL @era @StrictMaybe

-- | Pool influence
ppuA0L :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe NonNegativeInterval)
ppuA0L = ppuLens . hkdA0L @era @StrictMaybe

-- | Monetary expansion
ppuRhoL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuRhoL = ppuLens . hkdRhoL @era @StrictMaybe

-- | Treasury expansion
ppuTauL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuTauL = ppuLens . hkdTauL @era @StrictMaybe

-- | Decentralization parameter
ppuDL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 6) =>
  Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuDL = ppuLens . hkdDL @era @StrictMaybe

-- | Extra entropy
ppuExtraEntropyL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 6) =>
  Lens' (PParamsUpdate era) (StrictMaybe Nonce)
ppuExtraEntropyL = ppuLens . hkdExtraEntropyL @era @StrictMaybe

-- | Protocol version
ppuProtocolVersionL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe ProtVer)
ppuProtocolVersionL = ppuLens . hkdProtocolVersionL @era @StrictMaybe

-- | Minimum UTxO value
ppuMinUTxOValueL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 4) =>
  Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinUTxOValueL = ppuLens . hkdMinUTxOValueL @era @StrictMaybe

-- | Minimum Stake Pool Cost
ppuMinPoolCostL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinPoolCostL = ppuLens . hkdMinPoolCostL @era @StrictMaybe

mapPParams :: (PParamsHKD Identity era1 -> PParamsHKD Identity era2) -> PParams era1 -> PParams era2
mapPParams f (PParams pp) = PParams $ f pp

mapPParamsUpdate ::
  (PParamsHKD StrictMaybe era1 -> PParamsHKD StrictMaybe era2) ->
  PParamsUpdate era1 ->
  PParamsUpdate era2
mapPParamsUpdate f (PParamsUpdate pp) = PParamsUpdate $ f pp

upgradePParams ::
  (EraPParams era, EraPParams (PreviousEra era)) =>
  UpgradePParams Identity era ->
  PParams (PreviousEra era) ->
  PParams era
upgradePParams args (PParams pphkd) =
  PParams (upgradePParamsHKD @_ @Identity args pphkd)

downgradePParams ::
  (EraPParams era, EraPParams (PreviousEra era)) =>
  DowngradePParams Identity era ->
  PParams era ->
  PParams (PreviousEra era)
downgradePParams args (PParams pphkd) =
  PParams (downgradePParamsHKD @_ @Identity args pphkd)

upgradePParamsUpdate ::
  (EraPParams era, EraPParams (PreviousEra era)) =>
  UpgradePParams StrictMaybe era ->
  PParamsUpdate (PreviousEra era) ->
  PParamsUpdate era
upgradePParamsUpdate args (PParamsUpdate pphkd) =
  PParamsUpdate (upgradePParamsHKD @_ @StrictMaybe args pphkd)

downgradePParamsUpdate ::
  (EraPParams era, EraPParams (PreviousEra era)) =>
  DowngradePParams StrictMaybe era ->
  PParamsUpdate era ->
  PParamsUpdate (PreviousEra era)
downgradePParamsUpdate args (PParamsUpdate pphkd) =
  PParamsUpdate (downgradePParamsHKD @_ @StrictMaybe args pphkd)
