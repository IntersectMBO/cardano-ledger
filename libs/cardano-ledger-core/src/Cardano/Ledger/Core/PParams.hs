{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Core.PParams (
  EraPParams (..),
  PParams (..),
  PParam' (..),
  PParamUpdate (..),
  emptyPParams,
  PParamsUpdate (..),
  emptyPParamsUpdate,
  genericApplyPPUpdates,

  -- * PParams lens
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
  ppMinUTxOValueL,
  ppMinPoolCostL,

  -- * PParamsUpdate lens
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
  ppuMinUTxOValueL,
  ppuMinPoolCostL,

  -- * Utility
  ppLensHKD,
  ppuLensHKD,
  mapPParams,
  mapPParamsUpdate,
  upgradePParams,
  downgradePParams,
  upgradePParamsUpdate,
  downgradePParamsUpdate,

  -- * PParamsUpdate to Data
  PParam (..),
  makePParamMap,
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  Nonce (..),
  ProtVer,
  StrictMaybe (..),
  UnitInterval,
  maybeToStrictMaybe,
 )
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core.Era (Era (..), PreviousEra, ProtVerAtMost, fromEraCBOR, toEraCBOR)
import Cardano.Ledger.HKD (HKD, HKDApplicative, HKDFunctor (..), NoUpdate (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity)
import Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, (.:), (.=))
import qualified Data.Aeson as Aeson (KeyValue, withObject)
import qualified Data.Aeson.Key as Aeson (fromText)
import Data.Default (Default (..))
import qualified Data.Foldable as F (foldMap', foldl', foldlM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1, V1, type (:*:) (..))
import Lens.Micro (Lens', SimpleGetter, lens, set, (^.))
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

deriving stock instance
  Show (PParamsHKD Identity era) => Show (PParams era)

instance EraPParams era => ToJSON (PParams era) where
  toJSON = object . jsonPairsPParams
  toEncoding = pairs . mconcat . jsonPairsPParams

instance EraPParams era => FromJSON (PParams era) where
  parseJSON =
    Aeson.withObject (show . typeRep $ Proxy @(PParams era)) $ \obj ->
      let accum acc PParam' {ppName, ppLens} =
            set ppLens <$> obj .: Aeson.fromText ppName <*> pure acc
       in F.foldlM accum (emptyPParams @era) (eraPParams @era)

instance EraPParams era => EncCBOR (PParams era) where
  encCBOR pp =
    encodeListLen (fromIntegral (length (eraPParams @era)))
      <> F.foldMap' toEnc (eraPParams @era)
    where
      toEnc PParam' {ppLens} = encCBOR $ pp ^. ppLens

instance EraPParams era => DecCBOR (PParams era) where
  decCBOR =
    decodeRecordNamed
      (T.pack . show . typeRep $ Proxy @(PParams era))
      (const (fromIntegral (length (eraPParams @era))))
      $ F.foldlM accum (emptyPParams @era) (eraPParams @era)
    where
      accum acc PParam' {ppLens} =
        set ppLens <$> decCBOR <*> pure acc

instance EraPParams era => ToCBOR (PParams era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (PParams era) where
  fromCBOR = fromEraCBOR @era

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

deriving stock instance
  Show (PParamsHKD StrictMaybe era) => Show (PParamsUpdate era)

instance EraPParams era => EncCBOR (PParamsUpdate era) where
  encCBOR pp = encodeMapLen count <> enc
    where
      !(!count, !enc) = countAndConcat (eraPParams @era)
      encodeField PParam' {ppUpdate} = do
        PParamUpdate {ppuTag, ppuLens} <- maybeToStrictMaybe ppUpdate
        (encodeWord ppuTag <>) . encCBOR <$> pp ^. ppuLens
      countAndConcat = F.foldl' accum (0, mempty)
        where
          accum (!n, !acc) x = case encodeField x of
            SJust y -> (n + 1, acc <> y)
            SNothing -> (n, acc)

instance EraPParams era => DecCBOR (PParamsUpdate era) where
  decCBOR =
    decode $
      SparseKeyed
        (show . typeRep $ Proxy @(PParamsUpdate era))
        emptyPParamsUpdate
        updateField
        []
    where
      updateField k =
        IntMap.findWithDefault
          (invalidField k)
          (fromIntegral k)
          updateFieldMap
      updateFieldMap :: IntMap (Field (PParamsUpdate era))
      updateFieldMap =
        IntMap.fromList
          [ (fromIntegral ppuTag, field (set ppuLens . SJust) From)
          | PParam' {ppUpdate = Just PParamUpdate {ppuTag, ppuLens}} <- eraPParams @era
          ]

instance EraPParams era => ToCBOR (PParamsUpdate era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (PParamsUpdate era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => ToJSON (PParamsUpdate era) where
  toJSON = object . jsonPairsPParamsUpdate
  toEncoding = pairs . mconcat . jsonPairsPParamsUpdate

deriving instance Generic (PParamsUpdate era)

-- Generic derivation of `applyPPUpdates`

class Updatable a u where
  applyUpdate :: a -> u -> a

instance Updatable (U1 a) (U1 u) where
  applyUpdate x _ = x

instance Updatable (V1 a) (V1 u) where
  applyUpdate x _ = case x of {}

instance
  (Updatable (a1 a) (u1 u), Updatable (a2 a) (u2 u)) =>
  Updatable ((a1 :*: a2) a) ((u1 :*: u2) u)
  where
  applyUpdate (x1 :*: x2) (u1 :*: u2) = applyUpdate x1 u1 :*: applyUpdate x2 u2

instance Updatable (a x) (a' x') => Updatable (M1 i c a x) (M1 i' c' a' x') where
  applyUpdate (M1 x) (M1 y) = M1 $ applyUpdate x y

instance Updatable (K1 t x a) (K1 t (StrictMaybe x) u) where
  applyUpdate (K1 x') (K1 sm) = K1 $ case sm of
    SJust x -> x
    SNothing -> x'

instance Updatable (K1 t x a) (K1 t (NoUpdate x) u) where
  applyUpdate (K1 x) (K1 NoUpdate) = K1 x

genericApplyPPUpdates ::
  forall era a u.
  ( Generic (PParamsHKD Identity era)
  , Generic (PParamsHKD StrictMaybe era)
  , Updatable (Rep (PParamsHKD Identity era) a) (Rep (PParamsHKD StrictMaybe era) u)
  ) =>
  PParams era ->
  PParamsUpdate era ->
  PParams era
genericApplyPPUpdates (PParams a) (PParamsUpdate u) =
  PParams . to $ applyUpdate (from @_ @a a) (from @_ @u u)

class
  ( Era era
  , Eq (PParamsHKD Identity era)
  , Ord (PParamsHKD Identity era)
  , Show (PParamsHKD Identity era)
  , NFData (PParamsHKD Identity era)
  , NoThunks (PParamsHKD Identity era)
  , Eq (PParamsHKD StrictMaybe era)
  , Ord (PParamsHKD StrictMaybe era)
  , Show (PParamsHKD StrictMaybe era)
  , NFData (PParamsHKD StrictMaybe era)
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
  applyPPUpdates = genericApplyPPUpdates @_ @a @u

  emptyPParamsIdentity :: PParamsHKD Identity era
  emptyPParamsStrictMaybe :: PParamsHKD StrictMaybe era

  -- |
  type UpgradePParams (f :: Type -> Type) era :: Type

  type DowngradePParams (f :: Type -> Type) era :: Type

  -- | Upgrade PParams from previous era to the current one
  upgradePParamsHKD ::
    (HKDApplicative f, EraPParams (PreviousEra era)) =>
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
  hkdMaxBBSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Word32)

  -- | Maximal transaction size
  hkdMaxTxSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Word32)

  -- | Maximal block header size
  hkdMaxBHSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Word16)

  -- | The amount of a key registration deposit
  hkdKeyDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | The amount of a pool registration deposit
  hkdPoolDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | epoch bound on pool retirement
  hkdEMaxL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)

  -- | Desired number of pools
  hkdNOptL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Word16)

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
  ppDG = ppLensHKD . hkdDL @era @Identity

  -- | Extra entropy
  hkdExtraEntropyL :: (HKDFunctor f, ProtVerAtMost era 6) => Lens' (PParamsHKD f era) (HKD f Nonce)

  -- | Protocol version
  hkdProtocolVersionL ::
    (HKDFunctor f, ProtVerAtMost era 8) => Lens' (PParamsHKD f era) (HKD f ProtVer)

  ppProtocolVersionL :: Lens' (PParams era) ProtVer
  default ppProtocolVersionL :: ProtVerAtMost era 8 => Lens' (PParams era) ProtVer
  ppProtocolVersionL = ppLensHKD . hkdProtocolVersionL @era @Identity

  -- | PParamsUpdate Protocol version
  ppuProtocolVersionL :: ProtVerAtMost era 8 => Lens' (PParamsUpdate era) (StrictMaybe ProtVer)
  ppuProtocolVersionL = ppuLensHKD . hkdProtocolVersionL @era @StrictMaybe

  -- | Minimum UTxO value
  hkdMinUTxOValueL :: HKDFunctor f => ProtVerAtMost era 4 => Lens' (PParamsHKD f era) (HKD f Coin)

  -- | Minimum Stake Pool Cost
  hkdMinPoolCostL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)

  eraPParams :: [PParam' era]

emptyPParams :: EraPParams era => PParams era
emptyPParams = PParams emptyPParamsIdentity

emptyPParamsUpdate :: EraPParams era => PParamsUpdate era
emptyPParamsUpdate = PParamsUpdate emptyPParamsStrictMaybe

ppLensHKD :: Lens' (PParams era) (PParamsHKD Identity era)
ppLensHKD = lens (\(PParams x) -> x) (\_ pp -> PParams pp)

ppuLensHKD :: Lens' (PParamsUpdate era) (PParamsHKD StrictMaybe era)
ppuLensHKD = lens (\(PParamsUpdate x) -> x) (\_ pp -> PParamsUpdate pp)

-- PParams versions of lenses

-- | The linear factor for the minimum fee calculation
ppMinFeeAL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinFeeAL = ppLensHKD . hkdMinFeeAL @era @Identity

-- | The constant factor for the minimum fee calculation
ppMinFeeBL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinFeeBL = ppLensHKD . hkdMinFeeBL @era @Identity

-- | Maximal block body size
ppMaxBBSizeL :: forall era. EraPParams era => Lens' (PParams era) Word32
ppMaxBBSizeL = ppLensHKD . hkdMaxBBSizeL @era @Identity

-- | Maximal transaction size
ppMaxTxSizeL :: forall era. EraPParams era => Lens' (PParams era) Word32
ppMaxTxSizeL = ppLensHKD . hkdMaxTxSizeL @era @Identity

-- | Maximal block header size
ppMaxBHSizeL :: forall era. EraPParams era => Lens' (PParams era) Word16
ppMaxBHSizeL = ppLensHKD . hkdMaxBHSizeL @era @Identity

-- | The amount of a key registration deposit
ppKeyDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppKeyDepositL = ppLensHKD . hkdKeyDepositL @era @Identity

-- | The amount of a pool registration deposit
ppPoolDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppPoolDepositL = ppLensHKD . hkdPoolDepositL @era @Identity

-- | epoch bound on pool retirement
ppEMaxL :: forall era. EraPParams era => Lens' (PParams era) EpochInterval
ppEMaxL = ppLensHKD . hkdEMaxL @era @Identity

-- | Desired number of pools
ppNOptL :: forall era. EraPParams era => Lens' (PParams era) Word16
ppNOptL = ppLensHKD . hkdNOptL @era @Identity

-- | Pool influence
ppA0L :: forall era. EraPParams era => Lens' (PParams era) NonNegativeInterval
ppA0L = ppLensHKD . hkdA0L @era @Identity

-- | Monetary expansion
ppRhoL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppRhoL = ppLensHKD . hkdRhoL @era @Identity

-- | Treasury expansion
ppTauL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppTauL = ppLensHKD . hkdTauL @era @Identity

-- | Decentralization parameter
ppDL :: forall era. (EraPParams era, ProtVerAtMost era 6) => Lens' (PParams era) UnitInterval
ppDL = ppLensHKD . hkdDL @era @Identity

-- | Extra entropy
ppExtraEntropyL :: forall era. (EraPParams era, ProtVerAtMost era 6) => Lens' (PParams era) Nonce
ppExtraEntropyL = ppLensHKD . hkdExtraEntropyL @era @Identity

-- | Minimum UTxO value
ppMinUTxOValueL :: forall era. (EraPParams era, ProtVerAtMost era 4) => Lens' (PParams era) Coin
ppMinUTxOValueL = ppLensHKD . hkdMinUTxOValueL @era @Identity

-- | Minimum Stake Pool Cost
ppMinPoolCostL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinPoolCostL = ppLensHKD . hkdMinPoolCostL @era @Identity

-- PParamsUpdate versions of lenses

-- | The linear factor for the minimum fee calculation
ppuMinFeeAL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinFeeAL = ppuLensHKD . hkdMinFeeAL @era @StrictMaybe

-- | The constant factor for the minimum fee calculation
ppuMinFeeBL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinFeeBL = ppuLensHKD . hkdMinFeeBL @era @StrictMaybe

-- | Maximal block body size
ppuMaxBBSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word32)
ppuMaxBBSizeL = ppuLensHKD . hkdMaxBBSizeL @era @StrictMaybe

-- | Maximal transaction size
ppuMaxTxSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word32)
ppuMaxTxSizeL = ppuLensHKD . hkdMaxTxSizeL @era @StrictMaybe

-- | Maximal block header size
ppuMaxBHSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word16)
ppuMaxBHSizeL = ppuLensHKD . hkdMaxBHSizeL @era @StrictMaybe

-- | The amount of a key registration deposit
ppuKeyDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuKeyDepositL = ppuLensHKD . hkdKeyDepositL @era @StrictMaybe

-- | The amount of a pool registration deposit
ppuPoolDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuPoolDepositL = ppuLensHKD . hkdPoolDepositL @era @StrictMaybe

-- | epoch bound on pool retirement
ppuEMaxL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuEMaxL = ppuLensHKD . hkdEMaxL @era @StrictMaybe

-- | Desired number of pools
ppuNOptL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word16)
ppuNOptL = ppuLensHKD . hkdNOptL @era @StrictMaybe

-- | Pool influence
ppuA0L :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe NonNegativeInterval)
ppuA0L = ppuLensHKD . hkdA0L @era @StrictMaybe

-- | Monetary expansion
ppuRhoL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuRhoL = ppuLensHKD . hkdRhoL @era @StrictMaybe

-- | Treasury expansion
ppuTauL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuTauL = ppuLensHKD . hkdTauL @era @StrictMaybe

-- | Decentralization parameter
ppuDL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 6) =>
  Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuDL = ppuLensHKD . hkdDL @era @StrictMaybe

-- | Extra entropy
ppuExtraEntropyL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 6) =>
  Lens' (PParamsUpdate era) (StrictMaybe Nonce)
ppuExtraEntropyL = ppuLensHKD . hkdExtraEntropyL @era @StrictMaybe

-- | Minimum UTxO value
ppuMinUTxOValueL ::
  forall era.
  (EraPParams era, ProtVerAtMost era 4) =>
  Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinUTxOValueL = ppuLensHKD . hkdMinUTxOValueL @era @StrictMaybe

-- | Minimum Stake Pool Cost
ppuMinPoolCostL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinPoolCostL = ppuLensHKD . hkdMinPoolCostL @era @StrictMaybe

jsonPairsPParams :: forall era e a. EraPParams era => Aeson.KeyValue e a => PParams era -> [a]
jsonPairsPParams pp =
  [ Aeson.fromText ppName .= toJSON (pp ^. ppLens)
  | PParam' {ppName, ppLens} <- eraPParams @era
  ]

jsonPairsPParamsUpdate ::
  forall era e a.
  (EraPParams era, Aeson.KeyValue e a) =>
  PParamsUpdate era -> [a]
jsonPairsPParamsUpdate ppu =
  [ Aeson.fromText ppName .= toJSON v
  | PParam' {ppName, ppUpdate = Just (PParamUpdate {ppuLens})} <- eraPParams @era
  , SJust v <- [ppu ^. ppuLens]
  ]

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

-- =====================================================================================
-- Tools for building ToPlutusData instances for (PParamUpdates era).

-- | Pair the tag, and exisitenially hide the type of the lens for the field with that Lens'
data PParam era where
  PParam :: ToPlutusData t => Word -> Lens' (PParamsUpdate era) (StrictMaybe t) -> PParam era

-- | Turn a list into a Map, this assures we have no duplicates.
makePParamMap :: [PParam era] -> Map Word (PParam era)
makePParamMap xs = Map.fromList [(n, p) | p@(PParam n _) <- xs]

-- | Represents a single protocol parameter and the data required to serialize it.
data PParam' era where
  PParam' ::
    (DecCBOR t, EncCBOR t, FromJSON t, ToJSON t) =>
    { ppName :: Text
    -- ^ Used as JSON key
    , ppLens :: Lens' (PParams era) t
    , ppUpdate :: Maybe (PParamUpdate era t)
    -- ^ Not all protocol parameters have an update functionality in all eras
    } ->
    PParam' era

data PParamUpdate era t = PParamUpdate
  { ppuTag :: Word
  -- ^ Used in CBOR and Plutus Data encoding of
  , ppuLens :: Lens' (PParamsUpdate era) (StrictMaybe t)
  }
