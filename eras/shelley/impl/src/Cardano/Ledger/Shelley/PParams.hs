{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Shelley.PParams (
  emptyShelleyPParams,
  emptyShelleyPParamsUpdate,
  ShelleyPParams (..),
  emptyPParams,
  HKD,
  PPUpdateEnv (..),
  ProposedPPUpdates (..),
  emptyPPPUpdates,
  Update (..),
  upgradeUpdate,
  pvCanFollow,
  hasLegalProtVerUpdate,
  shelleyPParams,

  -- * JSON helpers
  shelleyCommonPParamsHKDPairs,
  shelleyCommonPParamsHKDPairsV6,
  shelleyCommonPParamsHKDPairsV8,

  -- * PParam
  ppA0,
  ppD,
  ppEMax,
  ppExtraEntropy,
  ppMaxBBSize,
  ppKeyDeposit,
  ppMinFeeA,
  ppMinFeeB,
  ppMinPoolCost,
  ppMaxBHSize,
  ppMaxTxSize,
  ppNOpt,
  ppProtocolVersion,
  ppPoolDeposit,
  ppRho,
  ppTau,
)
where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  Nonce (NeutralNonce),
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
  succVersion,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), decode, (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Hashes (GenDelegs)
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Control.DeepSeq (NFData)
import Data.Aeson (
  Key,
  ToJSON (..),
 )
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Void
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (lens, (^.))
import NoThunks.Class (NoThunks (..))

-- ====================================================================

-- | Protocol parameters.
data ShelleyPParams f era = ShelleyPParams
  { sppMinFeeA :: !(HKD f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , sppMinFeeB :: !(HKD f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , sppMaxBBSize :: !(HKD f Word32)
  -- ^ Maximal block body size
  , sppMaxTxSize :: !(HKD f Word32)
  -- ^ Maximal transaction size
  , sppMaxBHSize :: !(HKD f Word16)
  -- ^ Maximal block header size
  , sppKeyDeposit :: !(HKD f Coin)
  -- ^ The amount of a key registration deposit
  , sppPoolDeposit :: !(HKD f Coin)
  -- ^ The amount of a pool registration deposit
  , sppEMax :: !(HKD f EpochInterval)
  -- ^ epoch bound on pool retirement
  , sppNOpt :: !(HKD f Word16)
  -- ^ Desired number of pools
  , sppA0 :: !(HKD f NonNegativeInterval)
  -- ^ Pool influence
  , sppRho :: !(HKD f UnitInterval)
  -- ^ Monetary expansion
  , sppTau :: !(HKD f UnitInterval)
  -- ^ Treasury expansion
  , sppD :: !(HKD f UnitInterval)
  -- ^ Decentralization parameter
  , sppExtraEntropy :: !(HKD f Nonce)
  -- ^ Extra entropy
  , sppProtocolVersion :: !(HKD f ProtVer)
  -- ^ Protocol version
  , sppMinUTxOValue :: !(HKD f Coin)
  -- ^ Minimum UTxO value
  , sppMinPoolCost :: !(HKD f Coin)
  -- ^ Minimum Stake Pool Cost
  }
  deriving (Generic)

deriving instance Eq (ShelleyPParams Identity era)

deriving instance Ord (ShelleyPParams Identity era)

deriving instance Show (ShelleyPParams Identity era)

instance NoThunks (ShelleyPParams Identity era)

instance NFData (ShelleyPParams Identity era)

deriving instance Eq (ShelleyPParams StrictMaybe era)

deriving instance Ord (ShelleyPParams StrictMaybe era)

deriving instance Show (ShelleyPParams StrictMaybe era)

instance NoThunks (ShelleyPParams StrictMaybe era)

instance NFData (ShelleyPParams StrictMaybe era)

instance EraPParams ShelleyEra where
  type PParamsHKD f ShelleyEra = ShelleyPParams f ShelleyEra

  type UpgradePParams f ShelleyEra = Void
  type DowngradePParams f ShelleyEra = Void

  emptyPParamsIdentity = emptyShelleyPParams
  emptyPParamsStrictMaybe = emptyShelleyPParamsUpdate

  -- These two functions are impossible for two reasons:
  -- 1. Upgrade/DowngradePParams are Void
  -- 2. ByronEra does not have an instance for EraPParams
  --
  -- Using either one of these two functions with ShelleyEra will result in a build error:
  upgradePParamsHKD = error "IMPOSSIBLE! There cannot be PParams that can be upgraded to Shelley"
  downgradePParamsHKD = error "IMPOSSIBLE! There cannot be PParams that can be downgraded from Shelley"

  hkdMinFeeAL = lens sppMinFeeA $ \pp x -> pp {sppMinFeeA = x}
  hkdMinFeeBL = lens sppMinFeeB $ \pp x -> pp {sppMinFeeB = x}
  hkdMaxBBSizeL = lens sppMaxBBSize $ \pp x -> pp {sppMaxBBSize = x}
  hkdMaxTxSizeL = lens sppMaxTxSize $ \pp x -> pp {sppMaxTxSize = x}
  hkdMaxBHSizeL = lens sppMaxBHSize $ \pp x -> pp {sppMaxBHSize = x}
  hkdKeyDepositL = lens sppKeyDeposit $ \pp x -> pp {sppKeyDeposit = x}
  hkdPoolDepositL = lens sppPoolDeposit $ \pp x -> pp {sppPoolDeposit = x}
  hkdEMaxL = lens sppEMax $ \pp x -> pp {sppEMax = x}
  hkdNOptL = lens sppNOpt $ \pp x -> pp {sppNOpt = x}
  hkdA0L = lens sppA0 $ \pp x -> pp {sppA0 = x}
  hkdRhoL = lens sppRho $ \pp x -> pp {sppRho = x}
  hkdTauL = lens sppTau $ \pp x -> pp {sppTau = x}
  hkdDL = lens sppD $ \pp x -> pp {sppD = x}
  hkdExtraEntropyL = lens sppExtraEntropy $ \pp x -> pp {sppExtraEntropy = x}
  hkdProtocolVersionL = lens sppProtocolVersion $ \pp x -> pp {sppProtocolVersion = x}
  hkdMinUTxOValueL = lens sppMinUTxOValue $ \pp x -> pp {sppMinUTxOValue = x}
  hkdMinPoolCostL = lens sppMinPoolCost $ \pp x -> pp {sppMinPoolCost = x}

  pparams = shelleyPParams

emptyShelleyPParams :: forall era. Era era => ShelleyPParams Identity era
emptyShelleyPParams =
  ShelleyPParams
    { sppMinFeeA = Coin 0
    , sppMinFeeB = Coin 0
    , sppMaxBBSize = 0
    , sppMaxTxSize = 2048
    , sppMaxBHSize = 0
    , sppKeyDeposit = Coin 0
    , sppPoolDeposit = Coin 0
    , sppEMax = EpochInterval 0
    , sppNOpt = 100
    , sppA0 = minBound
    , sppRho = minBound
    , sppTau = minBound
    , sppD = minBound
    , sppExtraEntropy = NeutralNonce
    , sppProtocolVersion = ProtVer (eraProtVerLow @era) 0
    , sppMinUTxOValue = mempty
    , sppMinPoolCost = mempty
    }

emptyShelleyPParamsUpdate :: ShelleyPParams StrictMaybe era
emptyShelleyPParamsUpdate =
  ShelleyPParams
    { sppMinFeeA = SNothing
    , sppMinFeeB = SNothing
    , sppMaxBBSize = SNothing
    , sppMaxTxSize = SNothing
    , sppMaxBHSize = SNothing
    , sppKeyDeposit = SNothing
    , sppPoolDeposit = SNothing
    , sppEMax = SNothing
    , sppNOpt = SNothing
    , sppA0 = SNothing
    , sppRho = SNothing
    , sppTau = SNothing
    , sppD = SNothing
    , sppExtraEntropy = SNothing
    , sppProtocolVersion = SNothing
    , sppMinUTxOValue = SNothing
    , sppMinPoolCost = SNothing
    }

-- | Update Proposal
data Update era
  = Update !(ProposedPPUpdates era) !EpochNo
  deriving (Generic)

deriving instance Eq (PParamsUpdate era) => Eq (Update era)

instance NFData (PParamsUpdate era) => NFData (Update era)

deriving instance Show (PParamsUpdate era) => Show (Update era)

instance NoThunks (PParamsUpdate era) => NoThunks (Update era)

instance (Era era, EncCBOR (PParamsUpdate era)) => EncCBOR (Update era) where
  encCBOR (Update ppUpdate e) =
    encodeListLen 2 <> encCBOR ppUpdate <> encCBOR e

instance
  (Era era, DecCBOR (PParamsUpdate era)) =>
  DecCBOR (Update era)
  where
  decCBOR = decode $ RecD Update <! From <! From

data PPUpdateEnv = PPUpdateEnv SlotNo GenDelegs
  deriving (Show, Eq, Generic)

instance NoThunks PPUpdateEnv

{-# DEPRECATED PPUpdateEnv "As unused" #-}

-- | These are the fields that are common only up to major protocol version 6
shelleyCommonPParamsHKDPairsV6 ::
  forall f era.
  (HKDFunctor f, EraPParams era, ProtVerAtMost era 6) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
shelleyCommonPParamsHKDPairsV6 px pp =
  [ ("decentralization", hkdMap px (toJSON @UnitInterval) (pp ^. hkdDL @era @f))
  , ("extraPraosEntropy", hkdMap px (toJSON @Nonce) (pp ^. hkdExtraEntropyL @era @f))
  ]

shelleyCommonPParamsHKDPairsV8 ::
  forall f era.
  (HKDFunctor f, EraPParams era, ProtVerAtMost era 8) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
shelleyCommonPParamsHKDPairsV8 px pp =
  [ ("protocolVersion", hkdMap px (toJSON @ProtVer) (pp ^. hkdProtocolVersionL @era @f))
  ]

-- | These are the fields that are common across all eras
shelleyCommonPParamsHKDPairs ::
  forall f era.
  (HKDFunctor f, EraPParams era) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
shelleyCommonPParamsHKDPairs px pp =
  [ ("txFeePerByte", hkdMap px (toJSON @Coin) (pp ^. hkdMinFeeAL @_ @f :: HKD f Coin))
  , ("txFeeFixed", hkdMap px (toJSON @Coin) (pp ^. hkdMinFeeBL @era @f))
  , ("maxBlockBodySize", hkdMap px (toJSON @Word32) (pp ^. hkdMaxBBSizeL @era @f))
  , ("maxTxSize", hkdMap px (toJSON @Word32) (pp ^. hkdMaxTxSizeL @era @f))
  , ("maxBlockHeaderSize", hkdMap px (toJSON @Word16) (pp ^. hkdMaxBHSizeL @era @f))
  , ("stakeAddressDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdKeyDepositL @era @f))
  , ("stakePoolDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdPoolDepositL @era @f))
  , ("poolRetireMaxEpoch", hkdMap px (toJSON @EpochInterval) (pp ^. hkdEMaxL @era @f))
  , ("stakePoolTargetNum", hkdMap px (toJSON @Word16) (pp ^. hkdNOptL @era @f))
  , ("poolPledgeInfluence", hkdMap px (toJSON @NonNegativeInterval) (pp ^. hkdA0L @era @f))
  , ("monetaryExpansion", hkdMap px (toJSON @UnitInterval) (pp ^. hkdRhoL @era @f))
  , ("treasuryCut", hkdMap px (toJSON @UnitInterval) (pp ^. hkdTauL @era @f))
  , ("minPoolCost", hkdMap px (toJSON @Coin) (pp ^. hkdMinPoolCostL @era @f))
  ]

-- | Update operation for protocol parameters structure @PParams@
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis) (PParamsUpdate era))
  deriving (Generic, Semigroup, Monoid)

deriving instance Eq (PParamsUpdate era) => Eq (ProposedPPUpdates era)

deriving instance NFData (PParamsUpdate era) => NFData (ProposedPPUpdates era)

deriving instance Show (PParamsUpdate era) => Show (ProposedPPUpdates era)

instance NoThunks (PParamsUpdate era) => NoThunks (ProposedPPUpdates era)

deriving instance (Era era, ToCBOR (PParamsUpdate era)) => ToCBOR (ProposedPPUpdates era)

deriving instance (Era era, FromCBOR (PParamsUpdate era)) => FromCBOR (ProposedPPUpdates era)

deriving instance (Era era, EncCBOR (PParamsUpdate era)) => EncCBOR (ProposedPPUpdates era)

deriving instance (Era era, DecCBOR (PParamsUpdate era)) => DecCBOR (ProposedPPUpdates era)

instance EraPParams era => ToJSON (ProposedPPUpdates era) where
  toJSON (ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates
  toEncoding (ProposedPPUpdates ppUpdates) = toEncoding $ Map.toList ppUpdates

emptyPPPUpdates :: ProposedPPUpdates era
emptyPPPUpdates = ProposedPPUpdates Map.empty

-- | Check whether the new protocol version is a legitimate version bump with respect to the
-- previous one.
pvCanFollow ::
  -- | Previous protocol version
  ProtVer ->
  -- | New protocol version
  ProtVer ->
  Bool
pvCanFollow (ProtVer curMajor curMinor) (ProtVer newMajor newMinor) =
  (succVersion curMajor, 0) == (Just newMajor, newMinor)
    || (curMajor, curMinor + 1) == (newMajor, newMinor)

-- | Check whether `PParamsUpdate` contains a valid `ProtVer` update. When a protocol version
-- update is not included in `PParamsUpdate` it is considered a legal update.
hasLegalProtVerUpdate ::
  (ProtVerAtMost era 8, EraPParams era) => PParams era -> PParamsUpdate era -> Bool
hasLegalProtVerUpdate pp ppu =
  case ppu ^. ppuProtocolVersionL of
    SNothing -> True
    SJust newProtVer -> pvCanFollow (pp ^. ppProtocolVersionL) newProtVer

upgradeUpdate ::
  forall era.
  ( EraPParams era
  , EraPParams (PreviousEra era)
  ) =>
  UpgradePParams StrictMaybe era ->
  Update (PreviousEra era) ->
  Update era
upgradeUpdate args (Update pp epoch) = Update (upgradeProposedPPUpdates @era args pp) epoch

upgradeProposedPPUpdates ::
  ( EraPParams era
  , EraPParams (PreviousEra era)
  ) =>
  UpgradePParams StrictMaybe era ->
  ProposedPPUpdates (PreviousEra era) ->
  ProposedPPUpdates era
upgradeProposedPPUpdates args (ProposedPPUpdates ppus) =
  ProposedPPUpdates $ upgradePParamsUpdate args <$> ppus

ppA0 :: EraPParams era => PParam' era
ppA0 =
  PParam'
    { ppName = "poolPledgeInfluence"
    , ppTag = 9
    , ppLens' = ppA0L
    , ppUpdateLens = ppuA0L
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppD :: (EraPParams era, ProtVerAtMost era 6) => PParam' era
ppD =
  PParam'
    { ppName = "decentralization"
    , ppTag = 12
    , ppLens' = ppDL
    , ppUpdateLens = ppuDL
    , ppToPlutusData = Nothing
    , ppFromPlutusData = Nothing
    }

ppEMax :: EraPParams era => PParam' era
ppEMax =
  PParam'
    { ppName = "poolRetireMaxEpoch"
    , ppTag = 7
    , ppLens' = ppEMaxL
    , ppUpdateLens = ppuEMaxL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppExtraEntropy :: (EraPParams era, ProtVerAtMost era 6) => PParam' era
ppExtraEntropy =
  PParam'
    { ppName = "extraPraosEntropy"
    , ppTag = 13
    , ppLens' = ppExtraEntropyL
    , ppUpdateLens = ppuExtraEntropyL
    , ppToPlutusData = Nothing
    , ppFromPlutusData = Nothing
    }

ppKeyDeposit :: EraPParams era => PParam' era
ppKeyDeposit =
  PParam'
    { ppName = "stakeAddressDeposit"
    , ppTag = 5
    , ppLens' = ppKeyDepositL
    , ppUpdateLens = ppuKeyDepositL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxBBSize :: EraPParams era => PParam' era
ppMaxBBSize =
  PParam'
    { ppName = "maxBlockBodySize"
    , ppTag = 2
    , ppLens' = ppMaxBBSizeL
    , ppUpdateLens = ppuMaxBBSizeL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxBHSize :: EraPParams era => PParam' era
ppMaxBHSize =
  PParam'
    { ppName = "maxBlockHeaderSize"
    , ppTag = 4
    , ppLens' = ppMaxBHSizeL
    , ppUpdateLens = ppuMaxBHSizeL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxTxSize :: EraPParams era => PParam' era
ppMaxTxSize =
  PParam'
    { ppName = "maxTxSize"
    , ppTag = 3
    , ppLens' = ppMaxTxSizeL
    , ppUpdateLens = ppuMaxTxSizeL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMinFeeA :: EraPParams era => PParam' era
ppMinFeeA =
  PParam'
    { ppName = "txFeePerByte"
    , ppTag = 0
    , ppLens' = ppMinFeeAL
    , ppUpdateLens = ppuMinFeeAL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMinFeeB :: EraPParams era => PParam' era
ppMinFeeB =
  PParam'
    { ppName = "txFeeFixed"
    , ppTag = 1
    , ppLens' = ppMinFeeBL
    , ppUpdateLens = ppuMinFeeBL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMinPoolCost :: EraPParams era => PParam' era
ppMinPoolCost =
  PParam'
    { ppName = "minPoolCost"
    , ppTag = 16
    , ppLens' = ppMinPoolCostL
    , ppUpdateLens = ppuMinPoolCostL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMinUTxOValue :: (EraPParams era, ProtVerAtMost era 4) => PParam' era
ppMinUTxOValue =
  PParam'
    { ppName = "minUTxOValue"
    , ppTag = 15
    , ppLens' = ppMinUTxOValueL
    , ppUpdateLens = ppuMinUTxOValueL
    , ppToPlutusData = Nothing
    , ppFromPlutusData = Nothing
    }

ppNOpt :: EraPParams era => PParam' era
ppNOpt =
  PParam'
    { ppName = "stakePoolTargetNum"
    , ppTag = 8
    , ppLens' = ppNOptL
    , ppUpdateLens = ppuNOptL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppPoolDeposit :: EraPParams era => PParam' era
ppPoolDeposit =
  PParam'
    { ppName = "stakePoolDeposit"
    , ppTag = 6
    , ppLens' = ppPoolDepositL
    , ppUpdateLens = ppuPoolDepositL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppProtocolVersion :: (EraPParams era, ProtVerAtMost era 8) => PParam' era
ppProtocolVersion =
  PParam'
    { ppName = "protocolVersion"
    , ppTag = 14
    , ppLens' = ppProtocolVersionL
    , ppUpdateLens = ppuProtocolVersionL
    , ppToPlutusData = Nothing
    , ppFromPlutusData = Nothing
    }

ppRho :: EraPParams era => PParam' era
ppRho =
  PParam'
    { ppName = "monetaryExpansion"
    , ppTag = 10
    , ppLens' = ppRhoL
    , ppUpdateLens = ppuRhoL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppTau :: EraPParams era => PParam' era
ppTau =
  PParam'
    { ppName = "treasuryCut"
    , ppTag = 11
    , ppLens' = ppTauL
    , ppUpdateLens = ppuTauL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

shelleyPParams ::
  ( EraPParams era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , ProtVerAtMost era 8
  ) =>
  [PParam' era]
shelleyPParams =
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
  , ppD
  , ppExtraEntropy
  , ppProtocolVersion
  , ppMinUTxOValue
  , ppMinPoolCost
  ]
