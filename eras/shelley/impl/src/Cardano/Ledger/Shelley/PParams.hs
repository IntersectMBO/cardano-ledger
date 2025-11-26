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
  ProposedPPUpdates (..),
  emptyPPPUpdates,
  Update (..),
  upgradeUpdate,
  pvCanFollow,
  hasLegalProtVerUpdate,
  shelleyPParams,

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
) where

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
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Aeson (
  ToJSON (..),
 )
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (lens, (^.))
import NoThunks.Class (NoThunks (..))

-- ====================================================================

-- | Protocol parameters.
data ShelleyPParams f era = ShelleyPParams
  { sppMinFeeA :: !(HKD f (CompactForm Coin))
  -- ^ The linear factor for the minimum fee calculation
  , sppMinFeeB :: !(HKD f (CompactForm Coin))
  -- ^ The constant factor for the minimum fee calculation
  , sppMaxBBSize :: !(HKD f Word32)
  -- ^ Maximal block body size
  , sppMaxTxSize :: !(HKD f Word32)
  -- ^ Maximal transaction size
  , sppMaxBHSize :: !(HKD f Word16)
  -- ^ Maximal block header size
  , sppKeyDeposit :: !(HKD f (CompactForm Coin))
  -- ^ The amount of a key registration deposit
  , sppPoolDeposit :: !(HKD f (CompactForm Coin))
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
  , sppMinUTxOValue :: !(HKD f (CompactForm Coin))
  -- ^ Minimum UTxO value
  , sppMinPoolCost :: !(HKD f (CompactForm Coin))
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
  emptyUpgradePParamsUpdate = error "IMPOSSIBLE! There is no UpgradePParams in ShelleyEra"

  hkdMinFeeACompactL = lens sppMinFeeA $ \pp x -> pp {sppMinFeeA = x}
  hkdMinFeeBCompactL = lens sppMinFeeB $ \pp x -> pp {sppMinFeeB = x}
  hkdMaxBBSizeL = lens sppMaxBBSize $ \pp x -> pp {sppMaxBBSize = x}
  hkdMaxTxSizeL = lens sppMaxTxSize $ \pp x -> pp {sppMaxTxSize = x}
  hkdMaxBHSizeL = lens sppMaxBHSize $ \pp x -> pp {sppMaxBHSize = x}
  hkdKeyDepositCompactL = lens sppKeyDeposit $ \pp x -> pp {sppKeyDeposit = x}
  hkdPoolDepositCompactL = lens sppPoolDeposit $ \pp x -> pp {sppPoolDeposit = x}
  hkdEMaxL = lens sppEMax $ \pp x -> pp {sppEMax = x}
  hkdNOptL = lens sppNOpt $ \pp x -> pp {sppNOpt = x}
  hkdA0L = lens sppA0 $ \pp x -> pp {sppA0 = x}
  hkdRhoL = lens sppRho $ \pp x -> pp {sppRho = x}
  hkdTauL = lens sppTau $ \pp x -> pp {sppTau = x}
  hkdDL = lens sppD $ \pp x -> pp {sppD = x}
  hkdExtraEntropyL = lens sppExtraEntropy $ \pp x -> pp {sppExtraEntropy = x}
  hkdProtocolVersionL = lens sppProtocolVersion $ \pp x -> pp {sppProtocolVersion = x}
  hkdMinUTxOValueCompactL = lens sppMinUTxOValue $ \pp x -> pp {sppMinUTxOValue = x}
  hkdMinPoolCostCompactL = lens sppMinPoolCost $ \pp x -> pp {sppMinPoolCost = x}

  eraPParams = shelleyPParams

emptyShelleyPParams :: forall era. Era era => ShelleyPParams Identity era
emptyShelleyPParams =
  ShelleyPParams
    { sppMinFeeA = CompactCoin 0
    , sppMinFeeB = CompactCoin 0
    , sppMaxBBSize = 0
    , sppMaxTxSize = 2048
    , sppMaxBHSize = 0
    , sppKeyDeposit = CompactCoin 0
    , sppPoolDeposit = CompactCoin 0
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

-- | Update operation for protocol parameters structure @PParams@
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash GenesisRole) (PParamsUpdate era))
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
  (AtMostEra "Babbage" era, EraPParams era) => PParams era -> PParamsUpdate era -> Bool
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

ppMinFeeA :: EraPParams era => PParam era
ppMinFeeA =
  PParam
    { ppName = "txFeePerByte"
    , ppLens = ppMinFeeACompactL
    , ppUpdate = Just $ PParamUpdate 0 ppuMinFeeACompactL
    }

ppMinFeeB :: EraPParams era => PParam era
ppMinFeeB =
  PParam
    { ppName = "txFeeFixed"
    , ppLens = ppMinFeeBCompactL
    , ppUpdate = Just $ PParamUpdate 1 ppuMinFeeBCompactL
    }

ppMaxBBSize :: EraPParams era => PParam era
ppMaxBBSize =
  PParam
    { ppName = "maxBlockBodySize"
    , ppLens = ppMaxBBSizeL
    , ppUpdate = Just $ PParamUpdate 2 ppuMaxBBSizeL
    }

ppMaxTxSize :: EraPParams era => PParam era
ppMaxTxSize =
  PParam
    { ppName = "maxTxSize"
    , ppLens = ppMaxTxSizeL
    , ppUpdate = Just $ PParamUpdate 3 ppuMaxTxSizeL
    }

ppMaxBHSize :: EraPParams era => PParam era
ppMaxBHSize =
  PParam
    { ppName = "maxBlockHeaderSize"
    , ppLens = ppMaxBHSizeL
    , ppUpdate = Just $ PParamUpdate 4 ppuMaxBHSizeL
    }

ppKeyDeposit :: EraPParams era => PParam era
ppKeyDeposit =
  PParam
    { ppName = "stakeAddressDeposit"
    , ppLens = ppKeyDepositCompactL
    , ppUpdate = Just $ PParamUpdate 5 ppuKeyDepositCompactL
    }

ppPoolDeposit :: EraPParams era => PParam era
ppPoolDeposit =
  PParam
    { ppName = "stakePoolDeposit"
    , ppLens = ppPoolDepositCompactL
    , ppUpdate = Just $ PParamUpdate 6 ppuPoolDepositCompactL
    }

ppEMax :: EraPParams era => PParam era
ppEMax =
  PParam
    { ppName = "poolRetireMaxEpoch"
    , ppLens = ppEMaxL
    , ppUpdate = Just $ PParamUpdate 7 ppuEMaxL
    }

ppNOpt :: EraPParams era => PParam era
ppNOpt =
  PParam
    { ppName = "stakePoolTargetNum"
    , ppLens = ppNOptL
    , ppUpdate = Just $ PParamUpdate 8 ppuNOptL
    }

ppA0 :: EraPParams era => PParam era
ppA0 =
  PParam
    { ppName = "poolPledgeInfluence"
    , ppLens = ppA0L
    , ppUpdate = Just $ PParamUpdate 9 ppuA0L
    }

ppRho :: EraPParams era => PParam era
ppRho =
  PParam
    { ppName = "monetaryExpansion"
    , ppLens = ppRhoL
    , ppUpdate = Just $ PParamUpdate 10 ppuRhoL
    }

ppTau :: EraPParams era => PParam era
ppTau =
  PParam
    { ppName = "treasuryCut"
    , ppLens = ppTauL
    , ppUpdate = Just $ PParamUpdate 11 ppuTauL
    }

ppD :: (EraPParams era, AtMostEra "Alonzo" era) => PParam era
ppD =
  PParam
    { ppName = "decentralization"
    , ppLens = ppDL
    , ppUpdate = Just $ PParamUpdate 12 ppuDL
    }

ppExtraEntropy :: (EraPParams era, AtMostEra "Alonzo" era) => PParam era
ppExtraEntropy =
  PParam
    { ppName = "extraPraosEntropy"
    , ppLens = ppExtraEntropyL
    , ppUpdate = Just $ PParamUpdate 13 ppuExtraEntropyL
    }

ppProtocolVersion :: (EraPParams era, AtMostEra "Babbage" era) => PParam era
ppProtocolVersion =
  PParam
    { ppName = "protocolVersion"
    , ppLens = ppProtocolVersionL
    , ppUpdate = Just $ PParamUpdate 14 ppuProtocolVersionL
    }

ppMinUTxOValue :: (EraPParams era, AtMostEra "Mary" era) => PParam era
ppMinUTxOValue =
  PParam
    { ppName = "minUTxOValue"
    , ppLens = ppMinUTxOValueCompactL
    , ppUpdate = Just $ PParamUpdate 15 ppuMinUTxOValueCompactL
    }

ppMinPoolCost :: EraPParams era => PParam era
ppMinPoolCost =
  PParam
    { ppName = "minPoolCost"
    , ppLens = ppMinPoolCostCompactL
    , ppUpdate = Just $ PParamUpdate 16 ppuMinPoolCostCompactL
    }

shelleyPParams ::
  ( EraPParams era
  , AtMostEra "Mary" era
  , AtMostEra "Alonzo" era
  , AtMostEra "Babbage" era
  ) =>
  [PParam era]
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
