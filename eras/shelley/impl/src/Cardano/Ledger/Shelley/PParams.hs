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
  pvCanFollow,
  hasLegalProtVerUpdate,

  -- * JSON helpers
  shelleyCommonPParamsHKDPairs,
  shelleyCommonPParamsHKDPairsV6,
  shelleyCommonPParamsHKDPairsV8,

  -- * Deprecated
  updatePParams,
  upgradeUpdate,
)
where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  Nonce (NeutralNonce),
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
  invalidKey,
  strictMaybeToMaybe,
  succVersion,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeMapContents,
  decodeRecordNamed,
  decodeWord,
  encodeListLen,
  encodeMapLen,
  encodeWord,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), decode, (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (..),
  Key,
  KeyValue,
  ToJSON (..),
  object,
  pairs,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Void
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (lens, (^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

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
  , sppNOpt :: !(HKD f Natural)
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

instance Crypto c => EraPParams (ShelleyEra c) where
  type PParamsHKD f (ShelleyEra c) = ShelleyPParams f (ShelleyEra c)

  type UpgradePParams f (ShelleyEra c) = Void
  type DowngradePParams f (ShelleyEra c) = Void

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

instance Era era => EncCBOR (ShelleyPParams Identity era) where
  encCBOR
    ShelleyPParams {..} =
      encodeListLen 18
        <> encCBOR sppMinFeeA
        <> encCBOR sppMinFeeB
        <> encCBOR sppMaxBBSize
        <> encCBOR sppMaxTxSize
        <> encCBOR sppMaxBHSize
        <> encCBOR sppKeyDeposit
        <> encCBOR sppPoolDeposit
        <> encCBOR sppEMax
        <> encCBOR sppNOpt
        <> encCBOR sppA0
        <> encCBOR sppRho
        <> encCBOR sppTau
        <> encCBOR sppD
        <> encCBOR sppExtraEntropy
        <> encCBORGroup sppProtocolVersion
        <> encCBOR sppMinUTxOValue
        <> encCBOR sppMinPoolCost

instance Era era => DecCBOR (ShelleyPParams Identity era) where
  decCBOR = do
    decodeRecordNamed "ShelleyPParams" (const 18) $
      ShelleyPParams @Identity
        <$> decCBOR -- sppMinFeeA         :: Integer
        <*> decCBOR -- sppMinFeeB         :: Natural
        <*> decCBOR -- sppMaxBBSize       :: Natural
        <*> decCBOR -- sppMaxTxSize       :: Natural
        <*> decCBOR -- sppMaxBHSize       :: Natural
        <*> decCBOR -- sppKeyDeposit      :: Coin
        <*> decCBOR -- sppPoolDeposit     :: Coin
        <*> decCBOR -- sppEMax            :: EpochNo
        <*> decCBOR -- sppNOpt            :: Natural
        <*> decCBOR -- sppA0              :: NonNegativeInterval
        <*> decCBOR -- sppRho             :: UnitInterval
        <*> decCBOR -- sppTau             :: UnitInterval
        <*> decCBOR -- sppD               :: UnitInterval
        <*> decCBOR -- sppExtraEntropy    :: Nonce
        <*> decCBORGroup -- sppProtocolVersion :: ProtVer
        <*> decCBOR -- sppMinUTxOValue    :: Natural
        <*> decCBOR -- sppMinPoolCost     :: Natural

instance Era era => ToCBOR (ShelleyPParams Identity era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (ShelleyPParams Identity era) where
  fromCBOR = fromEraCBOR @era

instance
  ( EraPParams era
  , PParamsHKD Identity era ~ ShelleyPParams Identity era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , ProtVerAtMost era 8
  ) =>
  ToJSON (ShelleyPParams Identity era)
  where
  toJSON = object . shelleyPParamsPairs
  toEncoding = pairs . mconcat . shelleyPParamsPairs

shelleyPParamsPairs ::
  forall era a e.
  (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8, KeyValue e a) =>
  PParamsHKD Identity era ->
  [a]
shelleyPParamsPairs pp =
  uncurry (.=)
    <$> shelleyPParamsHKDPairs (Proxy @Identity) pp

instance FromJSON (ShelleyPParams Identity era) where
  parseJSON =
    Aeson.withObject "ShelleyPParams" $ \obj -> do
      ShelleyPParams
        <$> obj .: "txFeePerByte"
        <*> obj .: "txFeeFixed"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "stakeAddressDeposit"
        <*> obj .: "stakePoolDeposit"
        <*> obj .: "poolRetireMaxEpoch"
        <*> obj .: "stakePoolTargetNum"
        <*> obj .: "poolPledgeInfluence"
        <*> obj .: "monetaryExpansion"
        <*> obj .: "treasuryCut"
        <*> obj .: "decentralization"
        <*> obj .: "extraPraosEntropy"
        <*> obj .: "protocolVersion"
        <*> obj .:? "minUTxOValue" .!= mempty
        <*> obj .:? "minPoolCost" .!= mempty

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

data PPUpdateEnv era = PPUpdateEnv SlotNo (GenDelegs era)
  deriving (Show, Eq, Generic)

instance NoThunks (PPUpdateEnv era)

instance Era era => EncCBOR (ShelleyPParams StrictMaybe era) where
  encCBOR ppup =
    let l =
          mapMaybe
            strictMaybeToMaybe
            [ encodeMapElement 0 encCBOR =<< sppMinFeeA ppup
            , encodeMapElement 1 encCBOR =<< sppMinFeeB ppup
            , encodeMapElement 2 encCBOR =<< sppMaxBBSize ppup
            , encodeMapElement 3 encCBOR =<< sppMaxTxSize ppup
            , encodeMapElement 4 encCBOR =<< sppMaxBHSize ppup
            , encodeMapElement 5 encCBOR =<< sppKeyDeposit ppup
            , encodeMapElement 6 encCBOR =<< sppPoolDeposit ppup
            , encodeMapElement 7 encCBOR =<< sppEMax ppup
            , encodeMapElement 8 encCBOR =<< sppNOpt ppup
            , encodeMapElement 9 encCBOR =<< sppA0 ppup
            , encodeMapElement 10 encCBOR =<< sppRho ppup
            , encodeMapElement 11 encCBOR =<< sppTau ppup
            , encodeMapElement 12 encCBOR =<< sppD ppup
            , encodeMapElement 13 encCBOR =<< sppExtraEntropy ppup
            , encodeMapElement 14 encCBOR =<< sppProtocolVersion ppup
            , encodeMapElement 15 encCBOR =<< sppMinUTxOValue ppup
            , encodeMapElement 16 encCBOR =<< sppMinPoolCost ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement i encoder x = SJust (encodeWord i <> encoder x)

instance Era era => DecCBOR (ShelleyPParams StrictMaybe era) where
  decCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> decCBOR >>= \x -> pure (0, \up -> up {sppMinFeeA = SJust x})
          1 -> decCBOR >>= \x -> pure (1, \up -> up {sppMinFeeB = SJust x})
          2 -> decCBOR >>= \x -> pure (2, \up -> up {sppMaxBBSize = SJust x})
          3 -> decCBOR >>= \x -> pure (3, \up -> up {sppMaxTxSize = SJust x})
          4 -> decCBOR >>= \x -> pure (4, \up -> up {sppMaxBHSize = SJust x})
          5 -> decCBOR >>= \x -> pure (5, \up -> up {sppKeyDeposit = SJust x})
          6 -> decCBOR >>= \x -> pure (6, \up -> up {sppPoolDeposit = SJust x})
          7 -> decCBOR >>= \x -> pure (7, \up -> up {sppEMax = SJust x})
          8 -> decCBOR >>= \x -> pure (8, \up -> up {sppNOpt = SJust x})
          9 -> decCBOR >>= \x -> pure (9, \up -> up {sppA0 = SJust x})
          10 -> decCBOR >>= \x -> pure (10, \up -> up {sppRho = SJust x})
          11 -> decCBOR >>= \x -> pure (11, \up -> up {sppTau = SJust x})
          12 -> decCBOR >>= \x -> pure (12, \up -> up {sppD = SJust x})
          13 -> decCBOR >>= \x -> pure (13, \up -> up {sppExtraEntropy = SJust x})
          14 -> decCBOR >>= \x -> pure (14, \up -> up {sppProtocolVersion = SJust x})
          15 -> decCBOR >>= \x -> pure (15, \up -> up {sppMinUTxOValue = SJust x})
          16 -> decCBOR >>= \x -> pure (16, \up -> up {sppMinPoolCost = SJust x})
          k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) emptyShelleyPParamsUpdate (snd <$> mapParts)

instance Era era => ToCBOR (ShelleyPParams StrictMaybe era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (ShelleyPParams StrictMaybe era) where
  fromCBOR = fromEraCBOR @era

instance
  ( EraPParams era
  , PParamsHKD StrictMaybe era ~ ShelleyPParams StrictMaybe era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , ProtVerAtMost era 8
  ) =>
  ToJSON (ShelleyPParams StrictMaybe era)
  where
  toJSON = object . shelleyPParamsUpdatePairs
  toEncoding = pairs . mconcat . shelleyPParamsUpdatePairs

shelleyPParamsUpdatePairs ::
  forall era a e.
  (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8, KeyValue e a) =>
  PParamsHKD StrictMaybe era ->
  [a]
shelleyPParamsUpdatePairs pp =
  [ k .= v
  | (k, SJust v) <- shelleyPParamsHKDPairs (Proxy @StrictMaybe) pp
  ]

shelleyPParamsHKDPairs ::
  forall f era.
  (HKDFunctor f, EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
shelleyPParamsHKDPairs px pp =
  shelleyCommonPParamsHKDPairs px pp
    ++ shelleyCommonPParamsHKDPairsV6 px pp
    ++ shelleyCommonPParamsHKDPairsV8 px pp
    ++ [("minUTxOValue", hkdMap px (toJSON @Coin) (pp ^. hkdMinUTxOValueL @era @f))]

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
  , ("stakePoolTargetNum", hkdMap px (toJSON @Natural) (pp ^. hkdNOptL @era @f))
  , ("poolPledgeInfluence", hkdMap px (toJSON @NonNegativeInterval) (pp ^. hkdA0L @era @f))
  , ("monetaryExpansion", hkdMap px (toJSON @UnitInterval) (pp ^. hkdRhoL @era @f))
  , ("treasuryCut", hkdMap px (toJSON @UnitInterval) (pp ^. hkdTauL @era @f))
  , ("minPoolCost", hkdMap px (toJSON @Coin) (pp ^. hkdMinPoolCostL @era @f))
  ]

-- | Update operation for protocol parameters structure @PParams@
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
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

updatePParams :: EraPParams era => PParams era -> PParamsUpdate era -> PParams era
updatePParams = applyPPUpdates
{-# DEPRECATED updatePParams "Use applyPPUpdates instead" #-}

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
  , EraCrypto (PreviousEra era) ~ EraCrypto era
  ) =>
  UpgradePParams StrictMaybe era ->
  Update (PreviousEra era) ->
  Update era
upgradeUpdate args (Update pp epoch) = Update (upgradeProposedPPUpdates @era args pp) epoch

upgradeProposedPPUpdates ::
  ( EraPParams era
  , EraPParams (PreviousEra era)
  , EraCrypto (PreviousEra era) ~ EraCrypto era
  ) =>
  UpgradePParams StrictMaybe era ->
  ProposedPPUpdates (PreviousEra era) ->
  ProposedPPUpdates era
upgradeProposedPPUpdates args (ProposedPPUpdates ppus) =
  ProposedPPUpdates $ upgradePParamsUpdate args <$> ppus
