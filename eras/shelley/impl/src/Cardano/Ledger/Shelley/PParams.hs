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

  -- * Deprecated
  updatePParams,
)
where

import Cardano.Ledger.BaseTypes (
  NonNegativeInterval,
  Nonce (NeutralNonce),
  StrictMaybe (..),
  UnitInterval,
  invalidKey,
  strictMaybeToMaybe,
  succVersion,
 )
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  FromCBORGroup (..),
  ToCBOR (..),
  ToCBORGroup (..),
  decodeMapContents,
  decodeRecordNamed,
  decodeWord,
  encodeListLen,
  encodeMapLen,
  encodeWord,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), decode, (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro (lens)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- ====================================================================

-- | Protocol parameters.
data ShelleyPParams f era = ShelleyPParams
  { sppMinFeeA :: !(HKD f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , sppMinFeeB :: !(HKD f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , sppMaxBBSize :: !(HKD f Natural)
  -- ^ Maximal block body size
  , sppMaxTxSize :: !(HKD f Natural)
  -- ^ Maximal transaction size
  , sppMaxBHSize :: !(HKD f Natural)
  -- ^ Maximal block header size
  , sppKeyDeposit :: !(HKD f Coin)
  -- ^ The amount of a key registration deposit
  , sppPoolDeposit :: !(HKD f Coin)
  -- ^ The amount of a pool registration deposit
  , sppEMax :: !(HKD f EpochNo)
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
  , sppProtocolVersion :: !(HKD f BT.ProtVer)
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

instance Era era => ToCBOR (ShelleyPParams Identity era) where
  toCBOR
    ShelleyPParams {..} =
      encodeListLen 18
        <> toCBOR sppMinFeeA
        <> toCBOR sppMinFeeB
        <> toCBOR sppMaxBBSize
        <> toCBOR sppMaxTxSize
        <> toCBOR sppMaxBHSize
        <> toCBOR sppKeyDeposit
        <> toCBOR sppPoolDeposit
        <> toCBOR sppEMax
        <> toCBOR sppNOpt
        <> toCBOR sppA0
        <> toCBOR sppRho
        <> toCBOR sppTau
        <> toCBOR sppD
        <> toCBOR sppExtraEntropy
        <> toCBORGroup sppProtocolVersion
        <> toCBOR sppMinUTxOValue
        <> toCBOR sppMinPoolCost

instance Era era => FromCBOR (ShelleyPParams Identity era) where
  fromCBOR = do
    decodeRecordNamed "ShelleyPParams" (const 18) $
      ShelleyPParams @Identity
        <$> fromCBOR -- sppMinFeeA         :: Integer
        <*> fromCBOR -- sppMinFeeB         :: Natural
        <*> fromCBOR -- sppMaxBBSize       :: Natural
        <*> fromCBOR -- sppMaxTxSize       :: Natural
        <*> fromCBOR -- sppMaxBHSize       :: Natural
        <*> fromCBOR -- sppKeyDeposit      :: Coin
        <*> fromCBOR -- sppPoolDeposit     :: Coin
        <*> fromCBOR -- sppEMax            :: EpochNo
        <*> fromCBOR -- sppNOpt            :: Natural
        <*> fromCBOR -- sppA0              :: NonNegativeInterval
        <*> fromCBOR -- sppRho             :: UnitInterval
        <*> fromCBOR -- sppTau             :: UnitInterval
        <*> fromCBOR -- sppD               :: UnitInterval
        <*> fromCBOR -- sppExtraEntropy    :: Nonce
        <*> fromCBORGroup -- sppProtocolVersion :: ProtVer
        <*> fromCBOR -- sppMinUTxOValue    :: Natural
        <*> fromCBOR -- sppMinPoolCost     :: Natural

instance Era era => EncCBOR (ShelleyPParams Identity era) where
  encCBOR = toPlainEncoding (eraProtVerLow @era) . toCBOR

instance Era era => DecCBOR (ShelleyPParams Identity era) where
  decCBOR = toPlainDecoder (eraProtVerLow @era) fromCBOR

instance ToJSON (ShelleyPParams Identity era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= sppMinFeeA pp
      , "minFeeB" .= sppMinFeeB pp
      , "maxBlockBodySize" .= sppMaxBBSize pp
      , "maxTxSize" .= sppMaxTxSize pp
      , "maxBlockHeaderSize" .= sppMaxBHSize pp
      , "keyDeposit" .= sppKeyDeposit pp
      , "poolDeposit" .= sppPoolDeposit pp
      , "eMax" .= sppEMax pp
      , "nOpt" .= sppNOpt pp
      , "a0" .= sppA0 pp
      , "rho" .= sppRho pp
      , "tau" .= sppTau pp
      , "decentralisationParam" .= sppD pp
      , "extraEntropy" .= sppExtraEntropy pp
      , "protocolVersion" .= sppProtocolVersion pp
      , "minUTxOValue" .= sppMinUTxOValue pp
      , "minPoolCost" .= sppMinPoolCost pp
      ]

instance FromJSON (ShelleyPParams Identity era) where
  parseJSON =
    Aeson.withObject "ShelleyPParams" $ \obj ->
      ShelleyPParams
        <$> obj .: "minFeeA"
        <*> obj .: "minFeeB"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "keyDeposit"
        <*> obj .: "poolDeposit"
        <*> obj .: "eMax"
        <*> obj .: "nOpt"
        <*> obj .: "a0"
        <*> obj .: "rho"
        <*> obj .: "tau"
        <*> obj .: "decentralisationParam"
        <*> obj .: "extraEntropy"
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
    , sppEMax = EpochNo 0
    , sppNOpt = 100
    , sppA0 = minBound
    , sppRho = minBound
    , sppTau = minBound
    , sppD = minBound
    , sppExtraEntropy = NeutralNonce
    , sppProtocolVersion = BT.ProtVer (eraProtVerLow @era) 0
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

instance (Era era, ToCBOR (PParamsUpdate era)) => ToCBOR (Update era) where
  toCBOR (Update ppUpdate e) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR e

instance
  (Era era, FromCBOR (PParamsUpdate era)) =>
  FromCBOR (Update era)
  where
  fromCBOR = decode $ RecD Update <! From <! From

data PPUpdateEnv era = PPUpdateEnv SlotNo (GenDelegs era)
  deriving (Show, Eq, Generic)

instance NoThunks (PPUpdateEnv era)

instance Era era => ToCBOR (ShelleyPParams StrictMaybe era) where
  toCBOR ppup =
    let l =
          mapMaybe
            strictMaybeToMaybe
            [ encodeMapElement 0 toCBOR =<< sppMinFeeA ppup
            , encodeMapElement 1 toCBOR =<< sppMinFeeB ppup
            , encodeMapElement 2 toCBOR =<< sppMaxBBSize ppup
            , encodeMapElement 3 toCBOR =<< sppMaxTxSize ppup
            , encodeMapElement 4 toCBOR =<< sppMaxBHSize ppup
            , encodeMapElement 5 toCBOR =<< sppKeyDeposit ppup
            , encodeMapElement 6 toCBOR =<< sppPoolDeposit ppup
            , encodeMapElement 7 toCBOR =<< sppEMax ppup
            , encodeMapElement 8 toCBOR =<< sppNOpt ppup
            , encodeMapElement 9 toCBOR =<< sppA0 ppup
            , encodeMapElement 10 toCBOR =<< sppRho ppup
            , encodeMapElement 11 toCBOR =<< sppTau ppup
            , encodeMapElement 12 toCBOR =<< sppD ppup
            , encodeMapElement 13 toCBOR =<< sppExtraEntropy ppup
            , encodeMapElement 14 toCBOR =<< sppProtocolVersion ppup
            , encodeMapElement 15 toCBOR =<< sppMinUTxOValue ppup
            , encodeMapElement 16 toCBOR =<< sppMinPoolCost ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

instance Era era => FromCBOR (ShelleyPParams StrictMaybe era) where
  fromCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> fromCBOR >>= \x -> pure (0, \up -> up {sppMinFeeA = SJust x})
          1 -> fromCBOR >>= \x -> pure (1, \up -> up {sppMinFeeB = SJust x})
          2 -> fromCBOR >>= \x -> pure (2, \up -> up {sppMaxBBSize = SJust x})
          3 -> fromCBOR >>= \x -> pure (3, \up -> up {sppMaxTxSize = SJust x})
          4 -> fromCBOR >>= \x -> pure (4, \up -> up {sppMaxBHSize = SJust x})
          5 -> fromCBOR >>= \x -> pure (5, \up -> up {sppKeyDeposit = SJust x})
          6 -> fromCBOR >>= \x -> pure (6, \up -> up {sppPoolDeposit = SJust x})
          7 -> fromCBOR >>= \x -> pure (7, \up -> up {sppEMax = SJust x})
          8 -> fromCBOR >>= \x -> pure (8, \up -> up {sppNOpt = SJust x})
          9 -> fromCBOR >>= \x -> pure (9, \up -> up {sppA0 = SJust x})
          10 -> fromCBOR >>= \x -> pure (10, \up -> up {sppRho = SJust x})
          11 -> fromCBOR >>= \x -> pure (11, \up -> up {sppTau = SJust x})
          12 -> fromCBOR >>= \x -> pure (12, \up -> up {sppD = SJust x})
          13 -> fromCBOR >>= \x -> pure (13, \up -> up {sppExtraEntropy = SJust x})
          14 -> fromCBOR >>= \x -> pure (14, \up -> up {sppProtocolVersion = SJust x})
          15 -> fromCBOR >>= \x -> pure (15, \up -> up {sppMinUTxOValue = SJust x})
          16 -> fromCBOR >>= \x -> pure (16, \up -> up {sppMinPoolCost = SJust x})
          k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) emptyShelleyPParamsUpdate (snd <$> mapParts)

instance Era era => EncCBOR (ShelleyPParams StrictMaybe era) where
  encCBOR = toPlainEncoding (eraProtVerLow @era) . toCBOR

instance Era era => DecCBOR (ShelleyPParams StrictMaybe era) where
  decCBOR = toPlainDecoder (eraProtVerLow @era) fromCBOR

-- | Update operation for protocol parameters structure @PParams@
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
  deriving (Generic)

deriving instance Eq (PParamsUpdate era) => Eq (ProposedPPUpdates era)

deriving instance NFData (PParamsUpdate era) => NFData (ProposedPPUpdates era)

deriving instance Show (PParamsUpdate era) => Show (ProposedPPUpdates era)

instance NoThunks (PParamsUpdate era) => NoThunks (ProposedPPUpdates era)

deriving instance (Era era, EncCBOR (PParamsUpdate era)) => EncCBOR (ProposedPPUpdates era)

deriving instance (Era era, DecCBOR (PParamsUpdate era)) => DecCBOR (ProposedPPUpdates era)

deriving instance (Era era, ToCBOR (PParamsUpdate era)) => ToCBOR (ProposedPPUpdates era)

deriving instance (Era era, FromCBOR (PParamsUpdate era)) => FromCBOR (ProposedPPUpdates era)

emptyPPPUpdates :: ProposedPPUpdates era
emptyPPPUpdates = ProposedPPUpdates Map.empty

updatePParams :: EraPParams era => PParams era -> PParamsUpdate era -> PParams era
updatePParams = applyPPUpdates
{-# DEPRECATED updatePParams "Use applyPPUpdates instead" #-}

pvCanFollow :: BT.ProtVer -> StrictMaybe BT.ProtVer -> Bool
pvCanFollow _ SNothing = True
pvCanFollow (BT.ProtVer m n) (SJust (BT.ProtVer m' n')) =
  (succVersion m, 0) == (Just m', n') || (m, n + 1) == (m', n')

-- ==============================================

instance ToExpr (PParamsUpdate era) => ToExpr (ProposedPPUpdates era)

instance ToExpr (ShelleyPParams StrictMaybe era)

instance ToExpr (ShelleyPParams Identity era)
