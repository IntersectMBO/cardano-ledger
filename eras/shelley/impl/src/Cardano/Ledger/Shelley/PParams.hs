{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Shelley.PParams
  ( ShelleyPParams,
    ShelleyPParamsHKD (..),
    -- sppKeyDeposit,
    PPUPState (..),
    emptyPParams,
    HKD,
    HKDFunctor (..),
    PPUpdateEnv (..),
    ProposedPPUpdates (..),
    emptyPPPUpdates,
    ShelleyPParamsUpdate,
    emptyPParamsUpdate,
    Update (..),
    updatePParams,
    pvCanFollow,

    -- * Deprecated
    PParams,
    PParams',
    PParamsUpdate,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeWord,
  )
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    fromSMaybe,
    invalidKey,
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams (applyPPUpdates))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Coders
  ( Decode (From, RecD),
    decode,
    (<!),
  )
import Data.Default.Class (Default, def)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- ====================================================================

type PParams era = ShelleyPParams era

{-# DEPRECATED PParams "Use `ShelleyPParams` instead" #-}

type PParams' f era = ShelleyPParamsHKD f era

{-# DEPRECATED PParams' "Use `ShelleyPParamsHKD` instead" #-}

type PParamsUpdate era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsUpdate "Use `ShelleyPParamsUpdate` instead" #-}

-- | Protocol parameters.
--
-- We use the `HKD` type family so that the protocol parameters type and
-- the type for the updates to the protocol parameters can share records fields.
-- The protocol parameters will have type 'PParams'' 'Identity', and the updates
-- will have type 'PParams'' 'StrictMaybe', though 'Identity' will be hidden from use.
--
-- For example:
--
-- @
--   myParameters =
--     PParams
--       { sppMinfeeA = 0,
--         sppMinfeeB = 0,
--         ...
--       }
--
--   myUpdate =
--     PParamsUpdate
--       { sppMinfeeA = SNothing,
--         sppMinfeeB = SJust 42,
--         ...
--       }
-- @
data ShelleyPParamsHKD f era = ShelleyPParams
  { -- | The linear factor for the minimum fee calculation
    sppMinfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    sppMinfeeB :: !(HKD f Natural),
    -- | Maximal block body size
    sppMaxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    sppMaxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    sppMaxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    sppKeyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    sppPoolDeposit :: !(HKD f Coin),
    -- | epoch bound on pool retirement
    sppEMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    sppNOpt :: !(HKD f Natural),
    -- | Pool influence
    sppA0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    sppRho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    sppTau :: !(HKD f UnitInterval),
    -- | Decentralization parameter
    sppD :: !(HKD f UnitInterval),
    -- | Extra entropy
    sppExtraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    sppProtocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum UTxO value
    sppMinUTxOValue :: !(HKD f Coin),
    -- | Minimum Stake Pool Cost
    sppMinPoolCost :: !(HKD f Coin)
  }
  deriving (Generic)

type ShelleyPParams era = ShelleyPParamsHKD Identity era

type ShelleyPParamsUpdate era = ShelleyPParamsHKD StrictMaybe era

instance CC.Crypto c => EraPParams (ShelleyEra c) where
  type PParams (ShelleyEra c) = ShelleyPParams (ShelleyEra c)
  type PParamsUpdate (ShelleyEra c) = ShelleyPParamsUpdate (ShelleyEra c)

  applyPPUpdates = updatePParams

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (ShelleyPParams era)

instance (Era era) => ToCBOR (ShelleyPParams era) where
  toCBOR
    ShelleyPParams
      { sppMinfeeA = minfeeA',
        sppMinfeeB = minfeeB',
        sppMaxBBSize = maxBBSize',
        sppMaxTxSize = maxTxSize',
        sppMaxBHSize = maxBHSize',
        sppKeyDeposit = keyDeposit',
        sppPoolDeposit = poolDeposit',
        sppEMax = eMax',
        sppNOpt = nOpt',
        sppA0 = a0',
        sppRho = rho',
        sppTau = tau',
        sppD = d',
        sppExtraEntropy = extraEntropy',
        sppProtocolVersion = protocolVersion',
        sppMinUTxOValue = minUTxOValue',
        sppMinPoolCost = minPoolCost'
      } =
      encodeListLen 18
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR poolDeposit'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> toCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBORGroup protocolVersion'
        <> toCBOR minUTxOValue'
        <> toCBOR minPoolCost'

instance (Era era) => FromCBOR (ShelleyPParams era) where
  fromCBOR = do
    decodeRecordNamed "ShelleyPParams" (const 18) $
      ShelleyPParams
        <$> fromCBOR -- sppMinfeeA         :: Integer
        <*> fromCBOR -- sppMinfeeB         :: Natural
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

instance ToJSON (ShelleyPParams era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= sppMinfeeA pp,
        "minFeeB" .= sppMinfeeB pp,
        "maxBlockBodySize" .= sppMaxBBSize pp,
        "maxTxSize" .= sppMaxTxSize pp,
        "maxBlockHeaderSize" .= sppMaxBHSize pp,
        "keyDeposit" .= sppKeyDeposit pp,
        "poolDeposit" .= sppPoolDeposit pp,
        "eMax" .= sppEMax pp,
        "nOpt" .= sppNOpt pp,
        "a0" .= sppA0 pp,
        "rho" .= sppRho pp,
        "tau" .= sppTau pp,
        "decentralisationParam" .= sppD pp,
        "extraEntropy" .= sppExtraEntropy pp,
        "protocolVersion" .= sppProtocolVersion pp,
        "minUTxOValue" .= sppMinUTxOValue pp,
        "minPoolCost" .= sppMinPoolCost pp
      ]

instance FromJSON (ShelleyPParams era) where
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

instance Default (ShelleyPParams era) where
  def = emptyPParams

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: ShelleyPParams era
emptyPParams =
  ShelleyPParams
    { sppMinfeeA = 0,
      sppMinfeeB = 0,
      sppMaxBBSize = 0,
      sppMaxTxSize = 2048,
      sppMaxBHSize = 0,
      sppKeyDeposit = Coin 0,
      sppPoolDeposit = Coin 0,
      sppEMax = EpochNo 0,
      sppNOpt = 100,
      sppA0 = minBound,
      sppRho = minBound,
      sppTau = minBound,
      sppD = minBound,
      sppExtraEntropy = NeutralNonce,
      sppProtocolVersion = BT.ProtVer 0 0,
      sppMinUTxOValue = mempty,
      sppMinPoolCost = mempty
    }

-- | Update Proposal
data Update era
  = Update !(ProposedPPUpdates era) !EpochNo
  deriving (Generic)

deriving instance Eq (Core.PParamsUpdate era) => Eq (Update era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (Update era)

deriving instance Show (Core.PParamsUpdate era) => Show (Update era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (Update era)

instance (Era era, ToCBOR (Core.PParamsUpdate era)) => ToCBOR (Update era) where
  toCBOR (Update ppUpdate e) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR e

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (Update era)
  where
  fromCBOR = decode $ RecD Update <! From <! From

data PPUpdateEnv era = PPUpdateEnv SlotNo (GenDelegs era)
  deriving (Show, Eq, Generic)

instance NoThunks (PPUpdateEnv era)

deriving instance Eq (PParams' StrictMaybe era)

deriving instance Show (PParams' StrictMaybe era)

deriving instance Ord (PParams' StrictMaybe era)

deriving instance NFData (PParams' StrictMaybe era)

instance NoThunks (PParamsUpdate era)

instance (Era era) => ToCBOR (PParamsUpdate era) where
  toCBOR ppup =
    let l =
          mapMaybe
            strictMaybeToMaybe
            [ encodeMapElement 0 toCBOR =<< sppMinfeeA ppup,
              encodeMapElement 1 toCBOR =<< sppMinfeeB ppup,
              encodeMapElement 2 toCBOR =<< sppMaxBBSize ppup,
              encodeMapElement 3 toCBOR =<< sppMaxTxSize ppup,
              encodeMapElement 4 toCBOR =<< sppMaxBHSize ppup,
              encodeMapElement 5 toCBOR =<< sppKeyDeposit ppup,
              encodeMapElement 6 toCBOR =<< sppPoolDeposit ppup,
              encodeMapElement 7 toCBOR =<< sppEMax ppup,
              encodeMapElement 8 toCBOR =<< sppNOpt ppup,
              encodeMapElement 9 toCBOR =<< sppA0 ppup,
              encodeMapElement 10 toCBOR =<< sppRho ppup,
              encodeMapElement 11 toCBOR =<< sppTau ppup,
              encodeMapElement 12 toCBOR =<< sppD ppup,
              encodeMapElement 13 toCBOR =<< sppExtraEntropy ppup,
              encodeMapElement 14 toCBOR =<< sppProtocolVersion ppup,
              encodeMapElement 15 toCBOR =<< sppMinUTxOValue ppup,
              encodeMapElement 16 toCBOR =<< sppMinPoolCost ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

emptyPParamsUpdate :: PParamsUpdate era
emptyPParamsUpdate =
  ShelleyPParams
    { sppMinfeeA = SNothing,
      sppMinfeeB = SNothing,
      sppMaxBBSize = SNothing,
      sppMaxTxSize = SNothing,
      sppMaxBHSize = SNothing,
      sppKeyDeposit = SNothing,
      sppPoolDeposit = SNothing,
      sppEMax = SNothing,
      sppNOpt = SNothing,
      sppA0 = SNothing,
      sppRho = SNothing,
      sppTau = SNothing,
      sppD = SNothing,
      sppExtraEntropy = SNothing,
      sppProtocolVersion = SNothing,
      sppMinUTxOValue = SNothing,
      sppMinPoolCost = SNothing
    }

instance (Era era) => FromCBOR (PParamsUpdate era) where
  fromCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> fromCBOR >>= \x -> pure (0, \up -> up {sppMinfeeA = SJust x})
          1 -> fromCBOR >>= \x -> pure (1, \up -> up {sppMinfeeB = SJust x})
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
    pure $ foldr ($) emptyPParamsUpdate (snd <$> mapParts)

-- | Update operation for protocol parameters structure @PParams
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis (EraCrypto era)) (Core.PParamsUpdate era))
  deriving (Generic)

deriving instance Eq (Core.PParamsUpdate era) => Eq (ProposedPPUpdates era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (ProposedPPUpdates era)

deriving instance Show (Core.PParamsUpdate era) => Show (ProposedPPUpdates era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (ProposedPPUpdates era)

instance
  (Era era, ToCBOR (Core.PParamsUpdate era)) =>
  ToCBOR (ProposedPPUpdates era)
  where
  toCBOR (ProposedPPUpdates m) = mapToCBOR m

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (ProposedPPUpdates era)
  where
  fromCBOR = ProposedPPUpdates <$> mapFromCBOR

emptyPPPUpdates :: ProposedPPUpdates era
emptyPPPUpdates = ProposedPPUpdates Map.empty

updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  ShelleyPParams
    { sppMinfeeA = fromSMaybe (sppMinfeeA pp) (sppMinfeeA ppup),
      sppMinfeeB = fromSMaybe (sppMinfeeB pp) (sppMinfeeB ppup),
      sppMaxBBSize = fromSMaybe (sppMaxBBSize pp) (sppMaxBBSize ppup),
      sppMaxTxSize = fromSMaybe (sppMaxTxSize pp) (sppMaxTxSize ppup),
      sppMaxBHSize = fromSMaybe (sppMaxBHSize pp) (sppMaxBHSize ppup),
      sppKeyDeposit = fromSMaybe (sppKeyDeposit pp) (sppKeyDeposit ppup),
      sppPoolDeposit = fromSMaybe (sppPoolDeposit pp) (sppPoolDeposit ppup),
      sppEMax = fromSMaybe (sppEMax pp) (sppEMax ppup),
      sppNOpt = fromSMaybe (sppNOpt pp) (sppNOpt ppup),
      sppA0 = fromSMaybe (sppA0 pp) (sppA0 ppup),
      sppRho = fromSMaybe (sppRho pp) (sppRho ppup),
      sppTau = fromSMaybe (sppTau pp) (sppTau ppup),
      sppD = fromSMaybe (sppD pp) (sppD ppup),
      sppExtraEntropy = fromSMaybe (sppExtraEntropy pp) (sppExtraEntropy ppup),
      sppProtocolVersion = fromSMaybe (sppProtocolVersion pp) (sppProtocolVersion ppup),
      sppMinUTxOValue = fromSMaybe (sppMinUTxOValue pp) (sppMinUTxOValue ppup),
      sppMinPoolCost = fromSMaybe (sppMinPoolCost pp) (sppMinPoolCost ppup)
    }

data PPUPState era = PPUPState
  { proposals :: !(ProposedPPUpdates era),
    futureProposals :: !(ProposedPPUpdates era)
  }
  deriving (Generic)

deriving instance Show (Core.PParamsUpdate era) => Show (PPUPState era)

deriving instance Eq (Core.PParamsUpdate era) => Eq (PPUPState era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (PPUPState era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (PPUPState era)

instance (Era era, ToCBOR (Core.PParamsUpdate era)) => ToCBOR (PPUPState era) where
  toCBOR (PPUPState ppup fppup) =
    encodeListLen 2 <> toCBOR ppup <> toCBOR fppup

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (PPUPState era)
  where
  fromCBOR =
    decode $
      RecD PPUPState
        <! From
        <! From

instance Default (PPUPState era) where
  def = PPUPState emptyPPPUpdates emptyPPPUpdates

pvCanFollow :: BT.ProtVer -> StrictMaybe BT.ProtVer -> Bool
pvCanFollow _ SNothing = True
pvCanFollow (BT.ProtVer m n) (SJust (BT.ProtVer m' n')) =
  (m + 1, 0) == (m', n') || (m, n + 1) == (m', n')
