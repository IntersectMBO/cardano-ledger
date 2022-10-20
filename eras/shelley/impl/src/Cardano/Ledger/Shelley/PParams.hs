{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Shelley.PParams
  ( ShelleyPParams,
    emptyPParams,
    ShelleyPParamsHKD (..),
    PPUPState (..),
    HKD,
    HKDFunctor (..),
    PPUpdateEnv (..),
    ProposedPPUpdates (..),
    emptyPPPUpdates,
    ShelleyPParamsUpdate,
    Update (..),
    pvCanFollow,

    -- * Deprecated
    -- PParams,
    PParams',
    -- PParamsUpdate,
    updatePParams,
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
    invalidKey,
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (EraCrypto))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.PParams hiding (PParams, PParamsUpdate)
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
import Lens.Micro (lens)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- ====================================================================

-- type PParams era = ShelleyPParams era

-- {-# DEPRECATED PParams "Use `ShelleyPParams` instead" #-}

type PParams' f era = ShelleyPParamsHKD f era

{-# DEPRECATED PParams' "Use `ShelleyPParamsHKD` instead" #-}

-- type PParamsUpdate era = ShelleyPParamsUpdate era

-- {-# DEPRECATED PParamsUpdate "Use `ShelleyPParamsUpdate` instead" #-}

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
--       { _minfeeA = 0,
--         _minfeeB = 0,
--         ...
--       }
--
--   myUpdate =
--     PParamsUpdate
--       { _minfeeA = SNothing,
--         _minfeeB = SJust 42,
--         ...
--       }
-- @
data ShelleyPParamsHKD f era = ShelleyPParams
  { -- | The linear factor for the minimum fee calculation
    _minfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    _minfeeB :: !(HKD f Natural),
    -- | Maximal block body size
    _maxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    _maxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    _maxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    _keyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    _poolDeposit :: !(HKD f Coin),
    -- | epoch bound on pool retirement
    _eMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    _nOpt :: !(HKD f Natural),
    -- | Pool influence
    _a0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    _rho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    _tau :: !(HKD f UnitInterval),
    -- | Decentralization parameter
    _d :: !(HKD f UnitInterval),
    -- | Extra entropy
    _extraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    _protocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum UTxO value
    _minUTxOValue :: !(HKD f Coin),
    -- | Minimum Stake Pool Cost
    _minPoolCost :: !(HKD f Coin)
  }
  deriving (Generic)

instance NFData (ShelleyPParamsHKD Identity era)

type ShelleyPParams era = ShelleyPParamsHKD Identity era

type ShelleyPParamsUpdate era = ShelleyPParamsHKD StrictMaybe era

instance CC.Crypto c => Core.EraPParams (ShelleyEra c) where
  type PParamsHKD f (ShelleyEra c) = ShelleyPParamsHKD f (ShelleyEra c)

  emptyPParams = def
  emptyPParamsUpdate = def

  hkdMinFeeAL = lens _minfeeA $ \pp x -> pp {_minfeeA = x}
  hkdMinFeeBL = lens _minfeeB $ \pp x -> pp {_minfeeB = x}
  hkdMaxBBSizeL = lens _maxBBSize $ \pp x -> pp {_maxBBSize = x}
  hkdMaxTxSizeL = lens _maxTxSize $ \pp x -> pp {_maxTxSize = x}
  hkdMaxBHSizeL = lens _maxBHSize $ \pp x -> pp {_maxBHSize = x}
  hkdKeyDepositL = lens _keyDeposit $ \pp x -> pp {_keyDeposit = x}
  hkdPoolDepositL = lens _poolDeposit $ \pp x -> pp {_poolDeposit = x}
  hkdEMaxL = lens _eMax $ \pp x -> pp {_eMax = x}
  hkdNOptL = lens _nOpt $ \pp x -> pp {_nOpt = x}
  hkdA0L = lens _a0 $ \pp x -> pp {_a0 = x}
  hkdRhoL = lens _rho $ \pp x -> pp {_rho = x}
  hkdTauL = lens _tau $ \pp x -> pp {_tau = x}
  hkdDL = lens _d $ \pp x -> pp {_d = x}
  hkdExtraEntropyL = lens _extraEntropy $ \pp x -> pp {_extraEntropy = x}
  hkdProtocolVersionL = lens _protocolVersion $ \pp x -> pp {_protocolVersion = x}
  hkdMinUTxOValueL = lens _minUTxOValue $ \pp x -> pp {_minUTxOValue = x}
  hkdMinPoolCostL = lens _minPoolCost $ \pp x -> pp {_minPoolCost = x}

deriving instance Eq (ShelleyPParamsHKD Identity era)

deriving instance Show (ShelleyPParamsHKD Identity era)

deriving instance Ord (ShelleyPParamsHKD Identity era)

instance NoThunks (ShelleyPParamsHKD Identity era)

instance Era era => ToCBOR (ShelleyPParamsHKD Identity era) where
  toCBOR
    ShelleyPParams {..} =
      encodeListLen 18
        <> toCBOR _minfeeA
        <> toCBOR _minfeeB
        <> toCBOR _maxBBSize
        <> toCBOR _maxTxSize
        <> toCBOR _maxBHSize
        <> toCBOR _keyDeposit
        <> toCBOR _poolDeposit
        <> toCBOR _eMax
        <> toCBOR _nOpt
        <> toCBOR _a0
        <> toCBOR _rho
        <> toCBOR _tau
        <> toCBOR _d
        <> toCBOR _extraEntropy
        <> toCBORGroup _protocolVersion
        <> toCBOR _minUTxOValue
        <> toCBOR _minPoolCost

instance Era era => FromCBOR (ShelleyPParamsHKD Identity era) where
  fromCBOR = do
    decodeRecordNamed "ShelleyPParams" (const 18) $
      ShelleyPParams @Identity
        <$> fromCBOR -- _minfeeA         :: Integer
        <*> fromCBOR -- _minfeeB         :: Natural
        <*> fromCBOR -- _maxBBSize       :: Natural
        <*> fromCBOR -- _maxTxSize       :: Natural
        <*> fromCBOR -- _maxBHSize       :: Natural
        <*> fromCBOR -- _keyDeposit      :: Coin
        <*> fromCBOR -- _poolDeposit     :: Coin
        <*> fromCBOR -- _eMax            :: EpochNo
        <*> fromCBOR -- _nOpt            :: Natural
        <*> fromCBOR -- _a0              :: NonNegativeInterval
        <*> fromCBOR -- _rho             :: UnitInterval
        <*> fromCBOR -- _tau             :: UnitInterval
        <*> fromCBOR -- _d               :: UnitInterval
        <*> fromCBOR -- _extraEntropy    :: Nonce
        <*> fromCBORGroup -- _protocolVersion :: ProtVer
        <*> fromCBOR -- _minUTxOValue    :: Natural
        <*> fromCBOR -- _minPoolCost     :: Natural

instance ToJSON (ShelleyPParamsHKD Identity era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= _minfeeA pp,
        "minFeeB" .= _minfeeB pp,
        "maxBlockBodySize" .= _maxBBSize pp,
        "maxTxSize" .= _maxTxSize pp,
        "maxBlockHeaderSize" .= _maxBHSize pp,
        "keyDeposit" .= _keyDeposit pp,
        "poolDeposit" .= _poolDeposit pp,
        "eMax" .= _eMax pp,
        "nOpt" .= _nOpt pp,
        "a0" .= _a0 pp,
        "rho" .= _rho pp,
        "tau" .= _tau pp,
        "decentralisationParam" .= _d pp,
        "extraEntropy" .= _extraEntropy pp,
        "protocolVersion" .= _protocolVersion pp,
        "minUTxOValue" .= _minUTxOValue pp,
        "minPoolCost" .= _minPoolCost pp
      ]

instance FromJSON (ShelleyPParamsHKD Identity era) where
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

instance Default (ShelleyPParamsHKD Identity era) where
  def =
    ShelleyPParams
      { _minfeeA = 0,
        _minfeeB = 0,
        _maxBBSize = 0,
        _maxTxSize = 2048,
        _maxBHSize = 0,
        _keyDeposit = Coin 0,
        _poolDeposit = Coin 0,
        _eMax = EpochNo 0,
        _nOpt = 100,
        _a0 = minBound,
        _rho = minBound,
        _tau = minBound,
        _d = minBound,
        _extraEntropy = NeutralNonce,
        _protocolVersion = BT.ProtVer 0 0,
        _minUTxOValue = mempty,
        _minPoolCost = mempty
      }

-- | Update Proposal
data Update era
  = Update !(ProposedPPUpdates era) !EpochNo
  deriving (Generic)

deriving instance Eq (Core.PParamsUpdate era) => Eq (Update era)

instance NFData (Core.PParamsUpdate era) => NFData (Update era)

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

instance NFData (ShelleyPParamsHKD StrictMaybe era)

instance NoThunks (ShelleyPParamsHKD StrictMaybe era)

-- instance NoThunks (PParamsUpdate era)

instance Era era => ToCBOR (ShelleyPParamsHKD StrictMaybe era) where
  toCBOR ppup =
    let l =
          mapMaybe
            strictMaybeToMaybe
            [ encodeMapElement 0  toCBOR =<< _minfeeA ppup,
              encodeMapElement 1  toCBOR =<< _minfeeB ppup,
              encodeMapElement 2  toCBOR =<< _maxBBSize ppup,
              encodeMapElement 3  toCBOR =<< _maxTxSize ppup,
              encodeMapElement 4  toCBOR =<< _maxBHSize ppup,
              encodeMapElement 5  toCBOR =<< _keyDeposit ppup,
              encodeMapElement 6  toCBOR =<< _poolDeposit ppup,
              encodeMapElement 7  toCBOR =<< _eMax ppup,
              encodeMapElement 8  toCBOR =<< _nOpt ppup,
              encodeMapElement 9  toCBOR =<< _a0 ppup,
              encodeMapElement 10 toCBOR =<< _rho ppup,
              encodeMapElement 11 toCBOR =<< _tau ppup,
              encodeMapElement 12 toCBOR =<< _d ppup,
              encodeMapElement 13 toCBOR =<< _extraEntropy ppup,
              encodeMapElement 14 toCBOR =<< _protocolVersion ppup,
              encodeMapElement 15 toCBOR =<< _minUTxOValue ppup,
              encodeMapElement 16 toCBOR =<< _minPoolCost ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

instance Default (ShelleyPParamsHKD StrictMaybe era) where
  def =
    ShelleyPParams
      { _minfeeA = SNothing,
        _minfeeB = SNothing,
        _maxBBSize = SNothing,
        _maxTxSize = SNothing,
        _maxBHSize = SNothing,
        _keyDeposit = SNothing,
        _poolDeposit = SNothing,
        _eMax = SNothing,
        _nOpt = SNothing,
        _a0 = SNothing,
        _rho = SNothing,
        _tau = SNothing,
        _d = SNothing,
        _extraEntropy = SNothing,
        _protocolVersion = SNothing,
        _minUTxOValue = SNothing,
        _minPoolCost = SNothing
      }

instance Era era => FromCBOR (ShelleyPParamsHKD StrictMaybe era) where
  fromCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0  -> fromCBOR >>= \x -> pure (0 , \up -> up {_minfeeA = SJust x})
          1  -> fromCBOR >>= \x -> pure (1 , \up -> up {_minfeeB = SJust x})
          2  -> fromCBOR >>= \x -> pure (2 , \up -> up {_maxBBSize = SJust x})
          3  -> fromCBOR >>= \x -> pure (3 , \up -> up {_maxTxSize = SJust x})
          4  -> fromCBOR >>= \x -> pure (4 , \up -> up {_maxBHSize = SJust x})
          5  -> fromCBOR >>= \x -> pure (5 , \up -> up {_keyDeposit = SJust x})
          6  -> fromCBOR >>= \x -> pure (6 , \up -> up {_poolDeposit = SJust x})
          7  -> fromCBOR >>= \x -> pure (7 , \up -> up {_eMax = SJust x})
          8  -> fromCBOR >>= \x -> pure (8 , \up -> up {_nOpt = SJust x})
          9  -> fromCBOR >>= \x -> pure (9 , \up -> up {_a0 = SJust x})
          10 -> fromCBOR >>= \x -> pure (10, \up -> up {_rho = SJust x})
          11 -> fromCBOR >>= \x -> pure (11, \up -> up {_tau = SJust x})
          12 -> fromCBOR >>= \x -> pure (12, \up -> up {_d = SJust x})
          13 -> fromCBOR >>= \x -> pure (13, \up -> up {_extraEntropy = SJust x})
          14 -> fromCBOR >>= \x -> pure (14, \up -> up {_protocolVersion = SJust x})
          15 -> fromCBOR >>= \x -> pure (15, \up -> up {_minUTxOValue = SJust x})
          16 -> fromCBOR >>= \x -> pure (16, \up -> up {_minPoolCost = SJust x})
          k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) def (snd <$> mapParts)

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

updatePParams :: Core.EraPParams era => Core.PParams era -> Core.PParamsUpdate era -> Core.PParams era
updatePParams = applyPPUpdates

{-# DEPRECATED updatePParams "Use applyPPUpdates instead" #-}

data PPUPState era = PPUPState
  { proposals :: !(ProposedPPUpdates era),
    futureProposals :: !(ProposedPPUpdates era)
  }
  deriving (Generic)

deriving instance Show (Core.PParamsUpdate era) => Show (PPUPState era)

deriving instance Eq (Core.PParamsUpdate era) => Eq (PPUPState era)

instance NFData (Core.PParamsUpdate era) => NFData (PPUPState era)

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
