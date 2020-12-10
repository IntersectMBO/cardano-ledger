{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Alonzo.PParams
  ( PParams' (..),
    PParams,
    PPHash (..),
    emptyPParams,
    ProtVer (..),
    ProposedPPUpdates (..),
    emptyPPPUpdates,
    PParamsUpdate,
    emptyPParamsUpdate,
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
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..), Language, Prices (..))
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    interval0,
    invalidKey,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.PParams (HKD, ProtVer (..))
import Shelley.Spec.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
    ratioFromCBOR,
    ratioToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..))

-- TODO
-- make type families for PParams and PParamsUpdate
-- what is the encodeListLen ??

-- How to handle this alonzo-specific type?
type PParamsUpdate era = PParams' StrictMaybe era

-- | Protocol parameters.
-- Shelley parameters + additional ones
data PParams' f era = PParams
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
    _a0 :: !(HKD f Rational),
    -- | Monetary expansion
    _rho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    _tau :: !(HKD f UnitInterval),
    -- | Decentralization parameter
    _d :: !(HKD f UnitInterval),
    -- | Extra entropy
    _extraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    _protocolVersion :: !(HKD f ProtVer),
    -- | Minimum Stake Pool Cost
    _minPoolCost :: !(HKD f Coin),
    -- new/updated for alonzo

    -- | Cost in ada per byte of UTxO storage (instead of _minUTxOValue)
    _adaPerUTxOByte :: !(HKD f Coin),
    -- | Cost models for non-native script languages
    _costmdls :: !(HKD f (Map Language CostModel)),
    -- | Prices of execution units (for non-native script languages)
    _prices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    _maxTxExUnits :: !(HKD f ExUnits),
    -- | Max total script execution resources units allowed per block
    _maxBlockExUnits :: !(HKD f ExUnits)
  }
  deriving (Generic)

type PParams era = PParams' Identity era

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (PParams era)

instance (Era era) => ToCBOR (PParams era) where
  toCBOR
    ( PParams
        { _minfeeA = minfeeA',
          _minfeeB = minfeeB',
          _maxBBSize = maxBBSize',
          _maxTxSize = maxTxSize',
          _maxBHSize = maxBHSize',
          _keyDeposit = keyDeposit',
          _poolDeposit = poolDeposit',
          _eMax = eMax',
          _nOpt = nOpt',
          _a0 = a0',
          _rho = rho',
          _tau = tau',
          _d = d',
          _extraEntropy = extraEntropy',
          _protocolVersion = protocolVersion',
          _minPoolCost = minPoolCost',
          -- new/updated for alonzo
          _adaPerUTxOByte = adaPerUTxOByte',
          _costmdls = costmdls',
          _prices = prices',
          _maxTxExUnits = maxTxExUnits',
          _maxBlockExUnits = maxBlockExUnits'
        }
      ) =
      encodeListLen 22
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR poolDeposit'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> ratioToCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBORGroup protocolVersion'
        <> toCBOR minPoolCost'
        -- new/updated for alonzo
        <> toCBOR adaPerUTxOByte'
        <> toCBOR costmdls'
        <> toCBOR prices'
        <> toCBOR maxTxExUnits'
        <> toCBOR maxBlockExUnits'

instance (Era era) => FromCBOR (PParams era) where
  fromCBOR = do
    decodeRecordNamed "PParams" (const 22) $
      PParams
        <$> fromCBOR -- _minfeeA         :: Integer
        <*> fromCBOR -- _minfeeB         :: Natural
        <*> fromCBOR -- _maxBBSize       :: Natural
        <*> fromCBOR -- _maxTxSize       :: Natural
        <*> fromCBOR -- _maxBHSize       :: Natural
        <*> fromCBOR -- _keyDeposit      :: Coin
        <*> fromCBOR -- _poolDeposit     :: Coin
        <*> fromCBOR -- _eMax            :: EpochNo
        <*> fromCBOR -- _nOpt            :: Natural
        <*> ratioFromCBOR -- _a0         :: Rational
        <*> fromCBOR -- _rho             :: UnitInterval
        <*> fromCBOR -- _tau             :: UnitInterval
        <*> fromCBOR -- _d               :: UnitInterval
        <*> fromCBOR -- _extraEntropy    :: Nonce
        <*> fromCBORGroup -- _protocolVersion :: ProtVer
        <*> fromCBOR -- _minPoolCost     :: Natural
        -- new/updated for alonzo
        -- TODO what should all these really be?
        <*> fromCBOR -- _adaPerUTxOByte  ::
        <*> fromCBOR -- _costmdls = costmdls',
        <*> fromCBOR -- _prices = prices',
        <*> fromCBOR -- _maxTxExUnits = maxTxExUnits',
        <*> fromCBOR -- _maxBlockExUnits = maxBlockExUnits'

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams era
emptyPParams =
  PParams
    { _minfeeA = 0,
      _minfeeB = 0,
      _maxBBSize = 0,
      _maxTxSize = 2048,
      _maxBHSize = 0,
      _keyDeposit = Coin 0,
      _poolDeposit = Coin 0,
      _eMax = EpochNo 0,
      _nOpt = 100,
      _a0 = 0,
      _rho = interval0,
      _tau = interval0,
      _d = interval0,
      _extraEntropy = NeutralNonce,
      _protocolVersion = ProtVer 0 0,
      _minPoolCost = mempty,
      -- new/updated for alonzo
      _adaPerUTxOByte = Coin 0,
      _costmdls = mempty,
      _prices = Prices (Coin 0) (Coin 0),
      _maxTxExUnits = ExUnits 0 0,
      _maxBlockExUnits = ExUnits 0 0
    }

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
            [ encodeMapElement 0 toCBOR =<< _minfeeA ppup,
              encodeMapElement 1 toCBOR =<< _minfeeB ppup,
              encodeMapElement 2 toCBOR =<< _maxBBSize ppup,
              encodeMapElement 3 toCBOR =<< _maxTxSize ppup,
              encodeMapElement 4 toCBOR =<< _maxBHSize ppup,
              encodeMapElement 5 toCBOR =<< _keyDeposit ppup,
              encodeMapElement 6 toCBOR =<< _poolDeposit ppup,
              encodeMapElement 7 toCBOR =<< _eMax ppup,
              encodeMapElement 8 toCBOR =<< _nOpt ppup,
              encodeMapElement 9 ratioToCBOR =<< _a0 ppup,
              encodeMapElement 10 toCBOR =<< _rho ppup,
              encodeMapElement 11 toCBOR =<< _tau ppup,
              encodeMapElement 12 toCBOR =<< _d ppup,
              encodeMapElement 13 toCBOR =<< _extraEntropy ppup,
              encodeMapElement 14 toCBOR =<< _protocolVersion ppup,
              encodeMapElement 15 toCBOR =<< _minPoolCost ppup,
              -- new/updated for alonzo
              encodeMapElement 16 toCBOR =<< _adaPerUTxOByte ppup,
              encodeMapElement 17 toCBOR =<< _costmdls ppup,
              encodeMapElement 18 toCBOR =<< _prices ppup,
              encodeMapElement 19 toCBOR =<< _maxTxExUnits ppup,
              encodeMapElement 20 toCBOR =<< _maxBlockExUnits ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

emptyPParamsUpdate :: PParamsUpdate era
emptyPParamsUpdate =
  PParams
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
      _minPoolCost = SNothing,
      -- new/updated for alonzo
      _adaPerUTxOByte = SNothing,
      _costmdls = SNothing,
      _prices = SNothing,
      _maxTxExUnits = SNothing,
      _maxBlockExUnits = SNothing
    }

instance (Era era) => FromCBOR (PParamsUpdate era) where
  fromCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> fromCBOR >>= \x -> pure (0, \up -> up {_minfeeA = SJust x})
          1 -> fromCBOR >>= \x -> pure (1, \up -> up {_minfeeB = SJust x})
          2 -> fromCBOR >>= \x -> pure (2, \up -> up {_maxBBSize = SJust x})
          3 -> fromCBOR >>= \x -> pure (3, \up -> up {_maxTxSize = SJust x})
          4 -> fromCBOR >>= \x -> pure (4, \up -> up {_maxBHSize = SJust x})
          5 -> fromCBOR >>= \x -> pure (5, \up -> up {_keyDeposit = SJust x})
          6 -> fromCBOR >>= \x -> pure (6, \up -> up {_poolDeposit = SJust x})
          7 -> fromCBOR >>= \x -> pure (7, \up -> up {_eMax = SJust x})
          8 -> fromCBOR >>= \x -> pure (8, \up -> up {_nOpt = SJust x})
          9 -> ratioFromCBOR >>= \x -> pure (9, \up -> up {_a0 = SJust x})
          10 -> fromCBOR >>= \x -> pure (10, \up -> up {_rho = SJust x})
          11 -> fromCBOR >>= \x -> pure (11, \up -> up {_tau = SJust x})
          12 -> fromCBOR >>= \x -> pure (12, \up -> up {_d = SJust x})
          13 -> fromCBOR >>= \x -> pure (13, \up -> up {_extraEntropy = SJust x})
          14 -> fromCBOR >>= \x -> pure (14, \up -> up {_protocolVersion = SJust x})
          15 -> fromCBOR >>= \x -> pure (15, \up -> up {_minPoolCost = SJust x})
          -- new/updated for alonzo
          16 -> fromCBOR >>= \x -> pure (15, \up -> up {_adaPerUTxOByte = SJust x})
          17 -> fromCBOR >>= \x -> pure (15, \up -> up {_costmdls = SJust x})
          18 -> fromCBOR >>= \x -> pure (15, \up -> up {_prices = SJust x})
          19 -> fromCBOR >>= \x -> pure (15, \up -> up {_maxTxExUnits = SJust x})
          20 -> fromCBOR >>= \x -> pure (15, \up -> up {_maxBlockExUnits = SJust x})
          k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) emptyPParamsUpdate (snd <$> mapParts)

-- | Update operation for protocol parameters structure @PParams
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis (Crypto era)) (PParamsUpdate era))
  deriving (Show, Eq, Generic)

instance NFData (ProposedPPUpdates era)

instance NoThunks (ProposedPPUpdates era)

instance Era era => ToCBOR (ProposedPPUpdates era) where
  toCBOR (ProposedPPUpdates m) = mapToCBOR m

instance Era era => FromCBOR (ProposedPPUpdates era) where
  fromCBOR = ProposedPPUpdates <$> mapFromCBOR

emptyPPPUpdates :: ProposedPPUpdates era
emptyPPPUpdates = ProposedPPUpdates Map.empty

updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  PParams
    { _minfeeA = fromMaybe' (_minfeeA pp) (_minfeeA ppup),
      _minfeeB = fromMaybe' (_minfeeB pp) (_minfeeB ppup),
      _maxBBSize = fromMaybe' (_maxBBSize pp) (_maxBBSize ppup),
      _maxTxSize = fromMaybe' (_maxTxSize pp) (_maxTxSize ppup),
      _maxBHSize = fromMaybe' (_maxBHSize pp) (_maxBHSize ppup),
      _keyDeposit = fromMaybe' (_keyDeposit pp) (_keyDeposit ppup),
      _poolDeposit = fromMaybe' (_poolDeposit pp) (_poolDeposit ppup),
      _eMax = fromMaybe' (_eMax pp) (_eMax ppup),
      _nOpt = fromMaybe' (_nOpt pp) (_nOpt ppup),
      _a0 = fromMaybe' (_a0 pp) (_a0 ppup),
      _rho = fromMaybe' (_rho pp) (_rho ppup),
      _tau = fromMaybe' (_tau pp) (_tau ppup),
      _d = fromMaybe' (_d pp) (_d ppup),
      _extraEntropy = fromMaybe' (_extraEntropy pp) (_extraEntropy ppup),
      _protocolVersion = fromMaybe' (_protocolVersion pp) (_protocolVersion ppup),
      _minPoolCost = fromMaybe' (_minPoolCost pp) (_minPoolCost ppup),
      -- new/updated for alonzo
      _adaPerUTxOByte = fromMaybe' (_adaPerUTxOByte pp) (_adaPerUTxOByte ppup),
      _costmdls = fromMaybe' (_costmdls pp) (_costmdls ppup),
      _prices = fromMaybe' (_prices pp) (_prices ppup),
      _maxTxExUnits = fromMaybe' (_maxTxExUnits pp) (_maxTxExUnits ppup),
      _maxBlockExUnits = fromMaybe' (_maxBlockExUnits pp) (_maxBlockExUnits ppup)
    }
  where
    fromMaybe' :: a -> StrictMaybe a -> a
    fromMaybe' x = fromMaybe x . strictMaybeToMaybe

data EraIndependentPP

-- Hash of a subset of Protocol Parameters relevant to Plutus script evaluation
newtype PPHash crypto
  = PPHash
      (Hash.Hash (HASH crypto) EraIndependentPP)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance CC.Crypto crypto => FromCBOR (PPHash crypto)

deriving newtype instance CC.Crypto crypto => ToCBOR (PPHash crypto)
