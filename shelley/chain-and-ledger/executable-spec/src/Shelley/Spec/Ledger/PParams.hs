{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains just the type of protocol parameters.
module Shelley.Spec.Ledger.PParams
  ( PParams' (..),
    PParams,
    emptyPParams,
    ProtVer (..),
    PPUpdateEnv (..),
    ProposedPPUpdates (..),
    emptyPPPUpdates,
    PParamsUpdate,
    emptyPParamsUpdate,
    Update (..),
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
    enforceSize,
  )
import Cardano.Prelude (NoUnexpectedThunks (..), mapMaybe)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.=) , (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Foldable (fold)
import qualified Data.Aeson.Types as Aeson
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
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
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    mapFromCBOR,
    mapToCBOR,
    rationalFromCBOR,
    rationalToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- | Protocol parameters.
--
-- We use the HKD type family so that the protocol parameters type and
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
data PParams' f = PParams
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
    -- | Minimum UTxO value
    _minUTxOValue :: !(HKD f Natural)
  }
  deriving (Generic)

type PParams = PParams' Identity

deriving instance Eq (PParams' Identity)

deriving instance Show (PParams' Identity)

data ProtVer = ProtVer !Natural !Natural
  deriving (Show, Eq, Generic, Ord)
  deriving (ToCBOR) via (CBORGroup ProtVer)
  deriving (FromCBOR) via (CBORGroup ProtVer)

instance NoUnexpectedThunks ProtVer

instance ToJSON ProtVer where
  toJSON (ProtVer major minor) =
    Aeson.object
      [ "major" .= major
      , "minor" .= minor
      ]

instance FromJSON ProtVer where
  parseJSON =
    Aeson.withObject "ProtVer" $ \ obj ->
      ProtVer
        <$> obj .: "major"
        <*> obj .: "minor"

instance ToCBORGroup ProtVer where
  toCBORGroup (ProtVer x y) = toCBOR x <> toCBOR y
  encodedGroupSizeExpr size proxy =
    encodedSizeExpr size ((\(ProtVer x _) -> toWord x) <$> proxy)
      + encodedSizeExpr size ((\(ProtVer _ y) -> toWord y) <$> proxy)
    where
      toWord :: Natural -> Word
      toWord = fromIntegral

  listLen _ = 2
  listLenBound _ = 2

instance FromCBORGroup ProtVer where
  fromCBORGroup = do
    x <- fromCBOR
    y <- fromCBOR
    pure $ ProtVer x y

instance NoUnexpectedThunks PParams

instance ToCBOR PParams where
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
          _minUTxOValue = minUTxOValue'
        }
      ) =
      encodeListLen 17
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR poolDeposit'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> rationalToCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBORGroup protocolVersion'
        <> toCBOR minUTxOValue'

instance FromCBOR PParams where
  fromCBOR = do
    enforceSize "PParams" 17
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
      <*> rationalFromCBOR -- _a0              :: Rational
      <*> fromCBOR -- _rho             :: UnitInterval
      <*> fromCBOR -- _tau             :: UnitInterval
      <*> fromCBOR -- _d               :: UnitInterval
      <*> fromCBOR -- _extraEntropy    :: Nonce
      <*> fromCBORGroup -- _protocolVersion :: ProtVer
      <*> fromCBOR -- _minUTxOValue    :: Natural


instance ToJSON PParams where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= _minfeeA pp
      , "minFeeB" .= _minfeeB pp
      , "maxBlockBodySize" .= _maxBBSize pp
      , "maxTxSize" .= _maxTxSize pp
      , "maxBlockHeaderSize" .= _maxBHSize pp
      , "keyDeposit" .= _keyDeposit pp
      , "poolDeposit" .= _poolDeposit pp
      , "eMax" .= _eMax pp
      , "nOpt" .= _nOpt pp
      , "a0" .= (fromRational $ _a0 pp :: Double)
      , "rho" .= _rho pp
      , "tau" .= _tau pp
      , "decentralisationParam" .= _d pp
      , "extraEntropy" .= _extraEntropy pp
      , "protocolVersion" .= _protocolVersion pp
      , "minUTxOValue" .= _minUTxOValue pp
      ]

instance FromJSON PParams where
  parseJSON =
      Aeson.withObject "PParams" $ \ obj ->
        PParams
          <$> obj .: "minFeeA"
          <*> obj .: "minFeeB"
          <*> obj .: "maxBlockBodySize"
          <*> obj .: "maxTxSize"
          <*> obj .: "maxBlockHeaderSize"
          <*> obj .: "keyDeposit"
          <*> obj .: "poolDeposit"
          <*> obj .: "eMax"
          <*> obj .: "nOpt"
          <*> parseRationalFromDouble (obj .: "a0")
          <*> obj .: "rho"
          <*> obj .: "tau"
          <*> obj .: "decentralisationParam"
          <*> obj .: "extraEntropy"
          <*> obj .: "protocolVersion"
          <*> obj .:? "minUTxOValue" .!= 0
      where
        parseRationalFromDouble :: Aeson.Parser Double -> Aeson.Parser Rational
        parseRationalFromDouble p = realToFrac <$> p

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams
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
      _minUTxOValue = 0
    }

-- | Update Proposal
data Update crypto
  = Update !(ProposedPPUpdates crypto) !EpochNo
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (Update crypto)

instance Crypto crypto => ToCBOR (Update crypto) where
  toCBOR (Update ppUpdate e) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR e

instance Crypto crypto => FromCBOR (Update crypto) where
  fromCBOR =
    Update <$ enforceSize "Update" 2
      <*> fromCBOR
      <*> fromCBOR

data PPUpdateEnv crypto = PPUpdateEnv SlotNo (GenDelegs crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PPUpdateEnv crypto)

type PParamsUpdate = PParams' StrictMaybe

deriving instance Eq (PParams' StrictMaybe)

deriving instance Show (PParams' StrictMaybe)

deriving instance Ord (PParams' StrictMaybe)

instance NoUnexpectedThunks PParamsUpdate

instance ToCBOR PParamsUpdate where
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
              encodeMapElement 9 rationalToCBOR =<< _a0 ppup,
              encodeMapElement 10 toCBOR =<< _rho ppup,
              encodeMapElement 11 toCBOR =<< _tau ppup,
              encodeMapElement 12 toCBOR =<< _d ppup,
              encodeMapElement 13 toCBOR =<< _extraEntropy ppup,
              encodeMapElement 14 toCBOR =<< _protocolVersion ppup,
              encodeMapElement 15 toCBOR =<< _minUTxOValue ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

emptyPParamsUpdate :: PParamsUpdate
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
      _minUTxOValue = SNothing
    }

instance FromCBOR PParamsUpdate where
  fromCBOR = do
    mapParts <- decodeMapContents $
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
        9 -> rationalFromCBOR >>= \x -> pure (9, \up -> up {_a0 = SJust x})
        10 -> fromCBOR >>= \x -> pure (10, \up -> up {_rho = SJust x})
        11 -> fromCBOR >>= \x -> pure (11, \up -> up {_tau = SJust x})
        12 -> fromCBOR >>= \x -> pure (12, \up -> up {_d = SJust x})
        13 -> fromCBOR >>= \x -> pure (13, \up -> up {_extraEntropy = SJust x})
        14 -> fromCBOR >>= \x -> pure (14, \up -> up {_protocolVersion = SJust x})
        15 -> fromCBOR >>= \x -> pure (15, \up -> up {_minUTxOValue = SJust x})
        k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) emptyPParamsUpdate (snd <$> mapParts)

-- | Update operation for protocol parameters structure @PParams
newtype ProposedPPUpdates crypto
  = ProposedPPUpdates (Map (KeyHash 'Genesis crypto) PParamsUpdate)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (ProposedPPUpdates crypto)

instance Crypto crypto => ToCBOR (ProposedPPUpdates crypto) where
  toCBOR (ProposedPPUpdates m) = mapToCBOR m

instance Crypto crypto => FromCBOR (ProposedPPUpdates crypto) where
  fromCBOR = ProposedPPUpdates <$> mapFromCBOR

emptyPPPUpdates :: ProposedPPUpdates crypto
emptyPPPUpdates = ProposedPPUpdates Map.empty

updatePParams :: PParams -> PParamsUpdate -> PParams
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
      _minUTxOValue = fromMaybe' (_minUTxOValue pp) (_minUTxOValue ppup)
    }
  where
    fromMaybe' :: a -> StrictMaybe a -> a
    fromMaybe' x = fromMaybe x . strictMaybeToMaybe
