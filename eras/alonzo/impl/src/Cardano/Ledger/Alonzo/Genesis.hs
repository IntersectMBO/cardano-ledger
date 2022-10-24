{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Genesis
  ( AlonzoGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Binary
import Cardano.Crypto.Hash.Class (hashToTextAsHex)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    CostModels (..),
    ExUnits (..),
    ExUnits',
    Prices (..),
    getCostModelParams,
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (AlonzoTxOut))
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSONKey (..), ToJSONKey (..), toJSONKeyText)
import Data.Coders
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (fromRationalRepetendLimited)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import PlutusLedgerApi.Common (IsParamName, showParamName)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import PlutusPrelude (enumerate)

data AlonzoGenesis = AlonzoGenesis
  { coinsPerUTxOWord :: !Coin,
    costmdls :: !CostModels,
    prices :: !Prices,
    maxTxExUnits :: !ExUnits,
    maxBlockExUnits :: !ExUnits,
    maxValSize :: !Natural,
    collateralPercentage :: !Natural,
    maxCollateralInputs :: !Natural
  }
  deriving (Eq, Generic, NoThunks)

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  ShelleyPParams era1 ->
  AlonzoGenesis ->
  AlonzoPParams era2
extendPPWithGenesis
  pp
  AlonzoGenesis
    { coinsPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
    } =
    extendPP
      pp
      coinsPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance FromCBOR AlonzoGenesis where
  fromCBOR =
    decode $
      RecD AlonzoGenesis
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToCBOR AlonzoGenesis where
  toCBOR
    AlonzoGenesis
      { coinsPerUTxOWord,
        costmdls,
        prices,
        maxTxExUnits,
        maxBlockExUnits,
        maxValSize,
        collateralPercentage,
        maxCollateralInputs
      } =
      encode $
        Rec AlonzoGenesis
          !> To coinsPerUTxOWord
          !> To costmdls
          !> To prices
          !> To maxTxExUnits
          !> To maxBlockExUnits
          !> To maxValSize
          !> To collateralPercentage
          !> To maxCollateralInputs

deriving instance ToJSON a => ToJSON (ExUnits' a)

deriving instance FromJSON a => FromJSON (ExUnits' a)

instance ToJSON ExUnits where
  toJSON ExUnits {exUnitsMem = m, exUnitsSteps = s} =
    object
      [ "exUnitsMem" .= toJSON m,
        "exUnitsSteps" .= toJSON s
      ]

instance FromJSON ExUnits where
  parseJSON = Aeson.withObject "exUnits" $ \o -> do
    mem <- o .: "exUnitsMem"
    steps <- o .: "exUnitsSteps"
    bmem <- checkWord64Bounds mem
    bsteps <- checkWord64Bounds steps
    return $ ExUnits bmem bsteps
    where
      checkWord64Bounds n =
        if n >= fromIntegral (minBound @Word64)
          && n <= fromIntegral (maxBound @Word64)
          then pure n
          else fail ("Unit out of bounds for Word64: " <> show n)

toRationalJSON :: Rational -> Aeson.Value
toRationalJSON r =
  case fromRationalRepetendLimited 20 r of
    Right (s, Nothing) -> toJSON s
    _ -> toJSON r

instance ToJSON Prices where
  toJSON Prices {prSteps, prMem} =
    -- We cannot round-trip via NonNegativeInterval, so we go via Rational
    object
      [ "prSteps" .= toRationalJSON (BT.unboundRational prSteps),
        "prMem" .= toRationalJSON (BT.unboundRational prMem)
      ]

instance FromJSON Prices where
  parseJSON =
    Aeson.withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem <- checkBoundedRational mem
      return Prices {prSteps, prMem}
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case BT.boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s -> return s

instance ToJSON CostModel where
  toJSON = toJSON . getCostModelParams

instance ToJSON CostModels where
  toJSON = toJSON . unCostModels

languageToText :: Language -> Text
languageToText PlutusV1 = "PlutusV1"
languageToText PlutusV2 = "PlutusV2"

languageFromText :: MonadFail m => Text -> m Language
languageFromText "PlutusV1" = pure PlutusV1
languageFromText "PlutusV2" = pure PlutusV2
languageFromText lang = fail $ "Error decoding Language: " ++ show lang

instance FromJSON Language where
  parseJSON = Aeson.withText "Language" languageFromText

instance ToJSON Language where
  toJSON = Aeson.String . languageToText

instance ToJSONKey Language where
  toJSONKey = toJSONKeyText languageToText

instance FromJSONKey Language where
  fromJSONKey = Aeson.FromJSONKeyTextParser languageFromText

validateCostModel :: MonadFail m => Language -> [Integer] -> m (Language, CostModel)
validateCostModel lang cmps = case mkCostModel lang cmps of
  Left err -> fail $ show err
  Right cm -> pure (lang, cm)

cmParamValues :: Language -> Map Text Integer -> [Integer]
cmParamValues lang cmMap =
  case lang of
    PlutusV1 -> filterPreservingOrder $ paramNames (enumerate @PV1.ParamName)
    PlutusV2 -> filterPreservingOrder $ paramNames (enumerate @PV2.ParamName)
  where
    filterPreservingOrder = mapMaybe (`Map.lookup` cmMap)
    paramNames :: IsParamName a => [a] -> [Text]
    paramNames = map (Text.pack . showParamName)

instance FromJSON CostModels where
  parseJSON = Aeson.withObject "CostModels" $ \o -> do
    plutusV1 <- o .:? "PlutusV1"
    plutusV2 <- o .:? "PlutusV2"
    let plutusV1' = cmParamValues PlutusV1 <$> plutusV1
    let plutusV2' = cmParamValues PlutusV2 <$> plutusV2
    cms <-
      sequence
        [ validateCostModel lang cm
          | (lang, Just cm) <- [(PlutusV1, plutusV1'), (PlutusV2, plutusV2')]
        ]
    pure . CostModels . Map.fromList $ cms

instance FromJSON AlonzoGenesis where
  parseJSON = Aeson.withObject "Alonzo Genesis" $ \o -> do
    coinsPerUTxOWord <- o .: "lovelacePerUTxOWord"
    costmdls <- o .: "costModels"
    prices <- o .: "executionPrices"
    maxTxExUnits <- o .: "maxTxExUnits"
    maxBlockExUnits <- o .: "maxBlockExUnits"
    maxValSize <- o .: "maxValueSize"
    collateralPercentage <- o .: "collateralPercentage"
    maxCollateralInputs <- o .: "maxCollateralInputs"
    return
      AlonzoGenesis
        { coinsPerUTxOWord,
          costmdls,
          prices,
          maxTxExUnits,
          maxBlockExUnits,
          maxValSize,
          collateralPercentage,
          maxCollateralInputs
        }

instance ToJSON AlonzoGenesis where
  toJSON v =
    object
      [ "lovelacePerUTxOWord" .= coinsPerUTxOWord v,
        "costModels" .= costmdls v,
        "executionPrices" .= prices v,
        "maxTxExUnits" .= maxTxExUnits v,
        "maxBlockExUnits" .= maxBlockExUnits v,
        "maxValueSize" .= maxValSize v,
        "collateralPercentage" .= collateralPercentage v,
        "maxCollateralInputs" .= maxCollateralInputs v
      ]

instance ToJSON (AlonzoPParams era) where
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
        "minPoolCost" .= _minPoolCost pp,
        "lovelacePerUTxOWord" .= _coinsPerUTxOWord pp,
        "costmdls" .= _costmdls pp,
        "prices" .= _prices pp,
        "maxTxExUnits" .= _maxTxExUnits pp,
        "maxBlockExUnits" .= _maxBlockExUnits pp,
        "maxValSize" .= _maxValSize pp,
        "collateralPercentage" .= _collateralPercentage pp,
        "maxCollateralInputs " .= _maxCollateralInputs pp
      ]

instance FromJSON (AlonzoPParams era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      AlonzoPParams
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
        <*> obj .: "minPoolCost" .!= mempty
        <*> obj .: "lovelacePerUTxOWord"
        <*> obj .: "costmdls"
        <*> obj .: "prices"
        <*> obj .: "maxTxExUnits"
        <*> obj .: "maxBlockExUnits"
        <*> obj .: "maxValSize"
        <*> obj .: "collateralPercentage"
        <*> obj .: "maxCollateralInputs"

deriving instance ToJSON (AlonzoPParamsUpdate era)

instance
  (EraTxOut era, Show (Value era), ToJSON (Value era)) =>
  ToJSON (AlonzoTxOut era)
  where
  toJSON (AlonzoTxOut addr v dataHash) =
    object
      [ "address" .= toJSON addr,
        "value" .= toJSON v,
        "datahash" .= case BT.strictMaybeToMaybe dataHash of
          Nothing -> Aeson.Null
          Just dHash ->
            Aeson.String . hashToTextAsHex $
              extractHash dHash
      ]

deriving instance Show AlonzoGenesis
