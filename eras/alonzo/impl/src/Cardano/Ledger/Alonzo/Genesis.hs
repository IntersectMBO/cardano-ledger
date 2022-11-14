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
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Data.Aeson (Array, FromJSON (..), Object, ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSONKey (..), Parser, ToJSONKey (..), toJSONKeyText)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Scientific (fromRationalRepetendLimited)
import Data.Text (Text)
import Data.Vector as Vector (toList)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

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

instance FromJSON CostModels where
  parseJSON = Aeson.withObject "CostModels" $ \o -> do
    v1CostModels <- legacyParseCostModels o
    v2CostModels <- parseCostModels o
    pure $ CostModels $ Map.fromList (v1CostModels ++ v2CostModels)

-- The costmodel parameters in Alonzo Genesis are represented as a map.
-- Plutus API does no longer require the map as a parameter to `mkEvaluationContext`, but the list of Integers representing the values of the map.
-- The expectation on this list of Integers is that they are sorted in the order given by the `ParamName` enum,
-- so even though we just have to pass the list to plutus, we still need to use the names of the parameters in order to sort the list.
-- In new versions, we want to represent the costmodel parameters directly as a list, so we can avoid this reordering, hence the name of this function.
legacyParseCostModels :: Object -> Parser [(Language, CostModel)]
legacyParseCostModels o =
  do
    plutusV1 <- o .:? "PlutusV1"
    cms <- traverse (validateCostModel PlutusV1 . cmParamValues) plutusV1
    pure $ maybeToList cms
  where
    cmParamValues :: Map Text Integer -> [Integer]
    cmParamValues cmMap = mapMaybe (`Map.lookup` cmMap) plutusV1ParamNames

parseCostModels :: Object -> Parser [(Language, CostModel)]
parseCostModels o =
  do
    plutusV2 <- o .:? "PlutusV2"
    maybeCostModels <- traverse (Aeson.withArray "PlutusV2 values" parseCostModelsV2) plutusV2
    pure $ maybeToList maybeCostModels
  where
    parseCostModelsV2 :: Array -> Parser ((Language, CostModel))
    parseCostModelsV2 array = do
      paramValues <- mapM parseJSON $ Vector.toList array
      validateCostModel PlutusV2 paramValues

validateCostModel :: MonadFail m => Language -> [Integer] -> m (Language, CostModel)
validateCostModel lang cmps = case mkCostModel lang cmps of
  Left err -> fail $ show err
  Right cm -> pure (lang, cm)

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
  (EraTxOut era, ToJSON (Value era)) =>
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

-- We list the param names instead of using `enumerate PlutusLedgerApi.V1.ParamName`, because there is a difference in 6 parameter names
-- between the ones appearing alonzo genesis files and the values returned by plutus via `showParamName` on the `ParamName` enum.
-- This listed is sorted in the order given by `ParamName` enum, so we can use it to sort the costmodel param values before passing them to plutus `mkEvaluationContext`.
plutusV1ParamNames :: [Text]
plutusV1ParamNames =
  [ "addInteger-cpu-arguments-intercept",
    "addInteger-cpu-arguments-slope",
    "addInteger-memory-arguments-intercept",
    "addInteger-memory-arguments-slope",
    "appendByteString-cpu-arguments-intercept",
    "appendByteString-cpu-arguments-slope",
    "appendByteString-memory-arguments-intercept",
    "appendByteString-memory-arguments-slope",
    "appendString-cpu-arguments-intercept",
    "appendString-cpu-arguments-slope",
    "appendString-memory-arguments-intercept",
    "appendString-memory-arguments-slope",
    "bData-cpu-arguments",
    "bData-memory-arguments",
    "blake2b-cpu-arguments-intercept",
    "blake2b-cpu-arguments-slope",
    "blake2b-memory-arguments",
    "cekApplyCost-exBudgetCPU",
    "cekApplyCost-exBudgetMemory",
    "cekBuiltinCost-exBudgetCPU",
    "cekBuiltinCost-exBudgetMemory",
    "cekConstCost-exBudgetCPU",
    "cekConstCost-exBudgetMemory",
    "cekDelayCost-exBudgetCPU",
    "cekDelayCost-exBudgetMemory",
    "cekForceCost-exBudgetCPU",
    "cekForceCost-exBudgetMemory",
    "cekLamCost-exBudgetCPU",
    "cekLamCost-exBudgetMemory",
    "cekStartupCost-exBudgetCPU",
    "cekStartupCost-exBudgetMemory",
    "cekVarCost-exBudgetCPU",
    "cekVarCost-exBudgetMemory",
    "chooseData-cpu-arguments",
    "chooseData-memory-arguments",
    "chooseList-cpu-arguments",
    "chooseList-memory-arguments",
    "chooseUnit-cpu-arguments",
    "chooseUnit-memory-arguments",
    "consByteString-cpu-arguments-intercept",
    "consByteString-cpu-arguments-slope",
    "consByteString-memory-arguments-intercept",
    "consByteString-memory-arguments-slope",
    "constrData-cpu-arguments",
    "constrData-memory-arguments",
    "decodeUtf8-cpu-arguments-intercept",
    "decodeUtf8-cpu-arguments-slope",
    "decodeUtf8-memory-arguments-intercept",
    "decodeUtf8-memory-arguments-slope",
    "divideInteger-cpu-arguments-constant",
    "divideInteger-cpu-arguments-model-arguments-intercept",
    "divideInteger-cpu-arguments-model-arguments-slope",
    "divideInteger-memory-arguments-intercept",
    "divideInteger-memory-arguments-minimum",
    "divideInteger-memory-arguments-slope",
    "encodeUtf8-cpu-arguments-intercept",
    "encodeUtf8-cpu-arguments-slope",
    "encodeUtf8-memory-arguments-intercept",
    "encodeUtf8-memory-arguments-slope",
    "equalsByteString-cpu-arguments-constant",
    "equalsByteString-cpu-arguments-intercept",
    "equalsByteString-cpu-arguments-slope",
    "equalsByteString-memory-arguments",
    "equalsData-cpu-arguments-intercept",
    "equalsData-cpu-arguments-slope",
    "equalsData-memory-arguments",
    "equalsInteger-cpu-arguments-intercept",
    "equalsInteger-cpu-arguments-slope",
    "equalsInteger-memory-arguments",
    "equalsString-cpu-arguments-constant",
    "equalsString-cpu-arguments-intercept",
    "equalsString-cpu-arguments-slope",
    "equalsString-memory-arguments",
    "fstPair-cpu-arguments",
    "fstPair-memory-arguments",
    "headList-cpu-arguments",
    "headList-memory-arguments",
    "iData-cpu-arguments",
    "iData-memory-arguments",
    "ifThenElse-cpu-arguments",
    "ifThenElse-memory-arguments",
    "indexByteString-cpu-arguments",
    "indexByteString-memory-arguments",
    "lengthOfByteString-cpu-arguments",
    "lengthOfByteString-memory-arguments",
    "lessThanByteString-cpu-arguments-intercept",
    "lessThanByteString-cpu-arguments-slope",
    "lessThanByteString-memory-arguments",
    "lessThanEqualsByteString-cpu-arguments-intercept",
    "lessThanEqualsByteString-cpu-arguments-slope",
    "lessThanEqualsByteString-memory-arguments",
    "lessThanEqualsInteger-cpu-arguments-intercept",
    "lessThanEqualsInteger-cpu-arguments-slope",
    "lessThanEqualsInteger-memory-arguments",
    "lessThanInteger-cpu-arguments-intercept",
    "lessThanInteger-cpu-arguments-slope",
    "lessThanInteger-memory-arguments",
    "listData-cpu-arguments",
    "listData-memory-arguments",
    "mapData-cpu-arguments",
    "mapData-memory-arguments",
    "mkCons-cpu-arguments",
    "mkCons-memory-arguments",
    "mkNilData-cpu-arguments",
    "mkNilData-memory-arguments",
    "mkNilPairData-cpu-arguments",
    "mkNilPairData-memory-arguments",
    "mkPairData-cpu-arguments",
    "mkPairData-memory-arguments",
    "modInteger-cpu-arguments-constant",
    "modInteger-cpu-arguments-model-arguments-intercept",
    "modInteger-cpu-arguments-model-arguments-slope",
    "modInteger-memory-arguments-intercept",
    "modInteger-memory-arguments-minimum",
    "modInteger-memory-arguments-slope",
    "multiplyInteger-cpu-arguments-intercept",
    "multiplyInteger-cpu-arguments-slope",
    "multiplyInteger-memory-arguments-intercept",
    "multiplyInteger-memory-arguments-slope",
    "nullList-cpu-arguments",
    "nullList-memory-arguments",
    "quotientInteger-cpu-arguments-constant",
    "quotientInteger-cpu-arguments-model-arguments-intercept",
    "quotientInteger-cpu-arguments-model-arguments-slope",
    "quotientInteger-memory-arguments-intercept",
    "quotientInteger-memory-arguments-minimum",
    "quotientInteger-memory-arguments-slope",
    "remainderInteger-cpu-arguments-constant",
    "remainderInteger-cpu-arguments-model-arguments-intercept",
    "remainderInteger-cpu-arguments-model-arguments-slope",
    "remainderInteger-memory-arguments-intercept",
    "remainderInteger-memory-arguments-minimum",
    "remainderInteger-memory-arguments-slope",
    "sha2_256-cpu-arguments-intercept",
    "sha2_256-cpu-arguments-slope",
    "sha2_256-memory-arguments",
    "sha3_256-cpu-arguments-intercept",
    "sha3_256-cpu-arguments-slope",
    "sha3_256-memory-arguments",
    "sliceByteString-cpu-arguments-intercept",
    "sliceByteString-cpu-arguments-slope",
    "sliceByteString-memory-arguments-intercept",
    "sliceByteString-memory-arguments-slope",
    "sndPair-cpu-arguments",
    "sndPair-memory-arguments",
    "subtractInteger-cpu-arguments-intercept",
    "subtractInteger-cpu-arguments-slope",
    "subtractInteger-memory-arguments-intercept",
    "subtractInteger-memory-arguments-slope",
    "tailList-cpu-arguments",
    "tailList-memory-arguments",
    "trace-cpu-arguments",
    "trace-memory-arguments",
    "unBData-cpu-arguments",
    "unBData-memory-arguments",
    "unConstrData-cpu-arguments",
    "unConstrData-memory-arguments",
    "unIData-cpu-arguments",
    "unIData-memory-arguments",
    "unListData-cpu-arguments",
    "unListData-memory-arguments",
    "unMapData-cpu-arguments",
    "unMapData-memory-arguments",
    "verifySignature-cpu-arguments-intercept",
    "verifySignature-cpu-arguments-slope",
    "verifySignature-memory-arguments"
  ]
