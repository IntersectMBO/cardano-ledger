{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Plutus.CostModels (
  -- * Cost Model
  CostModel,
  CostModelError (..),
  decodeValidAndUnknownCostModels,
  emptyCostModels,
  updateCostModels,
  mkCostModel,
  mkCostModelsLenient,
  encodeCostModel,
  getCostModelLanguage,
  getCostModelParams,
  getEvaluationContext,
  costModelParamNames,
  costModelToMap,
  costModelFromMap,
  decodeCostModelFailHard,
  CostModels (..),
  PV1.CostModelApplyError (..),
)
where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  Decoder,
  EncCBOR (encCBOR),
  Encoding,
  decodeMapByKey,
  encodeFoldableAsDefLenList,
  ifDecoderVersionAtLeast,
 )
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Plutus.Language (Language (..), mkLanguageEnum, nonNativeLanguages)
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (..))
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Monad (forM, when)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Aeson (
  FromJSON (..),
  Object,
  ToJSON (..),
  Value (Array, Object),
  withObject,
  (.:?),
 )
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text as T (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), allNoThunks)
import PlutusLedgerApi.Common (CostModelApplyWarn, showParamName)
import qualified PlutusLedgerApi.V1 as PV1 (
  CostModelApplyError (..),
  EvaluationContext,
  ParamName,
  mkEvaluationContext,
 )
import qualified PlutusLedgerApi.V2 as PV2 (ParamName, mkEvaluationContext)
import qualified PlutusLedgerApi.V3 as PV3 (ParamName, mkEvaluationContext)

-- | A language dependent cost model for the Plutus evaluator.
-- Note that the `PV1.EvaluationContext` is entirely dependent on the
-- cost model parameters (ie the `Map` `Text` `Integer`) and that
-- this type uses the smart constructor `mkCostModel`
-- to hide the evaluation context.
data CostModel = CostModel
  { cmLanguage :: !Language
  , cmValues :: ![Integer]
  , cmEvalCtx :: !PV1.EvaluationContext
  }
  deriving (Generic)

-- | Note that this Eq instance ignores the evaluation context, which is
-- entirely dependent on the cost model parameters and is guarded by the
-- smart constructor `mkCostModel`.
instance Eq CostModel where
  CostModel l1 x _ == CostModel l2 y _ = l1 == l2 && x == y

instance Show CostModel where
  show (CostModel lang cm _) = "CostModel " <> show lang <> " " <> show cm

-- | Note that this Ord instance ignores the evaluation context, which is
-- entirely dependent on the cost model parameters and is guarded by the
-- smart constructor `mkCostModel`.
instance Ord CostModel where
  compare (CostModel l1 x _) (CostModel l2 y _) = compare l1 l2 <> compare x y

instance NoThunks CostModel

instance NFData CostModel where
  rnf (CostModel lang cm ectx) = lang `deepseq` cm `deepseq` rnf ectx

instance FromJSON CostModels where
  parseJSON = withObject "CostModels" $ \o -> do
    cms <- mapM (parseCostModels o) nonNativeLanguages
    let cmsMap = Map.fromList [(cmLanguage cm, cm) | Just cm <- cms]
    pure $ CostModels cmsMap mempty mempty

-- | The costmodel parameters in Alonzo Genesis are represented as a map.  Plutus API does
-- no longer require the map as a parameter to `mkEvaluationContext`, but the list of
-- Integers representing the values of the map.  The expectation on this list of Integers
-- is that they are sorted in the order given by the `ParamName` enum, so even though we
-- just have to pass the list to plutus, we still need to use the names of the parameters
-- in order to sort the list.  In new versions, we want to represent the costmodel
-- parameters directly as a list, so we can avoid this reordering.
parseCostModels :: Object -> Language -> Parser (Maybe CostModel)
parseCostModels o lang = do
  plutusCostModelValueMaybe <- o .:? fromString (show lang)
  forM plutusCostModelValueMaybe $ \plutusCostModelValue ->
    case plutusCostModelValue of
      Object _ -> costModelFromMap lang =<< parseJSON plutusCostModelValue
      Array _ -> validateCostModel lang =<< parseJSON plutusCostModelValue
      _ -> fail $ "Expected either an Array or an Object, but got: " ++ show plutusCostModelValue

costModelFromMap :: MonadFail m => Language -> Map Text Integer -> m CostModel
costModelFromMap lang cmMap =
  mapM lookupFail paramNames >>= validateCostModel lang
  where
    paramNames = costModelParamNames lang
    lookupFail paramName =
      case Map.lookup paramName cmMap of
        Nothing -> fail $ "Unrecognized cost model parameter name: " ++ show paramName
        Just v -> pure v

costModelToMap :: CostModel -> Map Text Integer
costModelToMap cm =
  Map.fromList $ zip (costModelParamNames (cmLanguage cm)) (cmValues cm)

costModelParamNames :: Language -> [Text]
costModelParamNames PlutusV1 = plutusV1ParamNames
costModelParamNames lang = plutusVXParamNames lang

-- | There is a difference in 6 parameter names between the ones appearing alonzo genesis
-- files and the values returned by plutus via `showParamName` on the `ParamName` enum.
-- This listed is sorted in the order given by `ParamName` enum, so we can use it to sort
-- the costmodel param values before passing them to plutus `mkEvaluationContext`.
plutusV1ParamNames :: [Text]
plutusV1ParamNames =
  map (\newName -> Map.findWithDefault newName newName oldNewMapping) newNames
  where
    newNames = plutusVXParamNames PlutusV1
    oldNewMapping =
      Map.fromList
        [ ("blake2b_256-cpu-arguments-intercept", "blake2b-cpu-arguments-intercept")
        , ("blake2b_256-cpu-arguments-slope", "blake2b-cpu-arguments-slope")
        , ("blake2b_256-memory-arguments", "blake2b-memory-arguments")
        , ("verifyEd25519Signature-cpu-arguments-intercept", "verifySignature-cpu-arguments-intercept")
        , ("verifyEd25519Signature-cpu-arguments-slope", "verifySignature-cpu-arguments-slope")
        , ("verifyEd25519Signature-memory-arguments", "verifySignature-memory-arguments")
        ]

plutusVXParamNames :: Language -> [Text]
plutusVXParamNames PlutusV1 = showParamName <$> [minBound .. maxBound :: PV1.ParamName]
plutusVXParamNames PlutusV2 = showParamName <$> [minBound .. maxBound :: PV2.ParamName]
plutusVXParamNames PlutusV3 = showParamName <$> [minBound .. maxBound :: PV3.ParamName]

validateCostModel :: MonadFail m => Language -> [Integer] -> m CostModel
validateCostModel lang cmps =
  case mkCostModel lang cmps of
    Left err -> fail $ show err
    Right cm -> pure cm

-- | Convert cost model parameters to a cost model, making use of the
--  conversion function mkEvaluationContext from the Plutus API.
mkCostModel :: Language -> [Integer] -> Either PV1.CostModelApplyError CostModel
mkCostModel lang cm =
  case eCostModel of
    Right (evalCtx, _) -> Right (CostModel lang cm evalCtx)
    Left e -> Left e
  where
    mkEvaluationContext =
      case lang of
        PlutusV1 -> PV1.mkEvaluationContext
        PlutusV2 -> PV2.mkEvaluationContext
        PlutusV3 -> PV3.mkEvaluationContext
    eCostModel :: Either PV1.CostModelApplyError (PV1.EvaluationContext, [CostModelApplyWarn])
    eCostModel = runWriterT (mkEvaluationContext cm)

getCostModelLanguage :: CostModel -> Language
getCostModelLanguage (CostModel lang _ _) = lang

getCostModelParams :: CostModel -> [Integer]
getCostModelParams (CostModel _ cm _) = cm

decodeCostModelsCollectingErrors :: Decoder s CostModels
decodeCostModelsCollectingErrors = mkCostModelsLenient <$> decCBOR
{-# INLINE decodeCostModelsCollectingErrors #-}

decodeCostModelsFailingOnError :: Decoder s CostModels
decodeCostModelsFailingOnError =
  CostModels <$> decodeMapByKey decCBOR legacyDecodeCostModel <*> pure mempty <*> pure mempty
{-# INLINE decodeCostModelsFailingOnError #-}

decodeValidAndUnknownCostModels :: Decoder s CostModels
decodeValidAndUnknownCostModels = do
  validAndUnkonwnCms <- decodeMapByKey decCBOR decodeValidOrUnknownCm
  pure $ Map.foldrWithKey addValidOrUnknownCm emptyCostModels validAndUnkonwnCms
  where
    decodeValidOrUnknownCm :: Word8 -> Decoder s (Either [Integer] CostModel)
    decodeValidOrUnknownCm langW8 = do
      case mkLanguageEnum (fromIntegral langW8) of
        Just lang -> Right <$> decodeCostModelFailHard lang
        Nothing -> Left <$> decCBOR

    addValidOrUnknownCm :: Word8 -> Either [Integer] CostModel -> CostModels -> CostModels
    addValidOrUnknownCm langW8 unknownOrCm (CostModels validCms errors invalidCms) =
      case unknownOrCm of
        Left cmIds -> CostModels validCms errors (Map.insert langW8 cmIds invalidCms)
        Right cm -> CostModels (Map.insert (cmLanguage cm) cm validCms) errors invalidCms

decodeCostModels :: Decoder s CostModels
decodeCostModels =
  ifDecoderVersionAtLeast
    (natVersion @9)
    decodeCostModelsCollectingErrors
    decodeCostModelsFailingOnError
{-# INLINEABLE decodeCostModels #-}

-- | Prior to version 9, each 'CostModel' was expected to be serialized as
-- an array of integers of a specific length (depending on the version of Plutus).
-- Starting in version 9, we allow the decoders to accept lists longer than what they
-- require, so that new fields can be added in the future.
-- For this reason, we must hard code the length expectation into the deserializers
-- prior to version 9.
--
-- Note that the number of elements in the V1 and V2 cost models
-- may change in the future, they are only fixed prior to version 9.
--
-- See https://github.com/input-output-hk/cardano-ledger/issues/2902
-- and https://github.com/input-output-hk/cardano-ledger/blob/master/docs/adr/2022-12-05_006-cost-model-serialization.md
legacyCostModelLength :: Language -> Int
legacyCostModelLength PlutusV1 = 166
legacyCostModelLength PlutusV2 = 175
legacyCostModelLength PlutusV3 = 223

-- | See the note for 'legacyCostModelLength'.
legacyDecodeCostModel :: Language -> Decoder s CostModel
legacyDecodeCostModel lang = do
  values <- decCBOR
  let numValues = length values
      expectedNumValues = legacyCostModelLength lang
  when (numValues /= expectedNumValues) $
    fail $
      "Expected array with "
        <> show expectedNumValues
        <> " entries, but encoded array has "
        <> show numValues
        <> " entries."
  case mkCostModel lang values of
    Left e -> fail $ show e
    Right cm -> pure cm
{-# INLINEABLE legacyDecodeCostModel #-}

decodeCostModelFailHard :: Language -> Decoder s CostModel
decodeCostModelFailHard lang = do
  checked <- mkCostModel lang <$> decCBOR
  case checked of
    Left e -> fail $ show e
    Right cm -> pure cm
{-# INLINEABLE decodeCostModelFailHard #-}

getEvaluationContext :: CostModel -> PV1.EvaluationContext
getEvaluationContext (CostModel _ _ ec) = ec

-- | See 'CostModels' for an explanation of how 'CostModelError' is used.
newtype CostModelError = CostModelError PV1.CostModelApplyError
  deriving stock (Show, Generic)

instance Eq CostModelError where
  CostModelError x1 == CostModelError x2 = show x1 == show x2

instance Ord CostModelError where
  CostModelError x1 <= CostModelError x2 = show x1 <= show x2

instance ToExpr CostModelError where
  toExpr (CostModelError x1) = toExpr (show x1)

instance NoThunks PV1.CostModelApplyError where
  showTypeOf _ = "CostModelApplyError"
  wNoThunks _ctxt _error = allNoThunks []

instance NoThunks CostModelError

instance NFData PV1.CostModelApplyError where
  rnf = rwhnf

instance NFData CostModelError

-- | For a known version of Plutus, attempting to construct a cost model with
-- too few parameters (depending on the version) will result in an error.
-- 'CostModelApplyError' exists to collect these errors in the 'CostModels' type.
-- The 'CostModels' type itself needs to be flexible enough to accept any map
-- of 'Word8' to '[Integer]', so that cost models can be placed in the protocol parameters
-- ahead of changes to the Plutus evaluation context. In this way, serializing a cost model,
-- updating software, and deserializing can result in errors going away.
--
-- Additionally, 'CostModels' needs to be able to store cost models for future version
-- of Plutus, which we cannot yet even validate. These are stored in
-- 'costModelsUnknown`.
data CostModels = CostModels
  { costModelsValid :: !(Map Language CostModel)
  , costModelsErrors :: !(Map Language CostModelError)
  , costModelsUnknown :: !(Map Word8 [Integer])
  }
  deriving stock (Eq, Show, Ord, Generic)

emptyCostModels :: CostModels
emptyCostModels = CostModels mempty mempty mempty

-- | Updates the first @CostModels@ with the second one so that only the
-- cost models that are present in the second one get updated while all the
-- others stay unchanged
updateCostModels :: CostModels -> CostModels -> CostModels
updateCostModels
  (CostModels oldValid oldErrors oldUnk)
  (CostModels newValid newErrors newUnk) =
    CostModels
      (Map.union newValid oldValid)
      (Map.union newErrors oldErrors)
      (Map.union newUnk oldUnk)

-- | This function attempts to add a new cost model to a given 'CostModels'.
-- If it is a valid cost model for a known version of Plutus, it is added to
-- 'validCostModels'. If it is an invalid cost model for a known version of Plutus,
-- the error is sorted in 'costModelErrors' and the cost model is stored in
-- 'invalidCostModels'. Lastly, if the Plutus version is unknown,
-- the cost model is also stored in 'invalidCostModels'.
addRawCostModel :: Word8 -> [Integer] -> CostModels -> CostModels
addRawCostModel langW8 cmIds (CostModels validCMs errs invalidCMs) =
  case mkLanguageEnum (fromIntegral langW8) of
    Just lang ->
      case mkCostModel lang cmIds of
        Right cm -> CostModels (Map.insert lang cm validCMs) errs invalidCMs
        Left e -> CostModels validCMs (addError lang e errs) updatedInvalidCMs
    Nothing -> CostModels validCMs errs updatedInvalidCMs
  where
    updatedInvalidCMs = Map.insert langW8 cmIds invalidCMs
    addError l e = Map.insert l (CostModelError e)

mkCostModelsLenient :: Map Word8 [Integer] -> CostModels
mkCostModelsLenient = Map.foldrWithKey addRawCostModel (CostModels mempty mempty mempty)

-- | Turn a 'CostModels' into a mapping of potential language versions and
-- cost model values, with no distinction between valid and invalid cost models.
-- This is used for serialization, so that judgements about validity can be made
-- upon deserialization.
flattenCostModel :: CostModels -> Map Word8 [Integer]
flattenCostModel (CostModels validCMs _ invalidCMs) =
  Map.foldrWithKey (\lang cm -> Map.insert (toWord8 lang) (cmValues cm)) invalidCMs validCMs
  where
    toWord8 lang =
      case fromEnum lang of
        li
          | 0 <= li && li <= fromIntegral (maxBound :: Word8) -> fromIntegral li
          | otherwise ->
              -- This should be impossible while we have under 256 versions of Plutus
              error $ "Overflow encountered during conversion of the language: " ++ show lang

instance NoThunks CostModels

instance NFData CostModels

instance DecCBOR CostModels where
  decCBOR = decodeCostModels
  {-# INLINE decCBOR #-}

instance EncCBOR CostModels where
  encCBOR = encCBOR . flattenCostModel

instance ToJSON CostModel where
  toJSON = toJSON . getCostModelParams

instance ToJSON CostModels where
  toJSON = toJSON . costModelsValid

-- | Encoding for the `CostModel`. Important to note that it differs from `Encoding` used
-- by `Cardano.Ledger.Alonzo.PParams.getLanguageView`
encodeCostModel :: CostModel -> Encoding
encodeCostModel = encodeFoldableAsDefLenList encCBOR . getCostModelParams

instance ToExpr CostModel where
  toExpr (CostModel lang cmmap _) =
    App "CostModel" [toExpr lang, toExpr cmmap, App "PV1.EvaluationContext" []]

instance ToExpr CostModels
