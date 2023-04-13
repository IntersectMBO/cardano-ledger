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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Scripts (
  Tag (..),
  BinaryPlutus (..),
  AlonzoScript (TimelockScript, PlutusScript),
  Script,
  txscriptfee,
  isPlutusScript,
  pointWiseExUnits,
  zipSemiExUnits,
  validScript,
  transProtocolVersion,
  eqAlonzoScriptRaw,

  -- * Cost Model
  CostModel,
  CostModelError (..),
  emptyCostModels,
  mkCostModel,
  mkCostModelsLenient,
  encodeCostModel,
  getCostModelLanguage,
  getCostModelParams,
  getEvaluationContext,
  costModelParamNames,
  costModelToMap,
  costModelFromMap,
  ExUnits (ExUnits, exUnitsMem, exUnitsSteps, ..),
  ExUnits',
  Prices (..),
  decodeCostModelFailHard,
  CostModels (..),
  PV1.CostModelApplyError (..),
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock, eqTimelockRaw)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.BaseTypes (
  BoundedRational (unboundRational),
  NonNegativeInterval,
  ProtVer (..),
  boundRational,
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  Encoding,
  ToCBOR (toCBOR),
  cborError,
  decodeMapByKey,
  encodeFoldableAsDefLenList,
  getVersion64,
  ifDecoderVersionAtLeast,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (Ann, D, From, Invalid, RecD, SumD, Summands),
  Encode (Rec, Sum, To),
  Wrapped (Open),
  decode,
  encode,
  (!>),
  (<!),
  (<*!),
 )
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Language (mkLanguageEnum)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (..), defaultExprViaShow)
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Monad (forM, when)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Aeson (
  FromJSON (..),
  Object,
  ToJSON (..),
  Value (Array, Object, String),
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson (Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.DerivingVia (InstantiatedAt (..))
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Measure (BoundedMeasure, Measure)
import Data.Scientific (fromRationalRepetendLimited)
import Data.Semigroup (All (..))
import Data.Text as T (Text, pack)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), allNoThunks)
import Numeric.Natural (Natural)
import PlutusCore.Evaluation.Machine.CostModelInterface (CostModelApplyWarn)
import qualified PlutusLedgerApi.Common as Plutus (showParamName)
import qualified PlutusLedgerApi.V1 as PV1 (
  CostModelApplyError (..),
  EvaluationContext,
  ParamName,
  ProtocolVersion (ProtocolVersion),
  ScriptDecodeError,
  assertScriptWellFormed,
  mkEvaluationContext,
 )
import qualified PlutusLedgerApi.V2 as PV2 (ParamName, assertScriptWellFormed, mkEvaluationContext)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawal from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

instance NoThunks Tag

instance NFData Tag where
  rnf = rwhnf

-- =======================================================

-- | Binary representation of a Plutus script.
newtype BinaryPlutus = BinaryPlutus {unBinaryPlutus :: ShortByteString}
  deriving stock (Eq, Show)
  deriving newtype (EncCBOR, DecCBOR, NFData)

instance DecCBOR (Annotator BinaryPlutus) where
  decCBOR = pure <$> decCBOR

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data AlonzoScript era
  = TimelockScript !(Timelock era)
  | PlutusScript !Language !ShortByteString
  deriving (Eq, Generic, NoThunks)

instance (EraScript era, Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript v _) = "PlutusScript " ++ show v ++ " " ++ show (hashScript @era s)

instance NFData (AlonzoScript era)

-- | Both constructors know their original bytes
instance SafeToHash (AlonzoScript era) where
  originalBytes (TimelockScript t) = originalBytes t
  originalBytes (PlutusScript _ bs) = fromShort bs

type instance SomeScript 'PhaseOne (AlonzoEra c) = Timelock (AlonzoEra c)

type instance SomeScript 'PhaseTwo (AlonzoEra c) = (Language, ShortByteString)

isPlutusScript :: AlonzoScript era -> Bool
isPlutusScript (PlutusScript _ _) = True
isPlutusScript (TimelockScript _) = False

instance Crypto c => EraScript (AlonzoEra c) where
  type Script (AlonzoEra c) = AlonzoScript (AlonzoEra c)
  phaseScript PhaseOneRep (TimelockScript s) = Just (Phase1Script s)
  phaseScript PhaseTwoRep (PlutusScript lang bytes) = Just (Phase2Script lang bytes)
  phaseScript _ _ = Nothing
  scriptPrefixTag script =
    case script of
      TimelockScript _ -> nativeMultiSigTag -- "\x00"
      PlutusScript PlutusV1 _ -> "\x01"
      PlutusScript PlutusV2 _ -> "\x02"

instance Era era => ToJSON (AlonzoScript era) where
  toJSON = String . serializeAsHexText

-- ===========================================

-- | Arbitrary execution unit in which we measure the cost of scripts in terms
-- of space in memory and execution time.
--
-- The ledger itself uses 'ExUnits' Natural' exclusively, but the flexibility here
-- allows the consensus layer to translate the execution units into something
-- equivalent to 'ExUnits (Inf Natural)'. This is needed in order to provide
-- a 'BoundedMeasure' instance, which itself is needed for the alonzo instance of
-- 'TxLimits' (in consensus).
data ExUnits' a = ExUnits'
  { exUnitsMem' :: !a
  , exUnitsSteps' :: !a
  }
  deriving (Eq, Generic, Show, Functor)
  -- It is deliberate that there is no Ord instance, use `pointWiseExUnits` instead.
  deriving
    (Measure, BoundedMeasure)
    via (InstantiatedAt Generic (ExUnits' a))
  deriving
    (Monoid, Semigroup)
    via (InstantiatedAt Measure (ExUnits' a))

instance NoThunks a => NoThunks (ExUnits' a)

instance NFData a => NFData (ExUnits' a)

deriving instance ToJSON a => ToJSON (ExUnits' a)

deriving instance FromJSON a => FromJSON (ExUnits' a)

-- | This newtype wrapper of ExUnits' is used to hide
--  an implementation detail inside the ExUnits pattern.
newtype ExUnits = WrapExUnits {unWrapExUnits :: ExUnits' Natural}
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance NoThunks ExUnits

instance NFData ExUnits

instance ToJSON ExUnits where
  toJSON exUnits@(ExUnits _ _) =
    let ExUnits {exUnitsMem, exUnitsSteps} = exUnits
     in object
          [ "exUnitsMem" .= toJSON exUnitsMem
          , "exUnitsSteps" .= toJSON exUnitsSteps
          ]

instance FromJSON ExUnits where
  parseJSON = withObject "exUnits" $ \o -> do
    exUnitsMem <- checkWord64Bounds =<< o .: "exUnitsMem"
    exUnitsSteps <- checkWord64Bounds =<< o .: "exUnitsSteps"
    pure $ ExUnits {exUnitsMem, exUnitsSteps}
    where
      checkWord64Bounds n =
        if n >= fromIntegral (minBound @Word64)
          && n <= fromIntegral (maxBound @Word64)
          then pure n
          else fail ("Unit out of bounds for Word64: " <> show n)

-- | Arbitrary execution unit in which we measure the cost of scripts in terms
-- of space in memory and execution time.
--
-- This pattern hides the fact that ExUnits' is parametric in the underlying type.
-- The ledger itself uses 'ExUnits' Natural' exclusively.
--
-- We would have preferred to use a type alias for 'ExUnits' Natural',
-- but this is not possible: https://gitlab.haskell.org/ghc/ghc/-/issues/19507.
pattern ExUnits :: Natural -> Natural -> ExUnits
pattern ExUnits {exUnitsMem, exUnitsSteps} <-
  WrapExUnits (ExUnits' exUnitsMem exUnitsSteps)
  where
    ExUnits m s = WrapExUnits (ExUnits' m s)

{-# COMPLETE ExUnits #-}

-- | It is deliberate that there is no `Ord` instance for `ExUnits`. Use this function to
--   compare if one `ExUnit` is pointwise compareable to another. In case when `Ord`
--   instance like comparison is necessary you can use @`zipSemiExUnits` `compare`@
pointWiseExUnits :: (Natural -> Natural -> Bool) -> ExUnits -> ExUnits -> Bool
pointWiseExUnits f ex1 ex2 = getAll (zipSemiExUnits (\x y -> All (f x y)) ex1 ex2)

-- | Pointwise combine units into a semigroup and mappened the results.
zipSemiExUnits :: Semigroup a => (Natural -> Natural -> a) -> ExUnits -> ExUnits -> a
zipSemiExUnits f (ExUnits m1 s1) (ExUnits m2 s2) = (m1 `f` m2) <> (s1 `f` s2)

-- =====================================

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
    cms <- mapM (parseCostModels o) [PlutusV1 .. PlutusV2]
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
costModelParamNames = \case
  PlutusV1 -> plutusV1ParamNames
  PlutusV2 -> plutusV2ParamNames

-- | There is a difference in 6 parameter names between the ones appearing alonzo genesis
-- files and the values returned by plutus via `showParamName` on the `ParamName` enum.
-- This listed is sorted in the order given by `ParamName` enum, so we can use it to sort
-- the costmodel param values before passing them to plutus `mkEvaluationContext`.
plutusV1ParamNames :: [Text]
plutusV1ParamNames =
  map (\newName -> Map.findWithDefault newName newName oldNewMapping) newNames
  where
    newNames = T.pack . Plutus.showParamName <$> [minBound .. maxBound :: PV1.ParamName]
    oldNewMapping =
      Map.fromList
        [ ("blake2b_256-cpu-arguments-intercept", "blake2b-cpu-arguments-intercept")
        , ("blake2b_256-cpu-arguments-slope", "blake2b-cpu-arguments-slope")
        , ("blake2b_256-memory-arguments", "blake2b-memory-arguments")
        , ("verifyEd25519Signature-cpu-arguments-intercept", "verifySignature-cpu-arguments-intercept")
        , ("verifyEd25519Signature-cpu-arguments-slope", "verifySignature-cpu-arguments-slope")
        , ("verifyEd25519Signature-memory-arguments", "verifySignature-memory-arguments")
        ]

plutusV2ParamNames :: [Text]
plutusV2ParamNames = T.pack . Plutus.showParamName <$> [minBound .. maxBound :: PV2.ParamName]

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
-- of Plutus, which we cannot yet even validate. These are stored in 'invalidCostModels'.
data CostModels = CostModels
  { costModelsValid :: !(Map Language CostModel)
  , costModelsErrors :: !(Map Language CostModelError)
  , costModelsUnknown :: !(Map Word8 [Integer])
  }
  deriving stock (Eq, Show, Ord, Generic)

emptyCostModels :: CostModels
emptyCostModels = CostModels mempty mempty mempty

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
    addError l e es = Map.insert l (CostModelError e) es

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

-- ==================================

-- | Prices per execution unit
data Prices = Prices
  { prMem :: !NonNegativeInterval
  , prSteps :: !NonNegativeInterval
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Prices

instance NFData Prices

instance ToJSON Prices where
  toJSON Prices {prSteps, prMem} =
    -- We cannot round-trip via NonNegativeInterval, so we go via Rational
    object
      [ "prSteps" .= toRationalJSON (unboundRational prSteps)
      , "prMem" .= toRationalJSON (unboundRational prMem)
      ]

toRationalJSON :: Rational -> Aeson.Value
toRationalJSON r =
  case fromRationalRepetendLimited 20 r of
    Right (s, Nothing) -> toJSON s
    _ -> toJSON r

instance FromJSON Prices where
  parseJSON =
    withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem <- checkBoundedRational mem
      return Prices {prSteps, prMem}
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s -> return s

-- | Compute the cost of a script based upon prices and the number of execution
-- units.
txscriptfee :: Prices -> ExUnits -> Coin
txscriptfee Prices {prMem, prSteps} ExUnits {exUnitsMem = m, exUnitsSteps = s} =
  Coin $
    ceiling $
      (fromIntegral m * unboundRational prMem)
        + (fromIntegral s * unboundRational prSteps)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

tagToWord8 :: Tag -> Word8
tagToWord8 = toEnum . fromEnum

word8ToTag :: Word8 -> Maybe Tag
word8ToTag e
  | fromEnum e > fromEnum (Prelude.maxBound :: Tag) = Nothing
  | fromEnum e < fromEnum (minBound :: Tag) = Nothing
  | otherwise = Just $ toEnum (fromEnum e)

instance EncCBOR Tag where
  encCBOR = encCBOR . tagToWord8

instance DecCBOR Tag where
  decCBOR =
    word8ToTag <$> decCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Tag" "Unknown redeemer tag"
      Just n -> pure n
  {-# INLINE decCBOR #-}

instance EncCBOR ExUnits where
  encCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance DecCBOR ExUnits where
  decCBOR = decode $ RecD ExUnits <! D decNat <! D decNat
    where
      decNat :: Decoder s Natural
      decNat = do
        x <- decCBOR
        when
          (x > fromIntegral (Prelude.maxBound :: Int64))
          ( cborError $
              DecoderErrorCustom "ExUnits field" "values must not exceed maxBound :: Int64"
          )
        pure $ wordToNatural x
      {-# INLINE decNat #-}
      wordToNatural :: Word64 -> Natural
      wordToNatural = fromIntegral
      {-# INLINE wordToNatural #-}
  {-# INLINE decCBOR #-}

instance EncCBOR Prices where
  encCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance DecCBOR Prices where
  decCBOR = decode $ RecD Prices <! From <! From
  {-# INLINE decCBOR #-}

instance Era era => EncCBOR (AlonzoScript era)

instance Era era => ToCBOR (AlonzoScript era) where
  toCBOR = toEraCBOR @era . encode . encodeScript

encodeScript :: Era era => AlonzoScript era -> Encode 'Open (AlonzoScript era)
encodeScript (TimelockScript i) = Sum TimelockScript 0 !> To i
-- Use the EncCBOR instance of ShortByteString:
encodeScript (PlutusScript PlutusV1 s) = Sum (PlutusScript PlutusV1) 1 !> To s
encodeScript (PlutusScript PlutusV2 s) = Sum (PlutusScript PlutusV2) 2 !> To s

instance Era era => DecCBOR (Annotator (AlonzoScript era)) where
  decCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript 0 = Ann (SumD TimelockScript) <*! From
      decodeScript 1 = Ann (SumD $ PlutusScript PlutusV1) <*! Ann From
      decodeScript 2 = Ann (SumD $ PlutusScript PlutusV2) <*! Ann From
      decodeScript n = Invalid n
      {-# INLINE decodeScript #-}
  {-# INLINE decCBOR #-}

-- | Test that every Alonzo script represents a real Script.
--     Run deepseq to see that there are no infinite computations and that
--     every Plutus Script unflattens into a real PV1.Script
validScript :: ProtVer -> AlonzoScript era -> Bool
validScript pv script =
  case script of
    TimelockScript sc -> deepseq sc True
    PlutusScript lang bytes ->
      let assertScriptWellFormed =
            case lang of
              PlutusV1 -> PV1.assertScriptWellFormed
              PlutusV2 -> PV2.assertScriptWellFormed
          eWellFormed :: Either PV1.ScriptDecodeError ()
          eWellFormed = assertScriptWellFormed (transProtocolVersion pv) bytes
       in isRight eWellFormed

transProtocolVersion :: ProtVer -> PV1.ProtocolVersion
transProtocolVersion (ProtVer major minor) =
  PV1.ProtocolVersion ((fromIntegral :: Word64 -> Int) (getVersion64 major)) (fromIntegral minor)

-- | Check the equality of two underlying types, while ignoring their binary
-- representation, which `Eq` instance normally does. This is used for testing.
eqAlonzoScriptRaw :: AlonzoScript era -> AlonzoScript era -> Bool
eqAlonzoScriptRaw (TimelockScript t1) (TimelockScript t2) = eqTimelockRaw t1 t2
eqAlonzoScriptRaw (PlutusScript l1 s1) (PlutusScript l2 s2) = l1 == l2 && s1 == s2
eqAlonzoScriptRaw _ _ = False

instance ToExpr CostModel where
  toExpr (CostModel lang cmmap _) =
    App "CostModel" [toExpr lang, toExpr cmmap, App "PV1.EvaluationContext" []]

instance ToExpr CostModels

instance ToExpr Prices

instance ToExpr ExUnits where
  toExpr (WrapExUnits (ExUnits' x y)) = App "ExUnits" [defaultExprViaShow x, defaultExprViaShow y]
