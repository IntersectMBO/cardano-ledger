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
  mkCostModel,
  encodeCostModel,
  getCostModelLanguage,
  getCostModelParams,
  getEvaluationContext,
  ExUnits (ExUnits, exUnitsMem, exUnitsSteps, ..),
  ExUnits',
  Prices (..),
  decodeCostModelMap,
  decodeCostModel,
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
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  Encoding,
  FromCBOR (fromCBOR),
  ToCBOR (toCBOR),
  cborError,
  decodeMapByKey,
  encodeFoldableAsDefLenList,
  encodeMap,
  getVersion64,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (Ann, D, From, Invalid, RecD, SumD, Summands),
  Encode (Enc, Rec, Sum, To),
  Wrapped (Open),
  decode,
  encode,
  (!>),
  (<!),
  (<*!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (..), defaultExprViaShow)
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Monad (when)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Aeson (
  Array,
  FromJSON (..),
  Object,
  ToJSON (..),
  object,
  withArray,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson (Value)
import Data.Aeson.Types (Parser)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.DerivingVia (InstantiatedAt (..))
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Measure (BoundedMeasure, Measure)
import Data.Scientific (fromRationalRepetendLimited)
import Data.Semigroup (All (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector as Vector (toList)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Numeric.Natural (Natural)
import PlutusCore.Evaluation.Machine.CostModelInterface (CostModelApplyWarn)
import qualified PlutusLedgerApi.V1 as PV1 (
  CostModelApplyError (..),
  EvaluationContext,
  ProtocolVersion (ProtocolVersion),
  ScriptDecodeError,
  assertScriptWellFormed,
  mkEvaluationContext,
 )
import qualified PlutusLedgerApi.V2 as PV2 (assertScriptWellFormed, mkEvaluationContext)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

instance NoThunks Tag

instance NFData Tag where
  rnf = rwhnf

-- =======================================================

-- | Binary representation of a Plutus script.
newtype BinaryPlutus = BinaryPlutus {unBinaryPlutus :: ShortByteString}
  deriving stock (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR, NFData)

instance FromCBOR (Annotator BinaryPlutus) where
  fromCBOR = pure <$> fromCBOR

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data AlonzoScript era
  = TimelockScript (Timelock era)
  | PlutusScript Language ShortByteString
  deriving (Eq, Generic)

instance (EraScript era, Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript v _) = "PlutusScript " ++ show v ++ " " ++ show (hashScript @era s)

deriving via
  InspectHeapNamed "AlonzoScript" (AlonzoScript era)
  instance
    NoThunks (AlonzoScript era)

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
  toJSON ExUnits {exUnitsMem = m, exUnitsSteps = s} =
    object
      [ "exUnitsMem" .= toJSON m
      , "exUnitsSteps" .= toJSON s
      ]

instance FromJSON ExUnits where
  parseJSON = withObject "exUnits" $ \o -> do
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
  , cmMap :: [Integer]
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
    v1CostModels <- legacyParseCostModels o
    v2CostModels <- parseCostModels o
    pure $ CostModels $ Map.fromList (v1CostModels ++ v2CostModels)

-- | The costmodel parameters in Alonzo Genesis are represented as a map.  Plutus API does
-- no longer require the map as a parameter to `mkEvaluationContext`, but the list of
-- Integers representing the values of the map.  The expectation on this list of Integers
-- is that they are sorted in the order given by the `ParamName` enum, so even though we
-- just have to pass the list to plutus, we still need to use the names of the parameters
-- in order to sort the list.  In new versions, we want to represent the costmodel
-- parameters directly as a list, so we can avoid this reordering, hence the name of this
-- function.
legacyParseCostModels :: Object -> Parser [(Language, CostModel)]
legacyParseCostModels o = do
  plutusV1 <- o .:? "PlutusV1"
  cms <- traverse (validateCostModel PlutusV1 . cmParamValues) plutusV1
  pure $ maybeToList cms
  where
    cmParamValues :: Map Text Integer -> [Integer]
    cmParamValues cmMap = mapMaybe (`Map.lookup` cmMap) plutusV1ParamNames

parseCostModels :: Object -> Parser [(Language, CostModel)]
parseCostModels o = do
  plutusV2 <- o .:? "PlutusV2"
  maybeCostModels <- traverse (withArray "PlutusV2 values" parseCostModelsV2) plutusV2
  pure $ maybeToList maybeCostModels
  where
    parseCostModelsV2 :: Array -> Parser ((Language, CostModel))
    parseCostModelsV2 array = do
      paramValues <- mapM parseJSON $ Vector.toList array
      validateCostModel PlutusV2 paramValues

-- | We list the param names instead of using `enumerate PlutusLedgerApi.V1.ParamName`,
-- because there is a difference in 6 parameter names between the ones appearing alonzo
-- genesis files and the values returned by plutus via `showParamName` on the `ParamName`
-- enum.  This listed is sorted in the order given by `ParamName` enum, so we can use it
-- to sort the costmodel param values before passing them to plutus `mkEvaluationContext`.
plutusV1ParamNames :: [Text]
plutusV1ParamNames =
  [ "addInteger-cpu-arguments-intercept"
  , "addInteger-cpu-arguments-slope"
  , "addInteger-memory-arguments-intercept"
  , "addInteger-memory-arguments-slope"
  , "appendByteString-cpu-arguments-intercept"
  , "appendByteString-cpu-arguments-slope"
  , "appendByteString-memory-arguments-intercept"
  , "appendByteString-memory-arguments-slope"
  , "appendString-cpu-arguments-intercept"
  , "appendString-cpu-arguments-slope"
  , "appendString-memory-arguments-intercept"
  , "appendString-memory-arguments-slope"
  , "bData-cpu-arguments"
  , "bData-memory-arguments"
  , "blake2b-cpu-arguments-intercept"
  , "blake2b-cpu-arguments-slope"
  , "blake2b-memory-arguments"
  , "cekApplyCost-exBudgetCPU"
  , "cekApplyCost-exBudgetMemory"
  , "cekBuiltinCost-exBudgetCPU"
  , "cekBuiltinCost-exBudgetMemory"
  , "cekConstCost-exBudgetCPU"
  , "cekConstCost-exBudgetMemory"
  , "cekDelayCost-exBudgetCPU"
  , "cekDelayCost-exBudgetMemory"
  , "cekForceCost-exBudgetCPU"
  , "cekForceCost-exBudgetMemory"
  , "cekLamCost-exBudgetCPU"
  , "cekLamCost-exBudgetMemory"
  , "cekStartupCost-exBudgetCPU"
  , "cekStartupCost-exBudgetMemory"
  , "cekVarCost-exBudgetCPU"
  , "cekVarCost-exBudgetMemory"
  , "chooseData-cpu-arguments"
  , "chooseData-memory-arguments"
  , "chooseList-cpu-arguments"
  , "chooseList-memory-arguments"
  , "chooseUnit-cpu-arguments"
  , "chooseUnit-memory-arguments"
  , "consByteString-cpu-arguments-intercept"
  , "consByteString-cpu-arguments-slope"
  , "consByteString-memory-arguments-intercept"
  , "consByteString-memory-arguments-slope"
  , "constrData-cpu-arguments"
  , "constrData-memory-arguments"
  , "decodeUtf8-cpu-arguments-intercept"
  , "decodeUtf8-cpu-arguments-slope"
  , "decodeUtf8-memory-arguments-intercept"
  , "decodeUtf8-memory-arguments-slope"
  , "divideInteger-cpu-arguments-constant"
  , "divideInteger-cpu-arguments-model-arguments-intercept"
  , "divideInteger-cpu-arguments-model-arguments-slope"
  , "divideInteger-memory-arguments-intercept"
  , "divideInteger-memory-arguments-minimum"
  , "divideInteger-memory-arguments-slope"
  , "encodeUtf8-cpu-arguments-intercept"
  , "encodeUtf8-cpu-arguments-slope"
  , "encodeUtf8-memory-arguments-intercept"
  , "encodeUtf8-memory-arguments-slope"
  , "equalsByteString-cpu-arguments-constant"
  , "equalsByteString-cpu-arguments-intercept"
  , "equalsByteString-cpu-arguments-slope"
  , "equalsByteString-memory-arguments"
  , "equalsData-cpu-arguments-intercept"
  , "equalsData-cpu-arguments-slope"
  , "equalsData-memory-arguments"
  , "equalsInteger-cpu-arguments-intercept"
  , "equalsInteger-cpu-arguments-slope"
  , "equalsInteger-memory-arguments"
  , "equalsString-cpu-arguments-constant"
  , "equalsString-cpu-arguments-intercept"
  , "equalsString-cpu-arguments-slope"
  , "equalsString-memory-arguments"
  , "fstPair-cpu-arguments"
  , "fstPair-memory-arguments"
  , "headList-cpu-arguments"
  , "headList-memory-arguments"
  , "iData-cpu-arguments"
  , "iData-memory-arguments"
  , "ifThenElse-cpu-arguments"
  , "ifThenElse-memory-arguments"
  , "indexByteString-cpu-arguments"
  , "indexByteString-memory-arguments"
  , "lengthOfByteString-cpu-arguments"
  , "lengthOfByteString-memory-arguments"
  , "lessThanByteString-cpu-arguments-intercept"
  , "lessThanByteString-cpu-arguments-slope"
  , "lessThanByteString-memory-arguments"
  , "lessThanEqualsByteString-cpu-arguments-intercept"
  , "lessThanEqualsByteString-cpu-arguments-slope"
  , "lessThanEqualsByteString-memory-arguments"
  , "lessThanEqualsInteger-cpu-arguments-intercept"
  , "lessThanEqualsInteger-cpu-arguments-slope"
  , "lessThanEqualsInteger-memory-arguments"
  , "lessThanInteger-cpu-arguments-intercept"
  , "lessThanInteger-cpu-arguments-slope"
  , "lessThanInteger-memory-arguments"
  , "listData-cpu-arguments"
  , "listData-memory-arguments"
  , "mapData-cpu-arguments"
  , "mapData-memory-arguments"
  , "mkCons-cpu-arguments"
  , "mkCons-memory-arguments"
  , "mkNilData-cpu-arguments"
  , "mkNilData-memory-arguments"
  , "mkNilPairData-cpu-arguments"
  , "mkNilPairData-memory-arguments"
  , "mkPairData-cpu-arguments"
  , "mkPairData-memory-arguments"
  , "modInteger-cpu-arguments-constant"
  , "modInteger-cpu-arguments-model-arguments-intercept"
  , "modInteger-cpu-arguments-model-arguments-slope"
  , "modInteger-memory-arguments-intercept"
  , "modInteger-memory-arguments-minimum"
  , "modInteger-memory-arguments-slope"
  , "multiplyInteger-cpu-arguments-intercept"
  , "multiplyInteger-cpu-arguments-slope"
  , "multiplyInteger-memory-arguments-intercept"
  , "multiplyInteger-memory-arguments-slope"
  , "nullList-cpu-arguments"
  , "nullList-memory-arguments"
  , "quotientInteger-cpu-arguments-constant"
  , "quotientInteger-cpu-arguments-model-arguments-intercept"
  , "quotientInteger-cpu-arguments-model-arguments-slope"
  , "quotientInteger-memory-arguments-intercept"
  , "quotientInteger-memory-arguments-minimum"
  , "quotientInteger-memory-arguments-slope"
  , "remainderInteger-cpu-arguments-constant"
  , "remainderInteger-cpu-arguments-model-arguments-intercept"
  , "remainderInteger-cpu-arguments-model-arguments-slope"
  , "remainderInteger-memory-arguments-intercept"
  , "remainderInteger-memory-arguments-minimum"
  , "remainderInteger-memory-arguments-slope"
  , "sha2_256-cpu-arguments-intercept"
  , "sha2_256-cpu-arguments-slope"
  , "sha2_256-memory-arguments"
  , "sha3_256-cpu-arguments-intercept"
  , "sha3_256-cpu-arguments-slope"
  , "sha3_256-memory-arguments"
  , "sliceByteString-cpu-arguments-intercept"
  , "sliceByteString-cpu-arguments-slope"
  , "sliceByteString-memory-arguments-intercept"
  , "sliceByteString-memory-arguments-slope"
  , "sndPair-cpu-arguments"
  , "sndPair-memory-arguments"
  , "subtractInteger-cpu-arguments-intercept"
  , "subtractInteger-cpu-arguments-slope"
  , "subtractInteger-memory-arguments-intercept"
  , "subtractInteger-memory-arguments-slope"
  , "tailList-cpu-arguments"
  , "tailList-memory-arguments"
  , "trace-cpu-arguments"
  , "trace-memory-arguments"
  , "unBData-cpu-arguments"
  , "unBData-memory-arguments"
  , "unConstrData-cpu-arguments"
  , "unConstrData-memory-arguments"
  , "unIData-cpu-arguments"
  , "unIData-memory-arguments"
  , "unListData-cpu-arguments"
  , "unListData-memory-arguments"
  , "unMapData-cpu-arguments"
  , "unMapData-memory-arguments"
  , "verifySignature-cpu-arguments-intercept"
  , "verifySignature-cpu-arguments-slope"
  , "verifySignature-memory-arguments"
  ]

validateCostModel :: MonadFail m => Language -> [Integer] -> m (Language, CostModel)
validateCostModel lang cmps =
  case mkCostModel lang cmps of
    Left err -> fail $ show err
    Right cm -> pure (lang, cm)

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

decodeCostModelMap :: Decoder s (Map Language CostModel)
decodeCostModelMap = decodeMapByKey fromCBOR decodeCostModel

decodeCostModel :: Language -> Decoder s CostModel
decodeCostModel lang = do
  checked <- mkCostModel lang <$> fromCBOR
  case checked of
    Left e -> fail $ show e
    Right cm -> pure cm

getEvaluationContext :: CostModel -> PV1.EvaluationContext
getEvaluationContext (CostModel _ _ ec) = ec

newtype CostModels = CostModels {unCostModels :: Map Language CostModel}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (NFData, NoThunks)

instance FromCBOR CostModels where
  fromCBOR = CostModels <$> decodeCostModelMap

instance ToCBOR CostModels where
  toCBOR = encodeMap toCBOR encodeCostModel . unCostModels

instance ToJSON CostModel where
  toJSON = toJSON . getCostModelParams

instance ToJSON CostModels where
  toJSON = toJSON . unCostModels

-- | Encoding for the `CostModel`. Important to note that it differs from `Encoding` used
-- by `Cardano.Ledger.Alonzo.PParams.getLanguageView`
encodeCostModel :: CostModel -> Encoding
encodeCostModel = encodeFoldableAsDefLenList toCBOR . getCostModelParams

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

instance ToCBOR Tag where
  toCBOR = toCBOR . tagToWord8

instance FromCBOR Tag where
  fromCBOR =
    word8ToTag <$> fromCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Tag" "Unknown redeemer tag"
      Just n -> pure n

instance ToCBOR ExUnits where
  toCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance FromCBOR ExUnits where
  fromCBOR = decode $ RecD ExUnits <! D decNat <! D decNat
    where
      decNat :: Decoder s Natural
      decNat = do
        x <- fromCBOR
        when
          (x > fromIntegral (Prelude.maxBound :: Int64))
          ( cborError $
              DecoderErrorCustom "ExUnits field" "values must not exceed maxBound :: Int64"
          )
        pure $ wordToNatural x
      wordToNatural :: Word64 -> Natural
      wordToNatural = fromIntegral

instance ToCBOR Prices where
  toCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance FromCBOR Prices where
  fromCBOR = decode $ RecD Prices <! From <! From

instance Era era => ToCBOR (AlonzoScript era)

instance Era era => EncCBOR (AlonzoScript era) where
  -- This is a workaround the fact that `encodeScript` uses `Sum`. Neither `EncCBOR`, nor
  -- `ToCBOR` are used for anything. This should be fixed.
  encCBOR = toPlainEncoding (eraProtVerLow @era) . encode . encodeScript

encodeScript :: (Typeable era) => AlonzoScript era -> Encode 'Open (AlonzoScript era)
encodeScript (TimelockScript i) = Sum TimelockScript 0 !> Enc i
-- Use the ToCBOR instance of ShortByteString:
encodeScript (PlutusScript PlutusV1 s) = Sum (PlutusScript PlutusV1) 1 !> To s
encodeScript (PlutusScript PlutusV2 s) = Sum (PlutusScript PlutusV2) 2 !> To s

instance Era era => FromCBOR (Annotator (AlonzoScript era)) where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript 0 = Ann (SumD TimelockScript) <*! From
      decodeScript 1 = Ann (SumD $ PlutusScript PlutusV1) <*! Ann From
      decodeScript 2 = Ann (SumD $ PlutusScript PlutusV2) <*! Ann From
      decodeScript n = Invalid n

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
