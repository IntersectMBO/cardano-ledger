{-# LANGUAGE DataKinds #-}
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

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    BinaryPlutus (..),
    AlonzoScript (TimelockScript, PlutusScript),
    Script,
    txscriptfee,
    isPlutusScript,
    pointWiseExUnits,
    validScript,
    transProtocolVersion,

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

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational), NonNegativeInterval, ProtVer (..))
import Cardano.Ledger.Binary
  ( Annotator,
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    cborError,
    decodeMapByKey,
    encodeFoldableAsDefLenList,
    encodeMap,
    getVersion64,
  )
import Cardano.Ledger.Binary.Coders
  ( Decode (Ann, D, From, Invalid, RecD, SumD, Summands),
    Encode (Rec, Sum, To),
    Wrapped (Open),
    decode,
    encode,
    (!>),
    (<!),
    (<*!),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
  ( Era (EraCrypto),
    EraScript,
    Phase (..),
    PhaseRep (..),
    PhasedScript (..),
    SomeScript,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Monad (when)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.DerivingVia (InstantiatedAt (..))
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Measure (BoundedMeasure, Measure)
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Numeric.Natural (Natural)
import PlutusCore.Evaluation.Machine.CostModelInterface (CostModelApplyWarn)
import qualified PlutusLedgerApi.V1 as PV1
  ( CostModelApplyError (..),
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

type Script era = AlonzoScript era

{-# DEPRECATED Script "Use `AlonzoScript` instead" #-}

instance (EraScript era, Core.Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript v _) = "PlutusScript " ++ show v ++ " " ++ show (Core.hashScript @era s)

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

instance CC.Crypto c => EraScript (AlonzoEra c) where
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
  { exUnitsMem' :: !a,
    exUnitsSteps' :: !a
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

-- | This newtype wrapper of ExUnits' is used to hide
--  an implementation detail inside the ExUnits pattern.
newtype ExUnits = WrapExUnits {unWrapExUnits :: ExUnits' Natural}
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance NoThunks ExUnits

instance NFData ExUnits

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

-- | It is deliberate that there is no `Ord` instance for `ExUnits`. Use this function
--   to compare if one `ExUnit` is pointwise compareable to another.
pointWiseExUnits :: (Natural -> Natural -> Bool) -> ExUnits -> ExUnits -> Bool
pointWiseExUnits oper (ExUnits m1 s1) (ExUnits m2 s2) = (m1 `oper` m2) && (s1 `oper` s2)

-- =====================================

-- | A language dependent cost model for the Plutus evaluator.
-- Note that the `PV1.EvaluationContext` is entirely dependent on the
-- cost model parameters (ie the `Map` `Text` `Integer`) and that
-- this type uses the smart constructor `mkCostModel`
-- to hide the evaluation context.
data CostModel = CostModel
  { cmLanguage :: !Language,
    cmMap :: [Integer],
    cmEvalCtx :: !PV1.EvaluationContext
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
  deriving (Eq, Show, Ord, Generic, NFData, NoThunks)

instance FromCBOR CostModels where
  fromCBOR = CostModels <$> decodeCostModelMap

instance ToCBOR CostModels where
  toCBOR = encodeMap toCBOR encodeCostModel . unCostModels

-- | Encoding for the `CostModel`. Important to note that it differs from `Encoding` used
-- by `Cardano.Ledger.Alonzo.PParams.getLanguageView`
encodeCostModel :: CostModel -> Encoding
encodeCostModel = encodeFoldableAsDefLenList toCBOR . getCostModelParams

-- ==================================

-- | Prices per execution unit
data Prices = Prices
  { prMem :: !NonNegativeInterval,
    prSteps :: !NonNegativeInterval
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Prices

instance NFData Prices

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

instance (Typeable (EraCrypto era), Typeable era) => ToCBOR (Script era) where
  toCBOR x = encode (encodeScript x)

encodeScript :: (Typeable era) => Script era -> Encode 'Open (Script era)
encodeScript (TimelockScript i) = Sum TimelockScript 0 !> To i
-- Use the ToCBOR instance of ShortByteString:
encodeScript (PlutusScript PlutusV1 s) = Sum (PlutusScript PlutusV1) 1 !> To s
encodeScript (PlutusScript PlutusV2 s) = Sum (PlutusScript PlutusV2) 2 !> To s

instance Era era => FromCBOR (Annotator (Script era)) where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (Script era))
      decodeScript 0 = Ann (SumD TimelockScript) <*! From
      decodeScript 1 = Ann (SumD $ PlutusScript PlutusV1) <*! Ann From
      decodeScript 2 = Ann (SumD $ PlutusScript PlutusV2) <*! Ann From
      decodeScript n = Invalid n

-- | Test that every Alonzo script represents a real Script.
--     Run deepseq to see that there are no infinite computations and that
--     every Plutus Script unflattens into a real PV1.Script
validScript :: ProtVer -> Script era -> Bool
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
