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
-- Needed for FromCBOR(Annotator CostModel)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    Script (TimelockScript, PlutusScript),
    txscriptfee,
    isPlutusScript,
    pointWiseExUnits,

    -- * Cost Model
    CostModel (..),
    getEvaluationContext,
    ExUnits (ExUnits, exUnitsMem, exUnitsSteps, ..),
    ExUnits',
    Prices (..),
    hashCostModel,
    isCostModelParamsWellFormed,
    decodeCostModelMap,
    decodeCostModel,
    CostModels (..),

    -- * Deprecated
    validateCostModelParams,
  )
where

import Cardano.Binary (DecoderError (..), FromCBOR (fromCBOR), ToCBOR (toCBOR), serialize')
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (hashScript))
import Cardano.Ledger.SafeHash
  ( HashWithCrypto (..),
    SafeHash,
    SafeToHash (..),
  )
import Cardano.Ledger.Serialization (mapToCBOR)
import Cardano.Ledger.ShelleyMA.Timelocks
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (when)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Coders
import Data.DerivingVia (InstantiatedAt (..))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Measure (BoundedMeasure, Measure)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api as PV1 hiding (Map, Script)
import Plutus.V2.Ledger.Api as PV2 hiding (Map, Script)

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

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data Script era
  = TimelockScript (Timelock (Crypto era))
  | PlutusScript Language ShortByteString
  deriving (Eq, Generic, Ord)

instance (ValidateScript era, Core.Script era ~ Script era) => Show (Script era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript v _) = "PlutusScript " ++ show v ++ " " ++ show (hashScript @era s)

deriving via
  InspectHeapNamed "Script" (Script era)
  instance
    NoThunks (Script era)

instance NFData (Script era)

-- | Both constructors know their original bytes
instance SafeToHash (Script era) where
  originalBytes (TimelockScript t) = originalBytes t
  originalBytes (PlutusScript _ bs) = fromShort bs

isPlutusScript :: Script era -> Bool
isPlutusScript (PlutusScript _ _) = True
isPlutusScript (TimelockScript _) = False

-- ===========================================

-- | Arbitrary execution unit in which we measure the cost of scripts in terms
-- of space in memory and execution time.
--
-- The ledger itself uses 'ExUnits' Natural' exclusively, but the flexibility here
-- alows the consensus layer to translate the execution units into something
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

data CostModel
  = CostModelV1 (Map Text Integer) PV1.EvaluationContext
  | CostModelV2 (Map Text Integer) PV2.EvaluationContext

instance Eq CostModel where
  (CostModelV1 x _) == (CostModelV1 y _) = x == y
  (CostModelV2 x _) == (CostModelV2 y _) = x == y
  _ == _ = False

instance Show CostModel where
  show (CostModelV1 cm _) = "CostModelV1 " <> show cm
  show (CostModelV2 cm _) = "CostModelV2 " <> show cm

instance Ord CostModel where
  compare (CostModelV1 x _) (CostModelV1 y _) = compare x y
  compare (CostModelV2 x _) (CostModelV2 y _) = compare x y
  compare (CostModelV1 _ _) (CostModelV2 _ _) = LT
  compare (CostModelV2 _ _) (CostModelV1 _ _) = GT

-- NOTE: Since cost model serializations need to be independently reproduced,
-- we use the 'canonical' serialization approach used in Byron.
instance ToCBOR CostModel where
  toCBOR (CostModelV1 cm _) = encodeFoldableAsDefinite $ Map.elems cm
  toCBOR (CostModelV2 cm _) = encodeFoldableAsDefinite $ Map.elems cm

instance SafeToHash CostModel where
  originalBytes = serialize'

-- CostModel does not determine 'crypto' so make a HashWithCrypto
-- rather than a HashAnotated instance.

instance HashWithCrypto CostModel CostModel

deriving via InspectHeapNamed "PV1.EvaluationContext" PV1.EvaluationContext instance NoThunks PV1.EvaluationContext

deriving via InspectHeapNamed "CostModel" CostModel instance NoThunks CostModel

instance NFData CostModel where
  rnf (CostModelV1 cm ectx) = seq (rnf cm) (rnf ectx)
  rnf (CostModelV2 cm ectx) = seq (rnf cm) (rnf ectx)

-- | Convert cost model parameters to a cost model, making use of the
--  conversion function mkEvaluationContext from the Plutus API.
costModelParamsToCostModel :: Language -> Map Text Integer -> Either String CostModel
costModelParamsToCostModel PlutusV1 cm =
  case PV1.mkEvaluationContext cm of
    Just evalCtx -> Right (CostModelV1 cm evalCtx)
    Nothing -> Left ("Invalid PlutusV1 cost model: " ++ show cm)
costModelParamsToCostModel PlutusV2 cm =
  case PV2.mkEvaluationContext cm of
    Just evalCtx -> Right (CostModelV2 cm evalCtx)
    Nothing -> Left ("Invalid PlutusV2 cost model: " ++ show cm)

decodeCostModelMap :: Decoder s (Map Language CostModel)
decodeCostModelMap = decodeMapByKey fromCBOR decodeCostModel

decodeCostModel :: Language -> Decoder s CostModel
decodeCostModel lang = do
  checked <- costModelParamsToCostModel lang <$> decodeArrayAsMap keys fromCBOR
  case checked of
    Left e -> fail e
    Right cm -> pure cm
  where
    keys = case lang of
      PlutusV1 -> PV1.costModelParamNames
      PlutusV2 -> PV2.costModelParamNames

decodeArrayAsMap :: Ord a => Set a -> Decoder s b -> Decoder s (Map a b)
decodeArrayAsMap keys decodeValue = do
  values <- decodeList decodeValue
  let numValues = length values
      numKeys = Set.size keys
  when (numValues /= numKeys) $
    fail $
      "Expected array with " <> show numKeys
        <> " entries, but encoded array has "
        <> show numValues
        <> " entries."
  pure $ Map.fromList $ zip (Set.toAscList keys) values

-- CostModel is not parameterized by Crypto or Era so we use the
-- hashWithCrypto function, rather than hashAnnotated

hashCostModel ::
  forall e.
  Era e =>
  Proxy e ->
  CostModel ->
  SafeHash (Crypto e) CostModel
hashCostModel _proxy = hashWithCrypto (Proxy @(Crypto e))

getEvaluationContext :: CostModel -> PV1.EvaluationContext
getEvaluationContext (CostModelV1 _ ec) = ec
getEvaluationContext (CostModelV2 _ ec) = ec

newtype CostModels = CostModels {unCostModels :: Map Language CostModel}
  deriving (Eq, Show, Ord, Generic, NFData, NoThunks)

instance FromCBOR CostModels where
  fromCBOR = CostModels <$> decodeCostModelMap

instance ToCBOR CostModels where
  toCBOR = mapToCBOR . unCostModels

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

instance forall era. (Typeable (Crypto era), Typeable era) => ToCBOR (Script era) where
  toCBOR x = encode (encodeScript x)

encodeScript :: (Typeable (Crypto era)) => Script era -> Encode 'Open (Script era)
encodeScript (TimelockScript i) = Sum TimelockScript 0 !> To i
encodeScript (PlutusScript PlutusV1 s) = Sum (PlutusScript PlutusV1) 1 !> To s -- Use the ToCBOR instance of ShortByteString
encodeScript (PlutusScript PlutusV2 s) = Sum (PlutusScript PlutusV1) 2 !> To s

instance
  (CC.Crypto (Crypto era), Typeable (Crypto era), Typeable era) =>
  FromCBOR (Annotator (Script era))
  where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (Script era))
      decodeScript 0 = Ann (SumD TimelockScript) <*! From
      decodeScript 1 = Ann (SumD $ PlutusScript PlutusV1) <*! Ann From
      decodeScript 2 = Ann (SumD $ PlutusScript PlutusV2) <*! Ann From
      decodeScript n = Invalid n

{-# DEPRECATED validateCostModelParams "this function is replaced by isCostModelParamsWellFormed" #-}
validateCostModelParams :: CostModelParams -> Bool
validateCostModelParams = isCostModelParamsWellFormed
