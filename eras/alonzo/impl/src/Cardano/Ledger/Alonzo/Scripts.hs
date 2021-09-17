{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ppTag,
    ppScript,
    isPlutusScript,
    alwaysSucceeds,
    alwaysFails,
    pointWiseExUnits,

    -- * Cost Model
    CostModel (..),
    ExUnits (..),
    Prices (..),
    hashCostModel,
    validateCostModelParams,
    ppExUnits,
    ppCostModel,
    ppPrices,
    decodeCostModelMap,
    decodeCostModel,

    -- * Deprecated
    defaultCostModel,
  )
where

import Cardano.Binary (DecoderError (..), FromCBOR (fromCBOR), ToCBOR (toCBOR), serialize')
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (hashScript))
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppInteger,
    ppMap,
    ppRational,
    ppRecord,
    ppScriptHash,
    ppSexp,
    ppString,
    ppWord64,
    text,
  )
import Cardano.Ledger.SafeHash
  ( HashWithCrypto (..),
    SafeHash,
    SafeToHash (..),
  )
import Cardano.Ledger.ShelleyMA.Timelocks
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Coders
import Data.DerivingVia (InstantiatedAt (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Measure (BoundedMeasure, Measure)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api (defaultCostModelParams, validateCostModelParams)
import qualified Plutus.V1.Ledger.Examples as Plutus
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )
import qualified Prettyprinter as PP

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

-- =======================================================

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data Script era
  = TimelockScript (Timelock (Crypto era))
  | PlutusScript ShortByteString -- A Plutus.V1.Ledger.Scripts(Script) that has been 'CBOR' encoded
  deriving (Eq, Generic, Ord)

instance (ValidateScript era, Core.Script era ~ Script era) => Show (Script era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript _) = "PlutusScript " ++ show (hashScript @era s)

deriving via
  InspectHeapNamed "Script" (Script era)
  instance
    NoThunks (Script era)

instance NFData (Script era)

-- | Both constructors know their original bytes
instance SafeToHash (Script era) where
  originalBytes (TimelockScript t) = originalBytes t
  originalBytes (PlutusScript bs) = fromShort bs

alwaysSucceeds, alwaysFails :: Natural -> Script era
alwaysSucceeds n = PlutusScript (Plutus.alwaysSucceedingNAryFunction n)
alwaysFails n = PlutusScript (Plutus.alwaysFailingNAryFunction n)

isPlutusScript :: Script era -> Bool
isPlutusScript (PlutusScript _) = True
isPlutusScript (TimelockScript _) = False

-- ===========================================

-- | Arbitrary execution unit in which we measure the cost of scripts.
data ExUnits = ExUnits
  { exUnitsMem :: !Word64,
    exUnitsSteps :: !Word64
  }
  deriving (Eq, Generic, Show)
  -- It is deliberate that there is no Ord instance, use `pointWiseExUnits` instead.
  deriving
    (BoundedMeasure, Measure)
    via (InstantiatedAt Generic ExUnits)
  deriving
    (Monoid, Semigroup)
    via (InstantiatedAt Measure ExUnits)

instance NoThunks ExUnits

instance NFData ExUnits

-- | It is deliberate that there is no `Ord` instance for `ExUnits`. Use this function
--   to compare if one `ExUnit` is pointwise compareable to another.
pointWiseExUnits :: (Word64 -> Word64 -> Bool) -> ExUnits -> ExUnits -> Bool
pointWiseExUnits oper (ExUnits m1 s1) (ExUnits m2 s2) = (m1 `oper` m2) && (s1 `oper` s2)

-- =====================================

newtype CostModel = CostModel (Map Text Integer)
  deriving (Eq, Generic, Show, Ord)

-- NOTE: Since cost model serializations need to be independently reproduced,
-- we use the 'canonical' serialization approach used in Byron.
instance ToCBOR CostModel where
  toCBOR (CostModel cm) = toCBOR $ Map.elems cm

instance SafeToHash CostModel where
  originalBytes = serialize'

-- CostModel does not determine 'crypto' so make a HashWithCrypto
-- rather than a HashAnotated instance.

instance HashWithCrypto CostModel CostModel

instance NoThunks CostModel

instance NFData CostModel

checkCostModel :: Map Text Integer -> Either String CostModel
checkCostModel cm =
  if validateCostModelParams cm
    then Right (CostModel cm)
    else Left ("Invalid cost model: " ++ show cm)

{-# DEPRECATED defaultCostModel "Use 'import Test.Cardano.Ledger.Alonzo.PlutusScripts' instead." #-}
defaultCostModel :: Maybe CostModel
defaultCostModel = CostModel <$> defaultCostModelParams

decodeCostModelMap :: Decoder s (Map Language CostModel)
decodeCostModelMap = decodeMapByKey fromCBOR decodeCostModel

decodeCostModel :: Language -> Decoder s CostModel
decodeCostModel PlutusV1 =
  case defaultCostModelParams of
    Nothing -> fail "Default Plutus Cost Model is corrupt."
    Just dcm -> do
      checked <- checkCostModel <$> decodeArrayAsMap (Map.keysSet dcm) fromCBOR
      case checked of
        Left e -> fail e
        Right cm -> pure cm

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
txscriptfee Prices {prMem, prSteps} ExUnits {exUnitsMem, exUnitsSteps} =
  Coin $
    ceiling $
      (fromIntegral exUnitsMem * unboundRational prMem)
        + (fromIntegral exUnitsSteps * unboundRational prSteps)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

tagToWord8 :: Tag -> Word8
tagToWord8 = toEnum . fromEnum

word8ToTag :: Word8 -> Maybe Tag
word8ToTag e
  | fromEnum e > fromEnum (maxBound :: Tag) = Nothing
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
  fromCBOR = decode $ RecD ExUnits <! From <! From

instance ToCBOR Prices where
  toCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance FromCBOR Prices where
  fromCBOR = decode $ RecD Prices <! From <! From

instance forall era. (Typeable (Crypto era), Typeable era) => ToCBOR (Script era) where
  toCBOR x = encode (encodeScript x)

encodeScript :: (Typeable (Crypto era)) => Script era -> Encode 'Open (Script era)
encodeScript (TimelockScript i) = Sum TimelockScript 0 !> To i
encodeScript (PlutusScript s) = Sum PlutusScript 1 !> To s -- Use the ToCBOR instance of ShortByteString

instance
  (CC.Crypto (Crypto era), Typeable (Crypto era), Typeable era) =>
  FromCBOR (Annotator (Script era))
  where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (Script era))
      decodeScript 0 = Ann (SumD TimelockScript) <*! From
      decodeScript 1 = Ann (SumD PlutusScript) <*! Ann From
      decodeScript n = Invalid n

-- ============================================================
-- Pretty printing versions

ppTag :: Tag -> PDoc
ppTag x = ppString (show x)

instance PrettyA Tag where prettyA = ppTag

ppScript :: forall era. (ValidateScript era, Core.Script era ~ Script era) => Script era -> PDoc
ppScript s@(PlutusScript _) = ppString "PlutusScript " PP.<+> ppScriptHash (hashScript @era s)
ppScript (TimelockScript x) = ppTimelock x

instance (ValidateScript era, Core.Script era ~ Script era) => PrettyA (Script era) where prettyA = ppScript

ppExUnits :: ExUnits -> PDoc
ppExUnits (ExUnits mem step) =
  ppRecord "ExUnits" [("memory", ppWord64 mem), ("steps", ppWord64 step)]

instance PrettyA ExUnits where prettyA = ppExUnits

ppCostModel :: CostModel -> PDoc
ppCostModel (CostModel m) =
  ppSexp "CostModel" [ppMap text ppInteger m]

instance PrettyA CostModel where prettyA = ppCostModel

ppPrices :: Prices -> PDoc
ppPrices Prices {prMem, prSteps} =
  ppRecord
    "Prices"
    [ ("prMem", ppRational $ unboundRational prMem),
      ("prSteps", ppRational $ unboundRational prSteps)
    ]

instance PrettyA Prices where prettyA = ppPrices
