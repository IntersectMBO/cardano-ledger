{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module CostModel
    (
      ExUnitsMSIG(..)
    , ExUnitsPLC(..)
    , ExUnits(..)
    , CostModMSIG(..)
    , CostModPLC(..)
    , CostMod(..)
    , PricesMSIG(..)
    , PricesPLC(..)
    , Prices(..)
    , ExUnitsAllTypes(..)
    , defaultUnits
    , defaultModel
    , defaultPrices
    , scriptFee
    ) where


import           Cardano.Prelude (NoUnexpectedThunks(..))
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Binary (Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, decodeWord,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen, encodeWord,
                     enforceSize, matchSize)
import           BaseTypes (CborSeq (..), UnitInterval, invalidKey)
--import           Serialization (CBORGroup (..), FromCBORGroup (..), ToCBORGroup (..))
--import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))

import           Cardano.Ledger.Shelley.Crypto
import           Scripts

-- | comparing required resources for Plutus scripts
instance Ord ExUnitsPLC where
   (<=) (ExUnitsPLC rs1 mu1) (ExUnitsPLC rs2 mu2) =
     ((<=) rs1 rs2) && ((<=) mu1 mu2)

-- | comparing required resources for MSIG scripts
instance Ord ExUnitsMSIG where
  (<=) (ExUnitsMSIG ns1 m1) (ExUnitsMSIG ns2 m2) =
    ((<=) ns1 ns2) && ((<=) m1 m2)

-- | Temporary stand-in for actual types in the execution cost
-- for Plutus script execution
data ExUnitsPLC = ExUnitsPLC
  { -- | The types of computational resources relevant to the cost model
      reductionSteps           :: Integer
    , memoryUnits              :: Integer
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ExUnitsPLC

-- | Temporary stand-in for actual types in the execution cost
-- for MSIG script execution
data ExUnitsMSIG = ExUnitsMSIG
  { -- | The types of computational resources relevant to the cost model
      numSigs                  :: Integer
    , maybeSomething           :: Integer
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ExUnitsMSIG

-- | The execution units of arbitrary scripts (MSIG, PLC so far)
data ExUnits =
       PLCUnits ExUnitsPLC
    |  MSIGUnits ExUnitsMSIG
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ExUnits

-- | Temporary stand-in for actual types in the cost model
-- for Plutus script execution
data CostModPLC = CostModPLC
  { -- | The types of computational resources relevant to the cost model
      stepPrim              :: ExUnitsPLC
    , memPrim               :: ExUnitsPLC
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks CostModPLC

-- | Temporary stand-in for actual types in the cost model
-- for MSIG script execution
data CostModMSIG = CostModMSIG
  { -- | The types of computational resources relevant to the cost model
      sigPrim           :: ExUnitsMSIG
    , smtPrim           :: ExUnitsMSIG
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks CostModMSIG

-- | The cost model for plc or msig scripts
data CostMod = CostMod
  { -- | The types of computational resources relevant to the cost model
      costModPLC            :: CostModPLC
    , costModMSIG           :: CostModMSIG
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks CostMod

-- | Default values
-- | Default execution units
defaultUnits :: ExUnitsAllTypes
defaultUnits = ExUnitsAllTypes (ExUnitsPLC 0 0) (ExUnitsMSIG 0 0)

-- | Default cost model
defaultModel :: CostMod
defaultModel = CostMod (CostModPLC (ExUnitsPLC 0 0) (ExUnitsPLC 0 0))
  (CostModMSIG (ExUnitsMSIG 0 0) (ExUnitsMSIG 0 0))

-- | The formula for conversion of execution cost into Ada
-- Needs to be defined
scriptFee :: (Prices crypto) -> ExUnits -> Integer
scriptFee _ _ = 0

-- | Temporary stand-in for actual types in the "prices"
-- for Plutus script execution
data PricesPLC crypto = PricesPLC
  { -- | The types of computational resources relevant to the cost model
      stepPrice             :: Value crypto
    , memPrice              :: Value crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PricesPLC crypto)

-- | Temporary stand-in for actual types in the "prices"
-- for MSIG script execution
data PricesMSIG  crypto = PricesMSIG
  { -- | The types of computational resources relevant to the cost model
      sigPrice           :: Value crypto
    , smtPrice           :: Value crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PricesMSIG crypto)

-- | The prices for plc or msig scripts
data Prices crypto = Prices
  {
      pricesPLC            :: PricesPLC  crypto
    , pricesMSIG           :: PricesMSIG crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (Prices crypto)

-- | All types of ex units limit
data ExUnitsAllTypes = ExUnitsAllTypes
  {
      exUnitsPLC            :: ExUnitsPLC
    , exUnitsMSIG           :: ExUnitsMSIG
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ExUnitsAllTypes

-- | Default cost model
defaultPrices :: Prices crypto
defaultPrices = Prices (PricesPLC 0 0) (PricesMSIG 0 0)


-- | CBOR temp

instance ToCBOR ExUnits
 where
   toCBOR = \case
     PLCUnits exu ->
       encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR exu

     MSIGUnits exu ->
       encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR exu

instance FromCBOR ExUnits
 where
   fromCBOR = do
     n <- decodeListLen
     decodeWord >>= \case
       0 -> do
         matchSize "PLCUnits" 1 n
         a <- fromCBOR
         pure $ PLCUnits a
       1 -> do
         matchSize "MSIGUnits" 1 n
         a <- fromCBOR
         pure $ MSIGUnits a
       k -> invalidKey k


instance FromCBOR ExUnitsMSIG
 where
   fromCBOR =
     pure $ (ExUnitsMSIG 0 0)

instance FromCBOR ExUnitsPLC
 where
   fromCBOR =
     pure $ (ExUnitsPLC 0 0)

instance ToCBOR ExUnitsMSIG
 where
   toCBOR exu =
     encodeListLen 2
       <> toCBOR (numSigs exu)
       <> toCBOR (maybeSomething exu)

instance ToCBOR ExUnitsPLC
 where
   toCBOR exu =
     encodeListLen 2
       <> toCBOR (reductionSteps exu)
       <> toCBOR (memoryUnits exu)

instance FromCBOR CostModMSIG
 where
   fromCBOR =
     pure $ (CostModMSIG (ExUnitsMSIG 0 0) (ExUnitsMSIG 0 0))

instance FromCBOR CostModPLC
 where
   fromCBOR =
     pure $ (CostModPLC (ExUnitsPLC 0 0) (ExUnitsPLC 0 0))


instance ToCBOR CostModMSIG
 where
   toCBOR cm =
     encodeListLen 2
       <> toCBOR (sigPrim cm)
       <> toCBOR (smtPrim cm)

instance ToCBOR CostModPLC
 where
   toCBOR cm =
     encodeListLen 2
       <> toCBOR (stepPrim cm)
       <> toCBOR (memPrim cm)

instance ToCBOR CostMod
 where
   toCBOR cm =
     encodeListLen 2
       <> toCBOR (costModPLC cm)
       <> toCBOR (costModMSIG cm)

instance FromCBOR CostMod
 where
   fromCBOR = do
     enforceSize "CostMod" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ CostMod a b

instance
  (Crypto crypto)
  => ToCBOR (Prices crypto)
 where
   toCBOR pr =
     encodeListLen 2
       <> toCBOR (pricesPLC pr)
       <> toCBOR (pricesMSIG pr)

instance
  (Crypto crypto)
  => ToCBOR (PricesMSIG crypto)
 where
   toCBOR pr =
     encodeListLen 2
       <> toCBOR (sigPrice pr)
       <> toCBOR (smtPrice pr)

instance
  (Crypto crypto)
  => ToCBOR (PricesPLC crypto)
 where
   toCBOR pr =
     encodeListLen 2
       <> toCBOR (stepPrice pr)
       <> toCBOR (memPrice pr)

instance
  (Crypto crypto)
  => FromCBOR (PricesMSIG crypto)
 where
   fromCBOR = do
     enforceSize "PricesMSIG" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ PricesMSIG a b

instance
  (Crypto crypto)
  => FromCBOR (PricesPLC crypto)
 where
   fromCBOR = do
     enforceSize "PricesPLC" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ PricesPLC a b

instance
  (Crypto crypto)
  => FromCBOR (Prices crypto)
 where
   fromCBOR = do
     enforceSize "Prices" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ Prices a b

instance ToCBOR ExUnitsAllTypes
 where
   toCBOR ex =
     encodeListLen 2
       <> toCBOR (exUnitsPLC ex)
       <> toCBOR (exUnitsMSIG ex)

instance FromCBOR ExUnitsAllTypes
 where
   fromCBOR = do
     enforceSize "ExUnitsAllTypes" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ ExUnitsAllTypes a b
