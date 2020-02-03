{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module CostModel
    (
    , ExUnits(..)
    , CostMod(..)
    , Prices(..)
    , defaultUnits
    , defaultModel
    , defaultPrices
    , scriptFee
    ) where


import           Cardano.Prelude (NoUnexpectedThunks(..), fmap, foldr)
import           GHC.Generics (Generic)
import           Data.Word (Word8)
import           Cardano.Binary (Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, decodeWord,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen, encodeWord,
                     enforceSize, matchSize)
import           BaseTypes (CborSeq (..), UnitInterval, invalidKey)
import           Data.Map.Strict (Map, empty)

import           Cardano.Ledger.Shelley.Crypto
import           Scripts
import           Coin

-- | comparing required resources for Plutus scripts
instance Ord ExUnits where
   (<=) (ExUnits rs1 mu1) (ExUnits rs2 mu2) =
     ((<=) rs1 rs2) && ((<=) mu1 mu2)


-- | Temporary stand-in for actual types in the execution cost
-- for Plutus script execution
data ExUnits = ExUnits
  { -- | The types of computational resources relevant to the cost model
      reductionSteps           :: Integer
    , memoryUnits              :: Integer
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ExUnits

-- | Temporary stand-in for actual types in the cost model
-- for Plutus script execution
data CostMod = CostMod
  { -- | The types of computational resources relevant to the cost model
    , memPrim                :: ExUnits
    , stepPrim               :: ExUnits
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks CostMod

-- | Default values
-- | Default execution units
defaultUnits :: ExUnitsAllTypes
defaultUnits = ExUnits 0 0

-- | Default cost model
defaultModel :: CostMod
defaultModel = CostMod (ExUnits 0 0) (ExUnits 0 0)


-- | Prices for Plutus script execution
data Prices = Prices
  { -- | The types of computational resources relevant to the cost model
    initPrim               :: Coin
  , memPrim                :: Coin
  , stepPrim               :: Coin
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks Prices


-- | Default cost model
defaultPrices :: Prices
defaultPrices = Prices (Coin 0) (Coin 0) (Coin 0)

-- | The formula for conversion of execution cost into Ada
-- Needs to be defined TODO
scriptFee :: Prices -> ExUnits -> Coin
scriptFee (Prices (Coin i) (Coin m) (Coin s)) (ExUnits mu su) =
  Coin (i + m*mu + s*su)


-- | CBOR temp

instance ToCBOR ExUnits
 where
   toCBOR = \case
     PLCUnits exu ->
       encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR exu

     MSigUnits exu ->
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
         matchSize "MSigUnits" 1 n
         a <- fromCBOR
         pure $ MSigUnits a
       k -> invalidKey k


instance FromCBOR ExUnitsMSig
 where
   fromCBOR = do
     enforceSize "ExUnitsMSig" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ ExUnitsMSig a b

instance FromCBOR ExUnitsPLC
 where
   fromCBOR = do
     enforceSize "ExUnitsPLC" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ ExUnitsPLC a b

instance ToCBOR ExUnitsMSig
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

instance FromCBOR CostModMSig
 where
   fromCBOR = do
     enforceSize "CostModMSig" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ CostModMSig a b

instance FromCBOR CostModPLC
 where
   fromCBOR = do
     enforceSize "CostModPLC" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ CostModPLC a b


instance ToCBOR CostModMSig
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
       <> toCBOR (costModMSig cm)

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
       <> toCBOR (pricesMSig pr)

instance
  (Crypto crypto)
  => ToCBOR (PricesMSig crypto)
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
  => FromCBOR (PricesMSig crypto)
 where
   fromCBOR = do
     enforceSize "PricesMSig" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ PricesMSig a b

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
       <> toCBOR (exUnitsMSig ex)

instance FromCBOR ExUnitsAllTypes
 where
   fromCBOR = do
     enforceSize "ExUnitsAllTypes" 2
     a <- fromCBOR
     b <- fromCBOR
     pure $ ExUnitsAllTypes a b
