{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Plutus.ToPlutusData where

import Cardano.Ledger.BaseTypes (
  BoundedRational (boundRational, unboundRational),
  EpochInterval (..),
  NonNegativeInterval,
  ProtVer (..),
  UnitInterval,
 )
import Cardano.Ledger.Binary.Version (Version, getVersion, mkVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus.CostModels (
  CostModels,
  flattenCostModels,
  mkCostModelsLenient,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Word
import GHC.Real (Ratio ((:%)))
import Numeric.Natural (Natural)
import PlutusLedgerApi.Common (Data (..))

-- ===============================================================

-- ToPlutusData class, and instances for parameterized data types List, Map.

class ToPlutusData x where
  toPlutusData :: x -> Data
  fromPlutusData :: Data -> Maybe x
  fromPlutusData _ = Nothing

instance ToPlutusData a => ToPlutusData [a] where
  toPlutusData xs = List (map toPlutusData xs)
  fromPlutusData (List xs) = mapM fromPlutusData xs
  fromPlutusData _ = Nothing

instance (Ord a, ToPlutusData a, ToPlutusData b) => ToPlutusData (Map a b) where
  toPlutusData m = Map $ map (\(a, b) -> (toPlutusData a, toPlutusData b)) (Map.toAscList m)
  fromPlutusData (Map pairs) =
    Map.fromList
      <$> mapM (\(k, v) -> (,) <$> fromPlutusData k <*> fromPlutusData v) pairs
  fromPlutusData _ = Nothing

-- ==========================================================
-- ToPlutusData instances for concrete types needed for PParamUpdates

instance ToPlutusData Version where
  toPlutusData v = I (getVersion @Integer v)
  fromPlutusData (I n) = mkVersion @Integer n
  fromPlutusData _ = Nothing

instance ToPlutusData ProtVer where
  toPlutusData pv = List [toPlutusData (pvMajor pv), toPlutusData (pvMinor pv)]
  fromPlutusData (List [major, minor]) = ProtVer <$> fromPlutusData major <*> fromPlutusData minor
  fromPlutusData _ = Nothing

instance ToPlutusData UnitInterval where
  toPlutusData x = List [I num, I denom]
    where
      (num :% denom) = unboundRational x
  fromPlutusData (List [I num, I denom]) = boundRational (num % denom)
  fromPlutusData _ = Nothing

instance ToPlutusData NonNegativeInterval where
  toPlutusData x = List [I num, I denom]
    where
      (num :% denom) = unboundRational x
  fromPlutusData (List [I num, I denom]) = boundRational (num % denom)
  fromPlutusData _ = Nothing

instance ToPlutusData CostModels where
  toPlutusData costModels = toPlutusData $ fmap toInteger <$> flattenCostModels costModels
  fromPlutusData costModelsData = do
    costModels :: Map.Map Word8 [Integer] <- fromPlutusData costModelsData
    mkCostModelsLenient (Map.map (map fromInteger) costModels)

instance ToPlutusData ExUnits where
  toPlutusData (ExUnits a b) = List [toPlutusData a, toPlutusData b]
  fromPlutusData (List [x, y]) = ExUnits <$> fromPlutusData x <*> fromPlutusData y
  fromPlutusData _ = Nothing

instance ToPlutusData Prices where
  toPlutusData p = List [toPlutusData (prMem p), toPlutusData (prSteps p)]
  fromPlutusData (List [x, y]) = Prices <$> fromPlutusData x <*> fromPlutusData y
  fromPlutusData _ = Nothing

deriving instance ToPlutusData Coin

instance ToPlutusData Word32 where
  toPlutusData w32 = I (toInteger @Word32 w32)
  fromPlutusData (I n)
    | n >= 0 && n <= toInteger (maxBound @Word32) =
        Just $ fromInteger @Word32 n
  fromPlutusData _ = Nothing

instance ToPlutusData Word16 where
  toPlutusData w16 = I (toInteger @Word16 w16)
  fromPlutusData (I n) | n >= 0 && n <= toInteger (maxBound @Word16) = Just $ fromInteger @Word16 n
  fromPlutusData _ = Nothing

instance ToPlutusData Word8 where
  toPlutusData w8 = I (toInteger @Word8 w8)
  fromPlutusData (I n) | n >= 0 && n <= toInteger (maxBound @Word8) = Just $ fromInteger @Word8 n
  fromPlutusData _ = Nothing

deriving instance ToPlutusData EpochInterval

instance ToPlutusData Natural where
  toPlutusData nat = I (toInteger @Natural nat)
  fromPlutusData (I n) | n >= 0 = Just $ fromInteger @Natural n
  fromPlutusData _ = Nothing

instance ToPlutusData Integer where
  toPlutusData n = I n
  fromPlutusData (I n) = Just n
  fromPlutusData _ = Nothing

instance ToPlutusData Word where
  toPlutusData w = I (toInteger @Word w)
  fromPlutusData (I n) | n >= 0 && n <= toInteger (maxBound @Word) = Just $ fromInteger @Word n
  fromPlutusData _ = Nothing
