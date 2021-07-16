{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Common.LovelacePortion
  ( LovelacePortion,
    rationalToLovelacePortion,
    lovelacePortionToRational,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude
import Control.Monad (fail)
import qualified Data.Aeson as Aeson
import Formatting (bprint, build, float, int, sformat)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Quiet
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | 'LovelacePortion' is a legacy Byron type that we keep only for
-- compatibility. It was originally intended to represent a fraction of stake
-- in the system. It is used only for the thresholds used in the update system
-- rules, most of which are now themselves unused. The remaining case is no
-- longer interpreted as a fraction of all stake, but as a fraction of the
-- number of genesis keys.
--
-- It has enormous precision, due to the fact that it was originally intended
-- to represent a fraction of all stake and can cover the precision of all the
-- Lovelace in the system.
--
-- It is represented as a rational nominator with a fixed implicit denominator
-- of 1e15. So the nominator must be in the range @[0..1e15]@. This is also the
-- representation used on-chain (in update proposals) and in the JSON
-- genesis file.
--
-- It is interpreted as a 'Rational' via the provided conversion functions.
newtype LovelacePortion = LovelacePortion
  { unLovelacePortion :: Word64
  }
  deriving (Ord, Eq, Generic, HeapWords, NFData, NoThunks)
  deriving (Show) via (Quiet LovelacePortion)

instance B.Buildable LovelacePortion where
  build cp@(LovelacePortion x) =
    bprint
      (int . "/" . int . " (approx. " . float . ")")
      x
      lovelacePortionDenominator
      (fromRational (lovelacePortionToRational cp) :: Double)

-- Used for debugging purposes only
instance Aeson.ToJSON LovelacePortion

instance ToCBOR LovelacePortion where
  toCBOR = toCBOR . unLovelacePortion

instance FromCBOR LovelacePortion where
  fromCBOR = do
    nominator <- fromCBOR
    when (nominator > lovelacePortionDenominator) $
      fail "LovelacePortion: value out of bounds [0..1e15]"
    return (LovelacePortion nominator)

-- The canonical JSON instance for LovelacePortion uses only the nominator in
-- the external representation,  rather than a real in the range [0,1].
-- This is because 'canonical-json' only supports numbers of type @Int54@.
instance Monad m => ToJSON m LovelacePortion where
  toJSON = toJSON . unLovelacePortion

instance MonadError SchemaError m => FromJSON m LovelacePortion where
  fromJSON val = do
    nominator <- fromJSON val
    when (nominator > lovelacePortionDenominator) $
      throwError
        SchemaError
          { seExpected = "LovelacePortion integer in bounds [0..1e15]",
            seActual = Just (sformat build nominator)
          }
    pure (LovelacePortion nominator)

-- | Denominator used by 'LovelacePortion'.
lovelacePortionDenominator :: Word64
lovelacePortionDenominator = 1e15

-- | Make a 'LovelacePortion' from a 'Rational'
-- which must be in the range @[0..1]@.
rationalToLovelacePortion :: Rational -> LovelacePortion
rationalToLovelacePortion r
  | r >= 0 && r <= 1 =
    LovelacePortion
      (ceiling (r * toRational lovelacePortionDenominator))
  | otherwise = panic "rationalToLovelacePortion: out of range [0..1]"

-- | Turn a 'LovelacePortion' into a 'Rational' in the range @[0..1]@.
lovelacePortionToRational :: LovelacePortion -> Rational
lovelacePortionToRational (LovelacePortion n) =
  toInteger n % toInteger lovelacePortionDenominator
