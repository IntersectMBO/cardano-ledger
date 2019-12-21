{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- This is for 'mkKnownLovelacePortion''s @n <= 45000000000000000@ constraint,
-- which is considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Chain.Common.LovelacePortion
  ( LovelacePortion(..)
  , LovelacePortionError
  , mkLovelacePortion
  , mkKnownLovelacePortion
  , lovelacePortionDenominator
  , lovelacePortionToDouble
  , rationalToLovelacePortion
  , lovelacePortionToRational
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import Formatting (bprint, build, float, int)
import qualified Formatting.Buildable as B
import GHC.TypeLits (type (<=))
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (FromCBOR(..), ToCBOR(..))


-- | LovelacePortion is some portion of Lovelace; it is interpreted as a fraction with
--   denominator of 'lovelacePortionDenominator'. The numerator must be in the
--   interval of [0, lovelacePortionDenominator].
--
--   Usually 'LovelacePortion' is used to determine some threshold expressed as
--   portion of total stake.
--
--   To multiply a lovelace portion by 'Lovelace', use 'applyLovelacePortionDown' (when
--   calculating number of lovelace) or 'applyLovelacePortionUp' (when calculating a
--   threshold).
newtype LovelacePortion = LovelacePortion
  { getLovelacePortion :: Word64
  } deriving (Show, Ord, Eq, Generic, HeapWords, NFData, NoUnexpectedThunks)

instance B.Buildable LovelacePortion where
  build cp@(getLovelacePortion -> x) = bprint
    (int . "/" . int . " (approx. " . float . ")")
    x
    lovelacePortionDenominator
    (lovelacePortionToDouble cp)

instance ToCBOR LovelacePortion where
  toCBOR = toCBOR . getLovelacePortion

instance FromCBOR LovelacePortion where
  fromCBOR = LovelacePortion <$> fromCBOR

-- The Canonical and Aeson instances for LovelacePortion are inconsistent -
-- Canonical reads/writes an integer, but Aeson reads/write a Real in range [0,1]
-- This is because 'canonical-json' only supports numbers of type @Int54@.
instance Monad m => ToJSON m LovelacePortion where
  toJSON = toJSON . getLovelacePortion

instance MonadError SchemaError m => FromJSON m LovelacePortion where
  fromJSON val = do
    number <- fromJSON val
    pure $ LovelacePortion number

-- | Denominator used by 'LovelacePortion'.
lovelacePortionDenominator :: Word64
lovelacePortionDenominator = 1e15

rationalToLovelacePortion :: Rational -> LovelacePortion
rationalToLovelacePortion r
  | r >= 0 && r <= 1 = LovelacePortion
                         (ceiling (r * toRational lovelacePortionDenominator))
  | otherwise        = panic "rationalToLovelacePortion: out of range [0..1]"

lovelacePortionToRational :: LovelacePortion -> Rational
lovelacePortionToRational (LovelacePortion n) =
  toInteger n % toInteger lovelacePortionDenominator

data LovelacePortionError
  = LovelacePortionDoubleOutOfRange Double
  | LovelacePortionTooLarge Word64
  deriving Show

instance B.Buildable LovelacePortionError where
  build = \case
    LovelacePortionDoubleOutOfRange d -> bprint
      ( "Double, "
      . build
      . " , out of range [0, 1] when constructing LovelacePortion"
      )
      d
    LovelacePortionTooLarge c -> bprint
      ("LovelacePortion, " . build . ", exceeds maximum, " . build)
      c
      lovelacePortionDenominator

-- | Constructor for 'LovelacePortion', returning 'LovelacePortionError' when @c@
--   exceeds 'lovelacePortionDenominator'
mkLovelacePortion :: Word64 -> Either LovelacePortionError LovelacePortion
mkLovelacePortion c
  | c <= lovelacePortionDenominator = Right (LovelacePortion c)
  | otherwise                       = Left (LovelacePortionTooLarge c)

-- | Construct a 'LovelacePortion' from a 'KnownNat', known to be less than
--   'lovelacePortionDenominator'
mkKnownLovelacePortion
  :: forall n . (KnownNat n, n <= 1000000000000000) => LovelacePortion
mkKnownLovelacePortion = LovelacePortion . fromIntegral . natVal $ Proxy @n

--FIXME: Use of 'Double' here is highly dubious.
lovelacePortionToDouble :: LovelacePortion -> Double
lovelacePortionToDouble (getLovelacePortion -> x) =
  realToFrac x / realToFrac lovelacePortionDenominator
{-# INLINE lovelacePortionToDouble #-}

