{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Common.TxSizeLinear
  ( TxSizeLinear (..),
    txSizeLinearMinValue,
    calculateTxSizeLinear,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Cardano.Chain.Common.Lovelace
  ( Lovelace,
    LovelaceError,
    addLovelace,
    integerToLovelace,
    mkLovelace,
    scaleLovelaceRationalUp,
    unsafeGetLovelace,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Data.Fixed (Nano)
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear
  = TxSizeLinear !Lovelace !Rational
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable TxSizeLinear where
  build (TxSizeLinear a b) = bprint (build . " + " . build . "*s") a b

-- Used for debugging purposes only
instance ToJSON TxSizeLinear

instance ToCBOR TxSizeLinear where
  -- We encode as 'Nano' for backwards compatibility
  toCBOR (TxSizeLinear a b) =
    encodeListLen 2
      <> toCBOR (fromIntegral (unsafeGetLovelace a) :: Nano)
      <> toCBOR (fromRational b :: Nano)

instance FromCBOR TxSizeLinear where
  fromCBOR = do
    enforceSize "TxSizeLinear" 2
    !a <- wrapLovelaceError . mkLovelace . round =<< fromCBOR @Nano
    !b <- toRational <$> fromCBOR @Nano
    return $ TxSizeLinear a b
    where
      wrapLovelaceError :: Either LovelaceError Lovelace -> Decoder s Lovelace
      wrapLovelaceError =
        toCborError . first (DecoderErrorCustom "TxSizeLinear" . sformat build)

calculateTxSizeLinear ::
  TxSizeLinear -> Natural -> Either LovelaceError Lovelace
calculateTxSizeLinear (TxSizeLinear a b) sz =
  addLovelace a
    =<< flip scaleLovelaceRationalUp b
    <$> integerToLovelace (fromIntegral sz)

txSizeLinearMinValue :: TxSizeLinear -> Lovelace
txSizeLinearMinValue (TxSizeLinear a _) = a
