{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Common.TxSizeLinear
  ( TxSizeLinear(..)
  , txSizeLinearMinValue
  , calculateTxSizeLinear
  )
where

import Cardano.Prelude

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Fixed (Nano)
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  (Bi(..), encodeListLen, enforceSize, DecoderError(..), Decoder)
import Cardano.Chain.Common.Lovelace
  ( Lovelace
  , mkLovelace
  , LovelaceError
  , addLovelace
  , scaleLovelace
  , unsafeGetLovelace
  )


-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear =
  TxSizeLinear !Lovelace !Lovelace
  deriving (Eq, Ord, Show, Generic)

instance NFData TxSizeLinear

instance B.Buildable TxSizeLinear where
  build (TxSizeLinear a b) = bprint (build . " + " . build . "*s") a b

instance Bi TxSizeLinear where
  -- We encode as 'Nano' for backwards compatibility
  encode (TxSizeLinear a b) =
    encodeListLen 2
      <> encode (fromIntegral (unsafeGetLovelace a) :: Nano)
      <> encode (fromIntegral (unsafeGetLovelace b) :: Nano)

  decode = do
    enforceSize "TxSizeLinear" 2
    !a <- wrapLovelaceError . mkLovelace . round =<< decode @Nano
    !b <- wrapLovelaceError . mkLovelace . round =<< decode @Nano
    return $ TxSizeLinear a b
   where
    wrapLovelaceError :: Either LovelaceError Lovelace -> Decoder s Lovelace
    wrapLovelaceError =
      toCborError . first (DecoderErrorCustom "TxSizeLinear" . sformat build)

instance Aeson.ToJSON TxSizeLinear where
  toJSON (TxSizeLinear a b) = object ["a" .= a, "b" .= b]

instance Aeson.FromJSON TxSizeLinear where
  parseJSON = Aeson.withObject "TxSizeLinear"
      $ \o -> TxSizeLinear <$> (o Aeson..: "a") <*> (o Aeson..: "b")

calculateTxSizeLinear
  :: TxSizeLinear -> Natural -> Either LovelaceError Lovelace
calculateTxSizeLinear (TxSizeLinear a b) = addLovelace a <=< scaleLovelace b

txSizeLinearMinValue :: TxSizeLinear -> Lovelace
txSizeLinearMinValue (TxSizeLinear a _) = a
