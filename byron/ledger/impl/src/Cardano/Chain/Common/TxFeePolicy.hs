{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Common.TxFeePolicy
  ( TxFeePolicy (..),
  )
where

import Cardano.Binary
  ( DecoderError (DecoderErrorUnknownTag),
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Cardano.Chain.Common.CBOR
  ( decodeKnownCborDataItem,
    encodeKnownCborDataItem,
  )
import Cardano.Chain.Common.Lovelace
  ( Lovelace,
    LovelaceError,
    lovelaceToInteger,
    mkLovelace,
  )
import Cardano.Chain.Common.TxSizeLinear (TxSizeLinear (..))
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Formatting (bprint, build, formatToString)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical
  ( FromJSON (..),
    ToJSON (..),
    expected,
    fromJSField,
    mkObject,
  )

-- | Transaction fee policy represents a formula to compute the minimal allowed
--   Fee for a transaction. Transactions with lesser fees won't be accepted. The
--   Minimal fee may depend on the properties of a transaction (for example, its
--   Size in bytes), so the policy can't be represented simply as a number.
--
--   Recall that a transaction fee is the difference between the sum of its
--   Inputs and the sum of its outputs. The transaction is accepted when
--   @minimal_fee(tx) <= fee(tx)@, where @minimal_fee@ is the function defined
--   By the policy.
--
--   The policy can change during the lifetime of the blockchain (using the
--   Update mechanism). At the moment we have just one policy type (a linear
--   Equation on the transaction size), but in the future other policies may Be
--   added. To make this future-proof, we also have an "unknown" policy used By
--   older node versions (the ones that haven't updated yet).
data TxFeePolicy
  = TxFeePolicyTxSizeLinear !TxSizeLinear
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable TxFeePolicy where
  build (TxFeePolicyTxSizeLinear tsp) =
    bprint ("policy(tx-size-linear): " . build) tsp

-- Used for debugging purposes only
instance Aeson.ToJSON TxFeePolicy

instance ToCBOR TxFeePolicy where
  toCBOR policy = case policy of
    TxFeePolicyTxSizeLinear txSizeLinear ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> encodeKnownCborDataItem txSizeLinear

instance FromCBOR TxFeePolicy where
  fromCBOR = do
    enforceSize "TxFeePolicy" 2
    tag <- fromCBOR @Word8
    case tag of
      0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
      _ -> cborError $ DecoderErrorUnknownTag "TxFeePolicy" tag

instance Monad m => ToJSON m TxFeePolicy where
  -- We multiply by 1e9 to keep compatibility with 'Nano' coefficients
  toJSON (TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)) =
    mkObject
      [ ("summand", toJSON $ 1e9 * lovelaceToInteger summand),
        ("multiplier", toJSON (floor $ 1e9 * multiplier :: Integer))
      ]

instance MonadError SchemaError m => FromJSON m TxFeePolicy where
  -- We div by 1e9 to keep compatibility with 'Nano' coefficients
  fromJSON obj = do
    summand <-
      wrapLovelaceError . mkLovelace . (`div` 1e9)
        =<< fromJSField
          obj
          "summand"
    multiplier <-
      (% 1e9)
        <$> fromJSField
          obj
          "multiplier"
    return $ TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)
    where
      wrapLovelaceError :: Either LovelaceError Lovelace -> m Lovelace
      wrapLovelaceError =
        either (expected "Lovelace" . Just . formatToString build) pure
