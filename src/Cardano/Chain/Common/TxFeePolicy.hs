{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Common.TxFeePolicy
  ( TxFeePolicy(..)
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import Data.Aeson (object, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Formatting (bprint, build, formatToString, shown)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  (FromJSON(..), ToJSON(..), expected, fromJSField, mkObject)

import Cardano.Binary.Class
  ( Bi(..)
  , decodeKnownCborDataItem
  , decodeUnknownCborDataItem
  , encodeKnownCborDataItem
  , encodeListLen
  , encodeUnknownCborDataItem
  , enforceSize
  )
import Cardano.Chain.Common.Lovelace
  (Lovelace, LovelaceError, lovelaceToInteger, mkLovelace)
import Cardano.Chain.Common.TxSizeLinear (TxSizeLinear(..))


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
    | TxFeePolicyUnknown !Word8 !ByteString
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass NFData

instance B.Buildable TxFeePolicy where
    build (TxFeePolicyTxSizeLinear tsp) =
        bprint ("policy(tx-size-linear): ".build) tsp
    build (TxFeePolicyUnknown v bs) =
        bprint ("policy(unknown:".build."): ".shown) v bs

instance Bi TxFeePolicy where
    encode policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            encodeListLen 2
                <> encode (0 :: Word8)
                <> encodeKnownCborDataItem txSizeLinear
        TxFeePolicyUnknown word8 bs ->
            encodeListLen 2 <> encode word8 <> encodeUnknownCborDataItem
                (LBS.fromStrict bs)

    decode = do
        enforceSize "TxFeePolicy" 2
        tag <- decode @Word8
        case tag of
            0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
            _ -> TxFeePolicyUnknown tag <$> decodeUnknownCborDataItem

instance Monad m => ToJSON m TxFeePolicy where
  -- We multiply by 1e9 to keep compatibility with 'Nano' coefficients
  toJSON (TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)) = mkObject
    [ ("summand"   , toJSON $ 1e9 * lovelaceToInteger summand)
    , ("multiplier", toJSON $ 1e9 * lovelaceToInteger multiplier)
    ]
  toJSON (TxFeePolicyUnknown{}) =
    panic "Having TxFeePolicyUnknown in genesis is likely a bug"

instance MonadError SchemaError m => FromJSON m TxFeePolicy where
  -- We div by 1e9 to keep compatibility with 'Nano' coefficients
  fromJSON obj = do
    summand <- wrapLovelaceError . mkLovelace . (`div` 1e9) =<< fromJSField
      obj
      "summand"
    multiplier <- wrapLovelaceError . mkLovelace . (`div` 1e9) =<< fromJSField
      obj
      "multiplier"
    return $ TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)
   where
    wrapLovelaceError :: Either LovelaceError Lovelace -> m Lovelace
    wrapLovelaceError =
      either (expected "Lovelace" . Just . formatToString build) pure

instance Aeson.ToJSON TxFeePolicy where
    toJSON = object . \case
        TxFeePolicyTxSizeLinear linear -> ["txSizeLinear" .= linear]
        TxFeePolicyUnknown policyTag policyPayload ->
            ["unknown" .= (policyTag, decodeUtf8 policyPayload)]

instance Aeson.FromJSON TxFeePolicy where
    parseJSON = Aeson.withObject "TxFeePolicy" $ \o -> do
        mLinear <- o .:? "txSizeLinear"
        mUnknown <- o .:? "unknown"
        toAesonError @Text $ case (mLinear, mUnknown) of
            (Nothing, Nothing)     -> Left "TxFeePolicy: none provided"
            (Just linear, Nothing) -> Right $ TxFeePolicyTxSizeLinear linear
            (Nothing, Just (tag, payload)) -> Right $ TxFeePolicyUnknown
                tag
                (encodeUtf8 payload)
            _ -> Left "TxFeePolicy: ambiguous choice"
