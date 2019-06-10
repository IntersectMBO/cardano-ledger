{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Common.KeyHash
  ( KeyHash
  , hashKey
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSONKey, ToJSONKey)
import Formatting (formatToString)
import Formatting.Buildable (Buildable)
import Text.JSON.Canonical
  (FromObjectKey(..), JSValue(..), ToObjectKey(..), toJSString)

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Chain.Common.AddressHash
import Cardano.Crypto (decodeAbstractHash, hashHexF)
import Cardano.Crypto.Signing (VerificationKey)


-- | A 'KeyHash' refers to a 'VerificationKey'
newtype KeyHash = KeyHash
  { unKeyHash :: AddressHash VerificationKey
  } deriving ( Eq
             , Ord
             , Show
             , NFData
             , Buildable
             , FromJSONKey
             , ToJSONKey
             , FromCBOR
             , ToCBOR
             , HeapWords
             )

instance Monad m => ToObjectKey m KeyHash where
  toObjectKey = pure . toJSString . formatToString hashHexF . unKeyHash

instance MonadError SchemaError m => FromObjectKey m KeyHash where
  fromObjectKey = fmap (Just . KeyHash)
    . parseJSString decodeAbstractHash
    . JSString

hashKey :: VerificationKey -> KeyHash
hashKey = KeyHash . addressHash
