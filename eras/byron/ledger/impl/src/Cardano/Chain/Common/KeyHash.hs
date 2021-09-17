{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Common.KeyHash
  ( KeyHash (..),
    hashKey,
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Chain.Common.AddressHash
import Cardano.Crypto (decodeAbstractHash, hashHexF)
import Cardano.Crypto.Signing (VerificationKey)
import Cardano.Prelude
import Formatting (formatToString)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical
  ( FromObjectKey (..),
    JSValue (..),
    ToObjectKey (..),
    toJSString,
  )

-- | A 'KeyHash' refers to a 'VerificationKey'
newtype KeyHash = KeyHash
  { unKeyHash :: AddressHash VerificationKey
  }
  deriving
    ( Eq,
      Ord,
      Show,
      NFData,
      Buildable,
      FromCBOR,
      ToCBOR,
      HeapWords,
      NoThunks
    )

instance Monad m => ToObjectKey m KeyHash where
  toObjectKey = pure . toJSString . formatToString hashHexF . unKeyHash

instance MonadError SchemaError m => FromObjectKey m KeyHash where
  fromObjectKey =
    fmap (Just . KeyHash)
      . parseJSString decodeAbstractHash
      . JSString

hashKey :: VerificationKey -> KeyHash
hashKey = KeyHash . addressHash
