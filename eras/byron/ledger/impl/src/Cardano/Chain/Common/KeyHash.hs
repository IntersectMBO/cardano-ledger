{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Common.KeyHash (
  KeyHash (..),
  hashKey,
)
where

import Cardano.Chain.Common.AddressHash
import Cardano.Crypto (decodeAbstractHash, hashHexF)
import Cardano.Crypto.Signing (VerificationKey)
import Cardano.HeapWords (HeapWords)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Prelude
import Formatting (formatToString)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (
  FromObjectKey (..),
  JSValue (..),
  ToObjectKey (..),
  toJSString,
 )

-- | A 'KeyHash' refers to a 'VerificationKey'
newtype KeyHash = KeyHash
  { unKeyHash :: AddressHash VerificationKey
  }
  deriving
    ( Eq
    , Ord
    , Show
    , NFData
    , Buildable
    , DecCBOR
    , EncCBOR
    , HeapWords
    , NoThunks
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
