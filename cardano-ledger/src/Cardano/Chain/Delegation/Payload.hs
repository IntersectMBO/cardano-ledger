{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}

module Cardano.Chain.Delegation.Payload
  ( Payload( getPayload, serializePayload )
  , pattern UnsafePayload
  )
where

import Cardano.Prelude

import Formatting (bprint, int)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary
  ( Decoded(..)
  , ToCBOR(..)
  , FromCBORAnnotated (..)
  , serialize'
  , encodePreEncoded
  , withSlice'
  )
import qualified Cardano.Chain.Delegation.Certificate as Delegation


-- | The delegation 'Payload' contains a list of delegation 'Certificate's
data Payload = UnsafePayload'
  { getPayload'      :: ![Delegation.Certificate]
  , serializePayload :: ByteString
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

{-# COMPLETE UnsafePayload #-}
pattern UnsafePayload :: [Delegation.Certificate] -> Payload
pattern UnsafePayload { getPayload } <- UnsafePayload' getPayload _
  where
  UnsafePayload sks = UnsafePayload' sks (serialize' sks)

instance Decoded Payload where
  type BaseType Payload = Payload
  recoverBytes = serializePayload

instance Buildable Payload where
  build (UnsafePayload psks) = bprint
    ("proxy signing keys (" . int . " items): " . listJson . "\n")
    (length psks)
    psks

instance ToCBOR Payload where
  toCBOR = encodePreEncoded . serializePayload

instance FromCBORAnnotated Payload where
  fromCBORAnnotated' = withSlice' $ UnsafePayload' <$> fromCBORAnnotated'
