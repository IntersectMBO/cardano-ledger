{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Update.SoftforkRule
  ( SoftforkRule (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import Cardano.Chain.Common (LovelacePortion)
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..), fromJSField, mkObject)

-- | Values defining softfork resolution rule
--
--   If a proposal is confirmed at the 's'-th epoch, softfork resolution
--   threshold at the 't'-th epoch will be 'max spMinThd (spInitThd - (t - s) *
--   spThdDecrement)'.
--
--   Softfork resolution threshold is the portion of total stake such that if
--   total stake of issuers of blocks with some block version is greater than
--   this portion, this block version becomes adopted.
data SoftforkRule = SoftforkRule
  { -- | Initial threshold (right after proposal is confirmed).
    srInitThd :: !LovelacePortion,
    -- | Minimal threshold (i. e. threshold can't become less than this one).
    srMinThd :: !LovelacePortion,
    -- | Theshold will be decreased by this value after each epoch.
    srThdDecrement :: !LovelacePortion
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable SoftforkRule where
  build sr =
    bprint
      ("(init = " . build . ", min = " . build . ", decrement = " . build . ")")
      (srInitThd sr)
      (srMinThd sr)
      (srThdDecrement sr)

-- Used for debugging purposes only
instance Aeson.ToJSON SoftforkRule

instance ToCBOR SoftforkRule where
  toCBOR sr =
    encodeListLen 3 <> toCBOR (srInitThd sr) <> toCBOR (srMinThd sr)
      <> toCBOR
        (srThdDecrement sr)

instance FromCBOR SoftforkRule where
  fromCBOR = do
    enforceSize "SoftforkRule" 3
    SoftforkRule <$> fromCBOR <*> fromCBOR <*> fromCBOR

instance Monad m => ToJSON m SoftforkRule where
  toJSON sr =
    mkObject
      [ ("initThd", toJSON $ srInitThd sr),
        ("minThd", toJSON $ srMinThd sr),
        ("thdDecrement", toJSON $ srThdDecrement sr)
      ]

instance MonadError SchemaError m => FromJSON m SoftforkRule where
  fromJSON obj =
    SoftforkRule
      <$> fromJSField obj "initThd"
      <*> fromJSField obj "minThd"
      <*> fromJSField obj "thdDecrement"
