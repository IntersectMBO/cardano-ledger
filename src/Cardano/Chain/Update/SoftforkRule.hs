{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Update.SoftforkRule
       ( SoftforkRule (..)
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..), fromJSField,
                     mkObject)

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Common (CoinPortion)


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
  { srInitThd      :: !CoinPortion
  -- ^ Initial threshold (right after proposal is confirmed).
  , srMinThd       :: !CoinPortion
  -- ^ Minimal threshold (i. e. threshold can't become less than this one).
  , srThdDecrement :: !CoinPortion
  -- ^ Theshold will be decreased by this value after each epoch.
  } deriving (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance B.Buildable SoftforkRule where
  build sr = bprint
    ("(init = " % build % ", min = " % build % ", decrement = " % build % ")")
    (srInitThd sr)
    (srMinThd sr)
    (srThdDecrement sr)

instance Bi SoftforkRule where
  encode sr =
    encodeListLen 3 <> encode (srInitThd sr) <> encode (srMinThd sr) <> encode
      (srThdDecrement sr)

  decode = do
    enforceSize "SoftforkRule" 3
    SoftforkRule <$> decode <*> decode <*> decode

instance Monad m => ToJSON m SoftforkRule where
  toJSON sr = mkObject
    [ ("initThd"     , toJSON $ srInitThd sr)
    , ("minThd"      , toJSON $ srMinThd sr)
    , ("thdDecrement", toJSON $ srThdDecrement sr)
    ]

instance MonadError SchemaError m => FromJSON m SoftforkRule where
  fromJSON obj =
    SoftforkRule
      <$> fromJSField obj "initThd"
      <*> fromJSField obj "minThd"
      <*> fromJSField obj "thdDecrement"

deriveJSON S.defaultOptions ''SoftforkRule
