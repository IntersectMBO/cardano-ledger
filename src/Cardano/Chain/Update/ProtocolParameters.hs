{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters(..)
  , isBootstrapEraPP
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.Aeson.Options as S (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time (NominalDiffTime)
import Formatting (Format, bprint, build, bytes, shortest)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..), fromJSField, mkObject)

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common (LovelacePortion, ScriptVersion, TxFeePolicy)
import Cardano.Chain.Slotting (EpochIndex, FlatSlotId(..), isBootstrapEra)
import Cardano.Chain.Update.SoftforkRule


-- | Data which is associated with 'BlockVersion'
data ProtocolParameters = ProtocolParameters
  { ppScriptVersion     :: !ScriptVersion
  , ppSlotDuration      :: !NominalDiffTime
  , ppMaxBlockSize      :: !Natural
  , ppMaxHeaderSize     :: !Natural
  , ppMaxTxSize         :: !Natural
  , ppMaxProposalSize   :: !Natural
  , ppMpcThd            :: !LovelacePortion
  , ppHeavyDelThd       :: !LovelacePortion
  , ppUpdateVoteThd     :: !LovelacePortion
  , ppUpdateProposalThd :: !LovelacePortion
  , ppUpdateImplicit    :: !FlatSlotId
  , ppSoftforkRule      :: !SoftforkRule
  , ppTxFeePolicy       :: !TxFeePolicy
  , ppUnlockStakeEpoch  :: !EpochIndex
  } deriving (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance B.Buildable ProtocolParameters where
  build pp = bprint
    ( "{ script version: " . build
    . ", slot duration: " . build
    . ", block size limit: " . bytes'
    . ", header size limit: " . bytes'
    . ", tx size limit: " . bytes'
    . ", proposal size limit: " . bytes'
    . ", mpc threshold: " . build
    . ", heavyweight delegation threshold: " . build
    . ", update vote threshold: " . build
    . ", update proposal threshold: " . build
    . ", update implicit period: " . build . " slots"
    . ", softfork rule: " . build
    . ", tx fee policy: " . build
    . ", unlock stake epoch: " . build
    . " }"
    )
    (ppScriptVersion pp)
    (ppSlotDuration pp)
    (ppMaxBlockSize pp)
    (ppMaxHeaderSize pp)
    (ppMaxTxSize pp)
    (ppMaxProposalSize pp)
    (ppMpcThd pp)
    (ppHeavyDelThd pp)
    (ppUpdateVoteThd pp)
    (ppUpdateProposalThd pp)
    (ppUpdateImplicit pp)
    (ppSoftforkRule pp)
    (ppTxFeePolicy pp)
    (ppUnlockStakeEpoch pp)
    where
     bytes' :: Format r (Natural -> r)
     bytes' = bytes (shortest @Double)

instance Monad m => ToJSON m ProtocolParameters where
  toJSON pp = mkObject
    [ ("scriptVersion"    , toJSON $ ppScriptVersion pp)
    , ("slotDuration"     , toJSON $ ppSlotDuration pp)
    , ("maxBlockSize"     , toJSON $ ppMaxBlockSize pp)
    , ("maxHeaderSize"    , toJSON $ ppMaxHeaderSize pp)
    , ("maxTxSize"        , toJSON $ ppMaxTxSize pp)
    , ("maxProposalSize"  , toJSON $ ppMaxProposalSize pp)
    , ("mpcThd"           , toJSON $ ppMpcThd pp)
    , ("heavyDelThd"      , toJSON $ ppHeavyDelThd pp)
    , ("updateVoteThd"    , toJSON $ ppUpdateVoteThd pp)
    , ("updateProposalThd", toJSON $ ppUpdateProposalThd pp)
    , ("updateImplicit"   , toJSON $ ppUpdateImplicit pp)
    , ("softforkRule"     , toJSON $ ppSoftforkRule pp)
    , ("txFeePolicy"      , toJSON $ ppTxFeePolicy pp)
    , ("unlockStakeEpoch" , toJSON $ ppUnlockStakeEpoch pp)
    ]

instance MonadError SchemaError m => FromJSON m ProtocolParameters where
  fromJSON obj =
    ProtocolParameters
      <$> fromJSField obj "scriptVersion"
      <*> fromJSField obj "slotDuration"
      <*> fromJSField obj "maxBlockSize"
      <*> fromJSField obj "maxHeaderSize"
      <*> fromJSField obj "maxTxSize"
      <*> fromJSField obj "maxProposalSize"
      <*> fromJSField obj "mpcThd"
      <*> fromJSField obj "heavyDelThd"
      <*> fromJSField obj "updateVoteThd"
      <*> fromJSField obj "updateProposalThd"
      <*> fromJSField obj "updateImplicit"
      <*> fromJSField obj "softforkRule"
      <*> fromJSField obj "txFeePolicy"
      <*> fromJSField obj "unlockStakeEpoch"

instance Bi ProtocolParameters where
  encode pp =
    encodeListLen 14
      <> encode (ppScriptVersion pp)
      <> encode (ppSlotDuration pp)
      <> encode (ppMaxBlockSize pp)
      <> encode (ppMaxHeaderSize pp)
      <> encode (ppMaxTxSize pp)
      <> encode (ppMaxProposalSize pp)
      <> encode (ppMpcThd pp)
      <> encode (ppHeavyDelThd pp)
      <> encode (ppUpdateVoteThd pp)
      <> encode (ppUpdateProposalThd pp)
      <> encode (ppUpdateImplicit pp)
      <> encode (ppSoftforkRule pp)
      <> encode (ppTxFeePolicy pp)
      <> encode (ppUnlockStakeEpoch pp)

  decode = do
    enforceSize "ProtocolParameters" 14
    ProtocolParameters
      <$> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode

deriveJSON S.defaultOptions ''ProtocolParameters

-- | Version of 'isBootstrapEra' which takes 'ProtocolParameters' instead of
--   unlock stake epoch
isBootstrapEraPP :: ProtocolParameters -> EpochIndex -> Bool
isBootstrapEraPP adoptedPP = isBootstrapEra (ppUnlockStakeEpoch adoptedPP)
