{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Update.BlockVersionData
       ( BlockVersionData (..)
       , isBootstrapEraBVD
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Time (NominalDiffTime)
import           Formatting (bprint, build, bytes, int, shortest, (%))
import qualified Formatting.Buildable as B
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..), fromJSField,
                     mkObject)

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Cardano.Chain.Slotting (EpochIndex, FlatSlotId, isBootstrapEra)
import           Cardano.Chain.Update.SoftforkRule


-- | Data which is associated with 'BlockVersion'
data BlockVersionData = BlockVersionData
  { bvdScriptVersion     :: !ScriptVersion
  , bvdSlotDuration      :: !NominalDiffTime
  , bvdMaxBlockSize      :: !Natural
  , bvdMaxHeaderSize     :: !Natural
  , bvdMaxTxSize         :: !Natural
  , bvdMaxProposalSize   :: !Natural
  , bvdMpcThd            :: !CoinPortion
  , bvdHeavyDelThd       :: !CoinPortion
  , bvdUpdateVoteThd     :: !CoinPortion
  , bvdUpdateProposalThd :: !CoinPortion
  , bvdUpdateImplicit    :: !FlatSlotId
  , bvdSoftforkRule      :: !SoftforkRule
  , bvdTxFeePolicy       :: !TxFeePolicy
  , bvdUnlockStakeEpoch  :: !EpochIndex
  } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionData where

instance B.Buildable BlockVersionData where
  build bvd = bprint
    ( "{ script version: " % build
    % ", slot duration: " % build
    % ", block size limit: " % bytes'
    % ", header size limit: " % bytes'
    % ", tx size limit: " % bytes'
    % ", proposal size limit: " % bytes'
    % ", mpc threshold: " % build
    % ", heavyweight delegation threshold: " % build
    % ", update vote threshold: " % build
    % ", update proposal threshold: " % build
    % ", update implicit period: " % int % " slots"
    % ", softfork rule: " % build
    % ", tx fee policy: " % build
    % ", unlock stake epoch: " % build
    % " }"
    )
    (bvdScriptVersion bvd)
    (bvdSlotDuration bvd)
    (bvdMaxBlockSize bvd)
    (bvdMaxHeaderSize bvd)
    (bvdMaxTxSize bvd)
    (bvdMaxProposalSize bvd)
    (bvdMpcThd bvd)
    (bvdHeavyDelThd bvd)
    (bvdUpdateVoteThd bvd)
    (bvdUpdateProposalThd bvd)
    (bvdUpdateImplicit bvd)
    (bvdSoftforkRule bvd)
    (bvdTxFeePolicy bvd)
    (bvdUnlockStakeEpoch bvd)
    where bytes' = bytes (shortest @Double)

instance Monad m => ToJSON m BlockVersionData where
  toJSON bvd = mkObject
    [ ("scriptVersion"    , toJSON $ bvdScriptVersion bvd)
    , ("slotDuration"     , toJSON $ bvdSlotDuration bvd)
    , ("maxBlockSize"     , toJSON $ bvdMaxBlockSize bvd)
    , ("maxHeaderSize"    , toJSON $ bvdMaxHeaderSize bvd)
    , ("maxTxSize"        , toJSON $ bvdMaxTxSize bvd)
    , ("maxProposalSize"  , toJSON $ bvdMaxProposalSize bvd)
    , ("mpcThd"           , toJSON $ bvdMpcThd bvd)
    , ("heavyDelThd"      , toJSON $ bvdHeavyDelThd bvd)
    , ("updateVoteThd"    , toJSON $ bvdUpdateVoteThd bvd)
    , ("updateProposalThd", toJSON $ bvdUpdateProposalThd bvd)
    , ("updateImplicit"   , toJSON $ bvdUpdateImplicit bvd)
    , ("softforkRule"     , toJSON $ bvdSoftforkRule bvd)
    , ("txFeePolicy"      , toJSON $ bvdTxFeePolicy bvd)
    , ("unlockStakeEpoch" , toJSON $ bvdUnlockStakeEpoch bvd)
    ]

instance MonadError SchemaError m => FromJSON m BlockVersionData where
  fromJSON obj =
    BlockVersionData
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

instance Bi BlockVersionData where
  encode bvd =
    encodeListLen 14
      <> encode (bvdScriptVersion bvd)
      <> encode (bvdSlotDuration bvd)
      <> encode (bvdMaxBlockSize bvd)
      <> encode (bvdMaxHeaderSize bvd)
      <> encode (bvdMaxTxSize bvd)
      <> encode (bvdMaxProposalSize bvd)
      <> encode (bvdMpcThd bvd)
      <> encode (bvdHeavyDelThd bvd)
      <> encode (bvdUpdateVoteThd bvd)
      <> encode (bvdUpdateProposalThd bvd)
      <> encode (bvdUpdateImplicit bvd)
      <> encode (bvdSoftforkRule bvd)
      <> encode (bvdTxFeePolicy bvd)
      <> encode (bvdUnlockStakeEpoch bvd)

  decode = do
    enforceSize "BlockVersionData" 14
    BlockVersionData
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

deriveJSON S.defaultOptions ''BlockVersionData

-- | Version of 'isBootstrapEra' which takes 'BlockVersionData' instead of
--   unlock stake epoch
isBootstrapEraBVD :: BlockVersionData -> EpochIndex -> Bool
isBootstrapEraBVD adoptedBVD = isBootstrapEra (bvdUnlockStakeEpoch adoptedBVD)
