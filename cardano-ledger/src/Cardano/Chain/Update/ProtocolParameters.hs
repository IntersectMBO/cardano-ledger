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
  , upAdptThd
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

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Common
  (LovelacePortion, TxFeePolicy, lovelacePortionToDouble)
import Cardano.Chain.Slotting (EpochIndex, SlotNumber(..), isBootstrapEra)
import Cardano.Chain.Update.SoftforkRule


-- | Data which is associated with 'BlockVersion'
data ProtocolParameters = ProtocolParameters
  { ppScriptVersion     :: !Word16
  , ppSlotDuration      :: !NominalDiffTime
  , ppMaxBlockSize      :: !Natural
  , ppMaxHeaderSize     :: !Natural
  , ppMaxTxSize         :: !Natural
  , ppMaxProposalSize   :: !Natural
  , ppMpcThd            :: !LovelacePortion
  , ppHeavyDelThd       :: !LovelacePortion
  , ppUpdateVoteThd     :: !LovelacePortion
  , ppUpdateProposalThd :: !LovelacePortion
  , ppUpdateProposalTTL :: !SlotNumber
  -- ^ Time to live for a protocol update proposal. This used to be the number
  -- of slots after which the system made a decision regarding an update
  -- proposal confirmation, when a majority of votes was not reached in the
  -- given number of slots. If there were more positive than negative votes the
  -- proposal became confirmed, otherwise it was rejected. Since in the
  -- Byron-Shelley bridge we do not have negative votes, and we aim at
  -- simplifying the update mechanism, 'ppUpdateProposalTTL' is re-interpreted as
  -- the number of slots a proposal has to gather a majority of votes. If a
  -- majority of votes has not been reached before this period, then the
  -- proposal is rejected.
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
    (ppUpdateProposalTTL pp)
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
    , ("updateImplicit"   , toJSON $ ppUpdateProposalTTL pp)
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

instance ToCBOR ProtocolParameters where
  toCBOR pp =
    encodeListLen 14
      <> toCBOR (ppScriptVersion pp)
      <> toCBOR (ppSlotDuration pp)
      <> toCBOR (ppMaxBlockSize pp)
      <> toCBOR (ppMaxHeaderSize pp)
      <> toCBOR (ppMaxTxSize pp)
      <> toCBOR (ppMaxProposalSize pp)
      <> toCBOR (ppMpcThd pp)
      <> toCBOR (ppHeavyDelThd pp)
      <> toCBOR (ppUpdateVoteThd pp)
      <> toCBOR (ppUpdateProposalThd pp)
      <> toCBOR (ppUpdateProposalTTL pp)
      <> toCBOR (ppSoftforkRule pp)
      <> toCBOR (ppTxFeePolicy pp)
      <> toCBOR (ppUnlockStakeEpoch pp)

instance FromCBOR ProtocolParameters where
  fromCBOR = do
    enforceSize "ProtocolParameters" 14
    ProtocolParameters
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

deriveJSON S.defaultOptions ''ProtocolParameters

-- | Version of 'isBootstrapEra' which takes 'ProtocolParameters' instead of
--   unlock stake epoch
isBootstrapEraPP :: ProtocolParameters -> EpochIndex -> Bool
isBootstrapEraPP adoptedPP = isBootstrapEra (ppUnlockStakeEpoch adoptedPP)

-- | In Byron we do not have a @upAdptThd@ protocol parameter, so we have to
--   use the existing ones.
--
--   @lovelacePortionToDouble . srMinThd . ppSoftforkRule@ will give us the
--   ratio (in the interval @[0, 1]@) of the total stake that has to endorse a
--   protocol version to become adopted. In genesis configuration, this ratio
--   will evaluate to @0.6@, so if we have 7 genesis keys, @upAdptThd = 4@.
upAdptThd :: Word8 -> ProtocolParameters -> Int
upAdptThd numGenKeys pps = floor $ stakeRatio * fromIntegral numGenKeys
  where stakeRatio = lovelacePortionToDouble . srMinThd . ppSoftforkRule $ pps
