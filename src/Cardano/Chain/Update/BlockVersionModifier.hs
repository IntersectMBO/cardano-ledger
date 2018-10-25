{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.Update.BlockVersionModifier
       ( BlockVersionModifier (..)
       , applyBVM
       ) where

import           Cardano.Prelude

import           Data.Text.Lazy.Builder (Builder)
import           Data.Time (NominalDiffTime)
import           Formatting (Format, bprint, build, bytes, int, later, shortest)
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Cardano.Chain.Slotting (EpochIndex, FlatSlotId)
import           Cardano.Chain.Update.BlockVersionData (BlockVersionData (..))
import           Cardano.Chain.Update.SoftforkRule (SoftforkRule)


-- | Data which represents modifications of block (aka protocol) version
data BlockVersionModifier = BlockVersionModifier
  { bvmScriptVersion     :: !(Maybe ScriptVersion)
  , bvmSlotDuration      :: !(Maybe NominalDiffTime)
  , bvmMaxBlockSize      :: !(Maybe Natural)
  , bvmMaxHeaderSize     :: !(Maybe Natural)
  , bvmMaxTxSize         :: !(Maybe Natural)
  , bvmMaxProposalSize   :: !(Maybe Natural)
  , bvmMpcThd            :: !(Maybe CoinPortion)
  , bvmHeavyDelThd       :: !(Maybe CoinPortion)
  , bvmUpdateVoteThd     :: !(Maybe CoinPortion)
  , bvmUpdateProposalThd :: !(Maybe CoinPortion)
  , bvmUpdateImplicit    :: !(Maybe FlatSlotId)
  , bvmSoftforkRule      :: !(Maybe SoftforkRule)
  , bvmTxFeePolicy       :: !(Maybe TxFeePolicy)
  , bvmUnlockStakeEpoch  :: !(Maybe EpochIndex)
  } deriving (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance B.Buildable BlockVersionModifier where
  build bvm = bprint
    ( "{ script version: " . bmodifier build
    . ", slot duration: " . bmodifier build
    . ", block size limit: " . bmodifier bytes'
    . ", header size limit: " . bmodifier bytes'
    . ", tx size limit: " . bmodifier bytes'
    . ", proposal size limit: " . bmodifier bytes'
    . ", mpc threshold: " . bmodifier build
    . ", heavyweight delegation threshold: " . bmodifier build
    . ", update vote threshold: " . bmodifier build
    . ", update proposal threshold: " . bmodifier build
    . ", update implicit period (slots): " . bmodifier int
    . ", softfork rule: " . bmodifier build
    . ", tx fee policy: " . bmodifier build
    . ", unlock stake epoch: " . bmodifier build
    . " }"
    )
    (bvmScriptVersion bvm)
    (bvmSlotDuration bvm)
    (bvmMaxBlockSize bvm)
    (bvmMaxHeaderSize bvm)
    (bvmMaxTxSize bvm)
    (bvmMaxProposalSize bvm)
    (bvmMpcThd bvm)
    (bvmHeavyDelThd bvm)
    (bvmUpdateVoteThd bvm)
    (bvmUpdateProposalThd bvm)
    (bvmUpdateImplicit bvm)
    (bvmSoftforkRule bvm)
    (bvmTxFeePolicy bvm)
    (bvmUnlockStakeEpoch bvm)
   where
    bmodifier :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
    bmodifier b = later $ maybe "no change" (bprint b)

    bytes' :: Format r (Natural -> r)
    bytes' = bytes (shortest @Double)

instance Bi BlockVersionModifier where
  encode bvm =
    encodeListLen 14
      <> encode (bvmScriptVersion bvm)
      <> encode (bvmSlotDuration bvm)
      <> encode (bvmMaxBlockSize bvm)
      <> encode (bvmMaxHeaderSize bvm)
      <> encode (bvmMaxTxSize bvm)
      <> encode (bvmMaxProposalSize bvm)
      <> encode (bvmMpcThd bvm)
      <> encode (bvmHeavyDelThd bvm)
      <> encode (bvmUpdateVoteThd bvm)
      <> encode (bvmUpdateProposalThd bvm)
      <> encode (bvmUpdateImplicit bvm)
      <> encode (bvmSoftforkRule bvm)
      <> encode (bvmTxFeePolicy bvm)
      <> encode (bvmUnlockStakeEpoch bvm)

  decode = do
    enforceSize "BlockVersionModifier" 14
    BlockVersionModifier
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

-- | Apply 'BlockVersionModifier' to 'BlockVersionData'
applyBVM :: BlockVersionModifier -> BlockVersionData -> BlockVersionData
applyBVM bvm bvd = BlockVersionData
  { bvdScriptVersion = fromMaybe (bvdScriptVersion bvd) (bvmScriptVersion bvm)
  , bvdSlotDuration      = fromMaybe (bvdSlotDuration bvd) (bvmSlotDuration bvm)
  , bvdMaxBlockSize      = fromMaybe (bvdMaxBlockSize bvd) (bvmMaxBlockSize bvm)
  , bvdMaxHeaderSize = fromMaybe (bvdMaxHeaderSize bvd) (bvmMaxHeaderSize bvm)
  , bvdMaxTxSize         = fromMaybe (bvdMaxTxSize bvd) (bvmMaxTxSize bvm)
  , bvdMaxProposalSize   = fromMaybe
    (bvdMaxProposalSize bvd)
    (bvmMaxProposalSize bvm)
  , bvdMpcThd            = fromMaybe (bvdMpcThd bvd) (bvmMpcThd bvm)
  , bvdHeavyDelThd       = fromMaybe (bvdHeavyDelThd bvd) (bvmHeavyDelThd bvm)
  , bvdUpdateVoteThd = fromMaybe (bvdUpdateVoteThd bvd) (bvmUpdateVoteThd bvm)
  , bvdUpdateProposalThd = fromMaybe
    (bvdUpdateProposalThd bvd)
    (bvmUpdateProposalThd bvm)
  , bvdUpdateImplicit    = fromMaybe
    (bvdUpdateImplicit bvd)
    (bvmUpdateImplicit bvm)
  , bvdSoftforkRule      = fromMaybe (bvdSoftforkRule bvd) (bvmSoftforkRule bvm)
  , bvdTxFeePolicy       = fromMaybe (bvdTxFeePolicy bvd) (bvmTxFeePolicy bvm)
  , bvdUnlockStakeEpoch  = fromMaybe
    (bvdUnlockStakeEpoch bvd)
    (bvmUnlockStakeEpoch bvm)
  }
