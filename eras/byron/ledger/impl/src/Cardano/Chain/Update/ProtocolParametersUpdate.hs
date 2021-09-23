{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Update.ProtocolParametersUpdate
  ( ProtocolParametersUpdate (..),
    isEmpty,
    apply,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import Cardano.Chain.Common (LovelacePortion, TxFeePolicy)
import Cardano.Chain.Slotting (EpochNumber, SlotNumber (..))
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters (..))
import Cardano.Chain.Update.SoftforkRule (SoftforkRule)
import Cardano.Prelude hiding (empty)
import Data.Aeson (ToJSON)
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, bytes, later, shortest)
import qualified Formatting.Buildable as B

-- | Data which represents modifications of block (aka protocol) version
data ProtocolParametersUpdate = ProtocolParametersUpdate
  { ppuScriptVersion :: !(Maybe Word16),
    ppuSlotDuration :: !(Maybe Natural),
    ppuMaxBlockSize :: !(Maybe Natural),
    ppuMaxHeaderSize :: !(Maybe Natural),
    ppuMaxTxSize :: !(Maybe Natural),
    ppuMaxProposalSize :: !(Maybe Natural),
    ppuMpcThd :: !(Maybe LovelacePortion),
    ppuHeavyDelThd :: !(Maybe LovelacePortion),
    ppuUpdateVoteThd :: !(Maybe LovelacePortion),
    ppuUpdateProposalThd :: !(Maybe LovelacePortion),
    ppuUpdateProposalTTL :: !(Maybe SlotNumber),
    ppuSoftforkRule :: !(Maybe SoftforkRule),
    ppuTxFeePolicy :: !(Maybe TxFeePolicy),
    ppuUnlockStakeEpoch :: !(Maybe EpochNumber)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance B.Buildable ProtocolParametersUpdate where
  build ppu =
    bprint
      ( "{ script version: " . bmodifier build
          . ", slot duration: "
          . bmodifier bytes'
          . ", block size limit: "
          . bmodifier bytes'
          . ", header size limit: "
          . bmodifier bytes'
          . ", tx size limit: "
          . bmodifier bytes'
          . ", proposal size limit: "
          . bmodifier bytes'
          . ", mpc threshold: "
          . bmodifier build
          . ", heavyweight delegation threshold: "
          . bmodifier build
          . ", update vote threshold: "
          . bmodifier build
          . ", update proposal threshold: "
          . bmodifier build
          . ", update implicit period (slots): "
          . bmodifier build
          . ", softfork rule: "
          . bmodifier build
          . ", tx fee policy: "
          . bmodifier build
          . ", unlock stake epoch: "
          . bmodifier build
          . " }"
      )
      (ppuScriptVersion ppu)
      (ppuSlotDuration ppu)
      (ppuMaxBlockSize ppu)
      (ppuMaxHeaderSize ppu)
      (ppuMaxTxSize ppu)
      (ppuMaxProposalSize ppu)
      (ppuMpcThd ppu)
      (ppuHeavyDelThd ppu)
      (ppuUpdateVoteThd ppu)
      (ppuUpdateProposalThd ppu)
      (ppuUpdateProposalTTL ppu)
      (ppuSoftforkRule ppu)
      (ppuTxFeePolicy ppu)
      (ppuUnlockStakeEpoch ppu)
    where
      bmodifier :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
      bmodifier b = later $ maybe "no change" (bprint b)

      bytes' :: Format r (Natural -> r)
      bytes' = bytes (shortest @Double)

-- Used for debugging purposes only
instance ToJSON ProtocolParametersUpdate

instance ToCBOR ProtocolParametersUpdate where
  toCBOR ppu =
    encodeListLen 14
      <> toCBOR (ppuScriptVersion ppu)
      <> toCBOR (ppuSlotDuration ppu)
      <> toCBOR (ppuMaxBlockSize ppu)
      <> toCBOR (ppuMaxHeaderSize ppu)
      <> toCBOR (ppuMaxTxSize ppu)
      <> toCBOR (ppuMaxProposalSize ppu)
      <> toCBOR (ppuMpcThd ppu)
      <> toCBOR (ppuHeavyDelThd ppu)
      <> toCBOR (ppuUpdateVoteThd ppu)
      <> toCBOR (ppuUpdateProposalThd ppu)
      <> toCBOR (ppuUpdateProposalTTL ppu)
      <> toCBOR (ppuSoftforkRule ppu)
      <> toCBOR (ppuTxFeePolicy ppu)
      <> toCBOR (ppuUnlockStakeEpoch ppu)

instance FromCBOR ProtocolParametersUpdate where
  fromCBOR = do
    enforceSize "ProtocolParametersUpdate" 14
    ProtocolParametersUpdate
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

empty :: ProtocolParametersUpdate
empty =
  ProtocolParametersUpdate
    { ppuScriptVersion = Nothing,
      ppuSlotDuration = Nothing,
      ppuMaxBlockSize = Nothing,
      ppuMaxHeaderSize = Nothing,
      ppuMaxTxSize = Nothing,
      ppuMaxProposalSize = Nothing,
      ppuMpcThd = Nothing,
      ppuHeavyDelThd = Nothing,
      ppuUpdateVoteThd = Nothing,
      ppuUpdateProposalThd = Nothing,
      ppuUpdateProposalTTL = Nothing,
      ppuSoftforkRule = Nothing,
      ppuTxFeePolicy = Nothing,
      ppuUnlockStakeEpoch = Nothing
    }

isEmpty :: ProtocolParametersUpdate -> Bool
isEmpty = (== empty)

-- | Apply 'ProtocolParametersUpdate' to 'ProtocolParameters'
apply :: ProtocolParametersUpdate -> ProtocolParameters -> ProtocolParameters
apply ppu pp =
  ProtocolParameters
    { ppScriptVersion = fromMaybe (ppScriptVersion pp) (ppuScriptVersion ppu),
      ppSlotDuration = fromMaybe (ppSlotDuration pp) (ppuSlotDuration ppu),
      ppMaxBlockSize = fromMaybe (ppMaxBlockSize pp) (ppuMaxBlockSize ppu),
      ppMaxHeaderSize = fromMaybe (ppMaxHeaderSize pp) (ppuMaxHeaderSize ppu),
      ppMaxTxSize = fromMaybe (ppMaxTxSize pp) (ppuMaxTxSize ppu),
      ppMaxProposalSize =
        fromMaybe
          (ppMaxProposalSize pp)
          (ppuMaxProposalSize ppu),
      ppMpcThd = fromMaybe (ppMpcThd pp) (ppuMpcThd ppu),
      ppHeavyDelThd = fromMaybe (ppHeavyDelThd pp) (ppuHeavyDelThd ppu),
      ppUpdateVoteThd = fromMaybe (ppUpdateVoteThd pp) (ppuUpdateVoteThd ppu),
      ppUpdateProposalThd =
        fromMaybe
          (ppUpdateProposalThd pp)
          (ppuUpdateProposalThd ppu),
      ppUpdateProposalTTL = fromMaybe (ppUpdateProposalTTL pp) (ppuUpdateProposalTTL ppu),
      ppSoftforkRule = fromMaybe (ppSoftforkRule pp) (ppuSoftforkRule ppu),
      ppTxFeePolicy = fromMaybe (ppTxFeePolicy pp) (ppuTxFeePolicy ppu),
      ppUnlockStakeEpoch =
        fromMaybe
          (ppUnlockStakeEpoch pp)
          (ppuUnlockStakeEpoch ppu)
    }
