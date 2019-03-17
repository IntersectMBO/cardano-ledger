{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Block.SlogUndo
  ( SlogUndo(..)
  , buildSlogUndo
  )
where

import Cardano.Prelude

import Formatting (Format, bprint, later)

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Slotting (FlatSlotId, EpochSlots, slotIdF, unflattenSlotId)


-- | Undo data from Slog, i. e. data which is necessary do rollback a block
--   inside Slog.
--
--   If block is one of the first 'blkSecurityParam' blocks, we don't need to
--   store anything. We also don't need to store anything for genesis blocks.
--   Otherwise we store 'FlatSlotId' of the oldest block from those for which we
--   stored slots before given block was applied.
newtype SlogUndo = SlogUndo
  { getSlogUndo :: Maybe FlatSlotId
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

buildSlogUndo :: EpochSlots -> Format r (SlogUndo -> r)
buildSlogUndo epochSlots = later $ \(SlogUndo oldSlot) ->
  "SlogUndo: "
    <> maybe "<nothing>" (bprint slotIdF . unflattenSlotId epochSlots) oldSlot

instance Bi SlogUndo where
  encode su = encodeListLen 1 <> encode (getSlogUndo su)
  decode = enforceSize "SlogUndo" 1 >> SlogUndo <$> decode
