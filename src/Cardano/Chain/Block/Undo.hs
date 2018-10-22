{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Block.Undo
       ( Undo (..)
       , buildUndo
       , Blund
       ) where

import           Cardano.Prelude

import           Formatting (Format, bprint, build, later)

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Block.Block (Block)
import           Cardano.Chain.Block.SlogUndo (SlogUndo (..), buildSlogUndo)
import qualified Cardano.Chain.Delegation as Delegation (Undo)
import           Cardano.Chain.Slotting (SlotCount)
import           Cardano.Chain.Txp (TxpUndo)
import           Cardano.Chain.Update.Undo (USUndo)


-- | Structure for undo block during rollback
data Undo = Undo
  { undoTx   :: !TxpUndo
  , undoDlg  :: !Delegation.Undo
  , undoUS   :: !USUndo
  , undoSlog :: !SlogUndo
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

-- | Block and its Undo
type Blund = (Block, Undo)

buildUndo :: SlotCount -> Format r (Undo -> r)
buildUndo epochSlots = later $ \undo -> bprint
  ( "Undo:\n"
  . "  undoTx: " . listJson . "\n"
  . "  undoDlg: " . build . "\n"
  . "  undoUS: " . build . "\n"
  . "  undoSlog: " . buildSlogUndo epochSlots
  )
  (map (bprint listJson) (undoTx undo))
  (undoDlg undo)
  (undoUS undo)
  (undoSlog undo)

instance Bi Undo where
  encode undo =
    encodeListLen 4
      <> encode (undoTx undo)
      <> encode (undoDlg undo)
      <> encode (undoUS undo)
      <> encode (undoSlog undo)

  decode = do
    enforceSize "Undo" 4
    Undo <$> decode <*> decode <*> decode <*> decode
