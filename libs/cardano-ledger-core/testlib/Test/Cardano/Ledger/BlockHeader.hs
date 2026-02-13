{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.BlockHeader where

import Cardano.Ledger.BaseTypes (Nonce, ProtVer, SlotNo)
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
import Cardano.Ledger.Core (
  Era,
  EraIndependentBlockBody,
  HASH,
  Hash,
  KeyHash,
  KeyRole (BlockIssuer),
 )
import Control.DeepSeq (NFData)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro

data TestBlockHeader
  = TestBlockHeader
  { tbhIssuer :: KeyHash BlockIssuer
  , tbhBSize :: Word32
  , tbhHSize :: Int
  , tbhBHash :: Hash HASH EraIndependentBlockBody
  , tbhSlot :: SlotNo
  , tbhPrevNonce :: Maybe Nonce
  , tbhProtVer :: ProtVer
  }
  deriving (Generic)

instance NFData TestBlockHeader

instance Era era => EraBlockHeader TestBlockHeader era where
  blockHeaderIssuerL =
    lens (tbhIssuer . blockHeader) $
      \b@Block {blockHeader} tbhIssuer -> b {blockHeader = blockHeader {tbhIssuer}}
  blockHeaderBSizeL =
    lens (tbhBSize . blockHeader) $
      \b@Block {blockHeader} tbhBSize -> b {blockHeader = blockHeader {tbhBSize}}
  blockHeaderHSizeL =
    lens (tbhHSize . blockHeader) $
      \b@Block {blockHeader} tbhHSize -> b {blockHeader = blockHeader {tbhHSize}}
  blockHeaderBHashL =
    lens (tbhBHash . blockHeader) $
      \b@Block {blockHeader} tbhBHash -> b {blockHeader = blockHeader {tbhBHash}}
  blockHeaderSlotL =
    lens (tbhSlot . blockHeader) $
      \b@Block {blockHeader} tbhSlot -> b {blockHeader = blockHeader {tbhSlot}}
  blockHeaderProtVerL =
    lens (tbhProtVer . blockHeader) $
      \b@Block {blockHeader} tbhProtVer -> b {blockHeader = blockHeader {tbhProtVer}}

mkTestBlockHeaderNoNonce :: EraBlockHeader h era => Block h era -> TestBlockHeader
mkTestBlockHeaderNoNonce blk =
  TestBlockHeader
    { tbhIssuer = blk ^. blockHeaderIssuerL
    , tbhBSize = blk ^. blockHeaderBSizeL
    , tbhHSize = blk ^. blockHeaderHSizeL
    , tbhBHash = blk ^. blockHeaderBHashL
    , tbhSlot = blk ^. blockHeaderSlotL
    , tbhPrevNonce = Nothing
    , tbhProtVer = blk ^. blockHeaderProtVerL
    }
