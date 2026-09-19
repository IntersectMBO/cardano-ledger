{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.BlockHeader where

import Cardano.Ledger.BaseTypes (ProtVer (..), SlotNo)
import Cardano.Ledger.Block
import Cardano.Ledger.Core
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
  , tbhProtVer :: ProtVer
  }
  deriving (Generic)

instance NFData TestBlockHeader

instance Era era => EraBlockHeader TestBlockHeader era where
  blockIssuerBlockHeaderG = to $ tbhIssuer . blockHeader
  blockHeaderSizeBlockHeaderG = to $ tbhHSize . blockHeader
  blockBodySizeBlockHeaderL =
    lens (tbhBSize . blockHeader) $
      \b@Block {blockHeader} tbhBSize -> b {blockHeader = blockHeader {tbhBSize}}
  blockBodyHashBlockHeaderL =
    lens (tbhBHash . blockHeader) $
      \b@Block {blockHeader} tbhBHash -> b {blockHeader = blockHeader {tbhBHash}}
  slotNoBlockHeaderL =
    lens (tbhSlot . blockHeader) $
      \b@Block {blockHeader} tbhSlot -> b {blockHeader = blockHeader {tbhSlot}}

instance Era era => TPraosEraBlockHeader TestBlockHeader era

instance Era era => PraosEraBlockHeader TestBlockHeader era where
  protVerBlockHeaderL =
    lens (tbhProtVer . blockHeader) $
      \b@Block {blockHeader} tbhProtVer -> b {blockHeader = blockHeader {tbhProtVer}}

instance Era era => LeiosEraBlockHeader TestBlockHeader era

mkTestBlockHeaderNoNonce ::
  forall era h.
  EraBlockHeader h era => Block h era -> TestBlockHeader
mkTestBlockHeaderNoNonce block =
  TestBlockHeader
    { tbhIssuer = block ^. blockIssuerBlockHeaderG
    , tbhHSize = block ^. blockHeaderSizeBlockHeaderG
    , tbhBSize = block ^. blockBodySizeBlockHeaderL
    , tbhBHash = block ^. blockBodyHashBlockHeaderL
    , tbhSlot = block ^. slotNoBlockHeaderL
    , tbhProtVer = ProtVer (eraProtVerLow @era) 0
    }
