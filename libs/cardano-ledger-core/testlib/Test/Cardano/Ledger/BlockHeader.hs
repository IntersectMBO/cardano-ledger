{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.BlockHeader where

import Cardano.Ledger.BaseTypes (ProtVer (..), SlotNo, getVersion32, mkVersion32)
import Cardano.Ledger.Block
import Cardano.Ledger.Core
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)
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
  , tbhVersionInfo :: BlockHeaderVersionInfo
  }
  deriving (Generic)

instance NFData TestBlockHeader

instance Era era => EraBlockHeader TestBlockHeader era where
  blockIssuerBlockHeaderG = blockHeaderL . to tbhIssuer
  blockHeaderSizeBlockHeaderG = blockHeaderL . to tbhHSize
  blockBodySizeBlockHeaderL =
    blockHeaderL . lens tbhBSize (\bh sz -> bh {tbhBSize = sz})
  blockBodyHashBlockHeaderL =
    blockHeaderL . lens tbhBHash (\bh h -> bh {tbhBHash = h})
  slotNoBlockHeaderL =
    blockHeaderL . lens tbhSlot (\hb sn -> hb {tbhSlot = sn})

instance Era era => TPraosEraBlockHeader TestBlockHeader era

instance Era era => PraosEraBlockHeader TestBlockHeader era where
  protVerBlockHeaderL =
    versionInfoBlockHeaderL
      . lens
        (\(BlockHeaderVersionInfo major minor) -> ProtVer (fromMaybe maxBound (mkVersion32 major)) minor)
        (\_ (ProtVer major minor) -> BlockHeaderVersionInfo (getVersion32 major) minor)

instance Era era => LeiosEraBlockHeader TestBlockHeader era where
  versionInfoBlockHeaderL = blockHeaderL . lens tbhVersionInfo (\bh vi -> bh {tbhVersionInfo = vi})

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
    , tbhVersionInfo = BlockHeaderVersionInfo (getVersion32 (eraProtVerLow @era)) 0
    }
