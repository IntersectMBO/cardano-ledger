{-# LANGUAGE OverloadedStrings #-}

-- | Boundary blocks have been deprecated, but we keep functions to decode them
module Cardano.Chain.Block.Boundary
  ( fromCBORBoundaryConsensusData,
    dropBoundaryExtraHeaderData,
    dropBoundaryExtraHeaderDataRetainGenesisTag,
    dropBoundaryBody,
    dropBoundaryExtraBodyData,
  )
where

import Cardano.Binary
  ( Decoder,
    Dropper,
    decodeWord64,
    dropBytes,
    dropList,
    enforceSize,
    fromCBOR,
  )
import Cardano.Chain.Common
  ( ChainDifficulty,
    attrData,
    dropAttributes,
    fromCBORAttributes,
  )
import Cardano.Prelude

--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

fromCBORBoundaryConsensusData :: Decoder s (Word64, ChainDifficulty)
fromCBORBoundaryConsensusData = do
  enforceSize "BoundaryConsensusData" 2
  w <- decodeWord64
  cd <- fromCBOR
  return (w, cd)

--------------------------------------------------------------------------------
-- BoundaryExtraHeaderData
--------------------------------------------------------------------------------

dropBoundaryExtraHeaderData :: Dropper s
dropBoundaryExtraHeaderData = do
  enforceSize "BoundaryExtraHeaderData" 1
  dropAttributes

-- | When starting a new chain in ourorobos-consensus, we often start from a
--   non-zero epoch. This is done in order to ensure synchronisation between
--   nodes - we assume that the chain started at some fixed point in the past
--   (e.g. midnight) which all nodes can agree on despite different node start
--   times. However, the standard deserialisation assumes that the genesis EBB
--   is precisely that in epoch zero.
--
--   In order to successfully round-trip a genesis EBB in a non-zero epoch,
--   then, we add a "magic" tag which indicates the presense of the genesis
--   hash. The choice of 255 and the word "Genesis" is completely arbitrary, and
--   only done to correspond with the matching encoder. This encoding will only
--   ever be seen when processing blocks from a demo.
dropBoundaryExtraHeaderDataRetainGenesisTag :: Decoder s Bool
dropBoundaryExtraHeaderDataRetainGenesisTag = do
  enforceSize "BoundaryExtraHeaderData" 1
  attrData
    <$> fromCBORAttributes
      False
      (\w8 bs t -> pure . Just $ t || w8 == 255 && bs == "Genesis")

--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

dropBoundaryBody :: Dropper s
dropBoundaryBody = dropList dropBytes

--------------------------------------------------------------------------------
-- BoundaryExtraBodyData
--------------------------------------------------------------------------------

dropBoundaryExtraBodyData :: Dropper s
dropBoundaryExtraBodyData = do
  enforceSize "BoundaryExtraBodyData" 1
  dropAttributes
