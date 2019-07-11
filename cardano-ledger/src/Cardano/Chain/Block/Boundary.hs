{-# LANGUAGE OverloadedStrings #-}

-- | Boundary blocks have been deprecated, but we keep functions to decode them

module Cardano.Chain.Block.Boundary
  ( fromCBORBoundaryConsensusData
  , dropBoundaryExtraHeaderData
  , dropBoundaryBody
  , dropBoundaryExtraBodyData
  )
where

import Control.Monad (return)
import Data.Word (Word64)

import Cardano.Binary
  (Decoder, Dropper, decodeWord64, dropBytes, dropList, enforceSize, fromCBOR)
import Cardano.Chain.Common (ChainDifficulty, dropAttributes)


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
