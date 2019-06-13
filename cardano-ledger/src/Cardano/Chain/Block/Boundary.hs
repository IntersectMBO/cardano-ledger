{-# LANGUAGE OverloadedStrings #-}

-- | Boundary blocks have been deprecated, but we keep functions to decode them

module Cardano.Chain.Block.Boundary
  ( dropBoundaryConsensusData
  , dropBoundaryConsensusDataRetainEpochNumber
  , dropBoundaryExtraHeaderData
  , dropBoundaryBody
  , dropBoundaryExtraBodyData
  )
where

import Control.Monad (return, void)
import Data.Word (Word64)

import Cardano.Binary
  (Decoder, Dropper, decodeWord64, dropBytes, dropList, enforceSize)
import Cardano.Chain.Common (dropAttributes, dropChainDifficulty)


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

dropBoundaryConsensusData :: Dropper s
dropBoundaryConsensusData = void dropBoundaryConsensusDataRetainEpochNumber

dropBoundaryConsensusDataRetainEpochNumber :: Decoder s Word64
dropBoundaryConsensusDataRetainEpochNumber = do
  enforceSize "BoundaryConsensusData" 2
  w <- decodeWord64
  dropChainDifficulty
  return w


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
