{-# LANGUAGE OverloadedStrings #-}

-- | Types used in epoch boundary blocks

module Cardano.Chain.Block.Boundary
       ( dropBoundaryConsensusData
       , dropBoundaryExtraHeaderData
       , dropBoundaryBody
       , dropBoundaryExtraBodyData
       ) where

import           Cardano.Binary.Class (Dropper, dropBytes, dropList, dropWord64,
                     enforceSize)
import           Cardano.Chain.Common (dropAttributes, dropChainDifficulty)


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

dropBoundaryConsensusData :: Dropper s
dropBoundaryConsensusData = do
  enforceSize "BoundaryConsensusData" 2
  dropWord64
  dropChainDifficulty


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
