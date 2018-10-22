{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Core types used in 'Slotting'

module Cardano.Chain.Slotting.Data
       ( EpochSlottingData (..)
       , SlottingData
       , getSlottingDataMap
       , createSlottingDataUnsafe
       , isValidSlottingDataMap
       , createInitSlottingData
       , getAllEpochIndices
       , getCurrentEpochIndex
       , getCurrentEpochSlottingData
       , getNextEpochIndex
       , getNextEpochSlottingData
       , addEpochSlottingData
       , lookupEpochSlottingData
       , computeSlotStart
       ) where

import           Cardano.Prelude

import           Data.Map.Strict as M
import           Data.Semigroup (Semigroup)
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime)

import           Cardano.Binary.Class (Bi (..), DecoderError (..),
                     encodeListLen, enforceSize)
import           Cardano.Chain.Slotting.EpochIndex (EpochIndex (..))
import           Cardano.Chain.Slotting.LocalSlotIndex (LocalSlotIndex)


--------------------------------------------------------------------------------
-- Type declarations
--------------------------------------------------------------------------------

-- | Data which is necessary for slotting and corresponds to a particular epoch
data EpochSlottingData = EpochSlottingData
  { esdSlotDuration :: !NominalDiffTime
  -- ^ Slot duration for a specific epoch
  , esdStartDiff    :: !NominalDiffTime
  -- ^ Difference between epoch start and system start time
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance Bi EpochSlottingData where
  encode esd =
    encodeListLen 2 <> encode (esdSlotDuration esd) <> encode (esdStartDiff esd)

  decode = do
    enforceSize "EpochSlottingData" 2
    EpochSlottingData <$> decode <*> decode

-- Helpful type aliases
type CurrentEpochSlottingData = EpochSlottingData
type NextEpochSlottingData    = EpochSlottingData

-- | Data necessary for slotting to work which is basically part of GState.
--   Note that it's important to use error rather than default values like 0,
--   because such cases indicate invariants violation and shouldn't be hidden
--   behind default values.
newtype SlottingData = SlottingData
  { getSlottingDataMap :: Map EpochIndex EpochSlottingData
  -- ^ Map containing the @EpochSlottingData@ for all the (known) @Epoch@
  }
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NFData

instance Bi SlottingData where
  encode slottingData = encode $ getSlottingDataMap slottingData
  decode = checkIfSlottindDataValid decode
   where
    -- We first check if the data we are trying to decode is valid.
    -- We don't want to throw a runtime error.
    checkIfSlottindDataValid slottingDataM = do
      slottingData <- slottingDataM
      if isValidSlottingDataMap slottingData
        then pure $ createSlottingDataUnsafe slottingData
        else cborError
          $ DecoderErrorCustom "SlottingData" "Invalid slotting data state!"


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Unsafe constructor that can lead to unsafe crash!
--
--   TODO: Refine the input type so this is no longer unsafe
createSlottingDataUnsafe :: Map EpochIndex EpochSlottingData -> SlottingData
createSlottingDataUnsafe epochSlottingDataMap =
  if isValidSlottingDataMap epochSlottingDataMap
    then SlottingData epochSlottingDataMap
    else criticalError
 where
  criticalError =
    panic
      "It's impossible to create slotting data without at least\
    \ two epochs. Epochs need to be sequential."

-- | The validation for the @SlottingData@. It's visible since it's needed
--   externally.
isValidSlottingDataMap :: Map EpochIndex EpochSlottingData -> Bool
isValidSlottingDataMap epochSlottingDataMap =
  M.size epochSlottingDataMap >= 2 && validEpochIndices
 where
  -- We validate if the epoch indices are sequential, it's invalid if they
  -- start having "holes" [..,6,7,9,...].
  validEpochIndices = correctEpochIndices == currentEpochIndices
   where
    currentEpochIndices = keys epochSlottingDataMap
    correctEpochIndices = EpochIndex . fromIntegral <$> [0 .. zIMapLenght]
      where zIMapLenght = pred . length . keys $ epochSlottingDataMap

-- | Restricted constructor for the (initial) creation of 'SlottingData'
createInitSlottingData
  :: CurrentEpochSlottingData -> NextEpochSlottingData -> SlottingData
createInitSlottingData psd esd = SlottingData validInitialSlottingData
 where
  validInitialSlottingData :: Map EpochIndex EpochSlottingData
  validInitialSlottingData =
    M.union currentEpochSlottingData nextEpochSlottingData

  currentEpochSlottingData :: Map EpochIndex CurrentEpochSlottingData
  currentEpochSlottingData = M.singleton 0 psd

  nextEpochSlottingData :: Map EpochIndex NextEpochSlottingData
  nextEpochSlottingData = M.singleton 1 esd

-- | Get all epoch indices
getAllEpochIndices :: SlottingData -> [EpochIndex]
getAllEpochIndices = M.keys . getSlottingDataMap

-- | Get the next epoch index
getNextEpochIndex :: SlottingData -> EpochIndex
getNextEpochIndex = fst . M.findMax . getSlottingDataMap

-- | Get the next epoch slotting data
getNextEpochSlottingData :: SlottingData -> EpochSlottingData
getNextEpochSlottingData = snd . M.findMax . getSlottingDataMap

-- | Get the current epoch index. Next epoch - 1.
getCurrentEpochIndex :: SlottingData -> EpochIndex
getCurrentEpochIndex = pred . getNextEpochIndex

-- | Get the current epoch slotting data. Next epoch - 1.
getCurrentEpochSlottingData :: SlottingData -> EpochSlottingData
getCurrentEpochSlottingData sdp@(getSlottingDataMap -> sd) =
  sd M.! currentEpochIndex
  where currentEpochIndex = getCurrentEpochIndex sdp

-- | Lookup the slotting data for an arbitrary 'EpochIndex'
lookupEpochSlottingData :: EpochIndex -> SlottingData -> Maybe EpochSlottingData
lookupEpochSlottingData epochIndex = M.lookup epochIndex . getSlottingDataMap

-- | Add `EpochSlottingData`.
addEpochSlottingData :: SlottingData -> EpochSlottingData -> SlottingData
addEpochSlottingData slottingData epochSlottingData = SlottingData
  $ M.insert nextEpochIndex epochSlottingData
  $ getSlottingDataMap slottingData
 where
    -- We can calculate the index ourselves, no need to pass it around
  nextEpochIndex :: EpochIndex
  nextEpochIndex = succ . getNextEpochIndex $ slottingData

-- | Compute when the slot started
--
--   Add time up until this epoch and time within this epoch to the start time
computeSlotStart :: UTCTime -> LocalSlotIndex -> EpochSlottingData -> UTCTime
computeSlotStart systemStart slotIndex esd = addUTCTime timeToSlot systemStart
 where
  timeToSlot :: NominalDiffTime
  timeToSlot = timeToEpoch + timeToLocalSlot

  -- | We get the epoch start time by adding the epoch slotting data start diff
  timeToEpoch :: NominalDiffTime
  timeToEpoch = esdStartDiff esd

  timeToLocalSlot :: NominalDiffTime
  timeToLocalSlot = fromIntegral slotIndex * esdSlotDuration esd
