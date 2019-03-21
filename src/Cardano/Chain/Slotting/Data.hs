{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Core types used in 'Slotting'

module Cardano.Chain.Slotting.Data
  ( EpochSlottingData(..)
  , SlottingData
  , SlottingDataError(..)
  , getSlottingDataMap
  , mkSlottingData
  , validateSlottingDataMap
  , createInitSlottingData
  , getAllEpochIndices
  , getCurrentEpochIndex
  , getCurrentEpochSlottingData
  , getNextEpochIndex
  , getNextEpochSlottingData
  , addEpochSlottingData
  , lookupEpochSlottingData
  , computeSlotStart
  , unsafeSlottingData
  )
where

import Cardano.Prelude

import Data.Data (Data)
import Data.Map.Strict as M
import Data.Semigroup (Semigroup)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Formatting (bprint, build, int, sformat, string)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  (Bi(..), Decoder, DecoderError(..), encodeListLen, enforceSize)
import Cardano.Chain.Slotting.EpochIndex (EpochIndex(..))
import Cardano.Chain.Slotting.LocalSlotIndex (LocalSlotIndex(..))


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

instance B.Buildable SlottingData where
  build (SlottingData sMap) = bprint
    ("SlottingData: " . string)
    (show sMap)

instance Bi SlottingData where
  encode slottingData = encode $ getSlottingDataMap slottingData
  decode = checkIfSlottindDataValid decode
   where
    -- We first check if the data we are trying to decode is valid.
    -- We don't want to throw a runtime error.
    checkIfSlottindDataValid
      :: Decoder s (Map EpochIndex EpochSlottingData) -> Decoder s SlottingData
    checkIfSlottindDataValid slottingDataM = do
      mkSlottingData <$> slottingDataM >>= \case
        Left err -> cborError $ DecoderErrorCustom "SlottingData" $ sformat build err
        Right sd -> pure sd

data SlottingDataError
  = SlottingDataTooFewIndices Int
  | SlottingDataInvalidIndices [EpochIndex] [EpochIndex]
  deriving Data

instance B.Buildable SlottingDataError where
  build = \case
    SlottingDataInvalidIndices expected actual -> bprint
      ( "Incorrect indices while constructing SlottingData.\n"
      . "Expected: "
      . listJson
      . "\n"
      . "Got: "
      . listJson
      )
      expected
      actual
    SlottingDataTooFewIndices numIndices -> bprint
      ( "Too few indices while constructing SlottingData.\n"
      . "Expected more than 2, but only got "
      . int
      )
      numIndices


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Constructor that returns a `SlottingDataError` on invalid input
--
--   TODO: Refine the input type so this is no longer unsafe
mkSlottingData
  :: Map EpochIndex EpochSlottingData -> Either SlottingDataError SlottingData
mkSlottingData epochSlottingDataMap = do
  validateSlottingDataMap epochSlottingDataMap
  pure $ SlottingData epochSlottingDataMap

-- This doesn't check invariants, use at your own risk.
-- Ideally this should only be used in tests
unsafeSlottingData :: Map EpochIndex EpochSlottingData -> SlottingData
unsafeSlottingData = SlottingData

-- | The validation for the @SlottingData@. It's visible since it's needed
--   externally.
validateSlottingDataMap
  :: Map EpochIndex EpochSlottingData -> Either SlottingDataError ()
validateSlottingDataMap epochSlottingDataMap
  | numIndices < 2 = Left $ SlottingDataTooFewIndices numIndices
  | not validEpochIndices = Left
  $ SlottingDataInvalidIndices correctEpochIndices currentEpochIndices
  | otherwise = Right ()
 where
  numIndices          = M.size epochSlottingDataMap
  -- We validate if the epoch indices are sequential, it's invalid if they
  -- start having "holes" [..,6,7,9,...].
  validEpochIndices   = correctEpochIndices == currentEpochIndices
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
addEpochSlottingData slottingData epochSlottingData =
  SlottingData $ M.insert nextEpochIndex epochSlottingData $ getSlottingDataMap
    slottingData
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
  timeToLocalSlot = fromIntegral (unLocalSlotIndex slotIndex) * esdSlotDuration esd
