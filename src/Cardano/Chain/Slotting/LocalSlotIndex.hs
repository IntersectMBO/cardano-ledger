{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Slotting.LocalSlotIndex
  ( LocalSlotIndex(..)
  , LocalSlotIndexError(..)
  , mkLocalSlotIndex
  , addLocalSlotIndex
  , localSlotIndexToEnum
  , localSlotIndexFromEnum
  , localSlotIndexSucc
  , localSlotIndexPred
  , localSlotIndexMinBound
  , localSlotIndexMaxBound
  , localSlotIndices
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data)
import Formatting (bprint, build, int)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Slotting.EpochSlots (EpochSlots(..))


-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = UnsafeLocalSlotIndex
  { unLocalSlotIndex :: Word16
  } deriving (Eq, Ord, Generic, NFData, Show, B.Buildable)

instance Bi LocalSlotIndex where
  encode = encode . unLocalSlotIndex
  decode = UnsafeLocalSlotIndex <$> decode

deriveJSON defaultOptions ''LocalSlotIndex

data LocalSlotIndexError
  = LocalSlotIndexEnumOverflow EpochSlots Int
  | LocalSlotIndexEnumUnderflow Int
  | LocalSlotIndexOverflow EpochSlots Word64
  deriving Data

instance B.Buildable LocalSlotIndexError where
  build = \case
    LocalSlotIndexEnumOverflow epochSlots i -> bprint
      ( "localSlotIndexToEnum: "
      . int
      . " is greater than the maxBound, "
      . build
      )
      i
      (unEpochSlots epochSlots - 1)
    LocalSlotIndexEnumUnderflow i -> bprint
      ("localSlotIndexToEnum: " . build . " is less than the minBound, 0")
      i
    LocalSlotIndexOverflow epochSlots i -> bprint
      ( "Cannot construct LocalSlotIndex: "
      . build
      . " is greater than or equal to epochSlots, "
      . build
      )
      i
      epochSlots

localSlotIndexToEnum
  :: EpochSlots -> Int -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexToEnum epochSlots i
  | i >= fromIntegral (unEpochSlots epochSlots) =
      Left $ LocalSlotIndexEnumOverflow epochSlots i
  | i < 0 = Left $ LocalSlotIndexEnumUnderflow i
  | otherwise = Right $ UnsafeLocalSlotIndex (fromIntegral i)

localSlotIndexFromEnum :: LocalSlotIndex -> Int
localSlotIndexFromEnum = fromIntegral . unLocalSlotIndex

localSlotIndexSucc
  :: EpochSlots -> LocalSlotIndex -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexSucc epochSlots =
  localSlotIndexToEnum epochSlots . (+ 1) . localSlotIndexFromEnum

localSlotIndexPred
  :: EpochSlots -> LocalSlotIndex -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexPred epochSlots =
  localSlotIndexToEnum epochSlots . subtract 1 . localSlotIndexFromEnum

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: EpochSlots -> LocalSlotIndex
localSlotIndexMaxBound (EpochSlots es) =
  UnsafeLocalSlotIndex (fromIntegral $ es - 1)

-- | All local slot indices for the given number of slots in epoch, in ascending
--   order.
localSlotIndices :: EpochSlots -> [LocalSlotIndex]
localSlotIndices slotsInEpoch = fmap UnsafeLocalSlotIndex [0 .. upperBound]
 where
  upperBound :: Word16
  upperBound = fromIntegral (unEpochSlots slotsInEpoch) - 1

mkLocalSlotIndex
  :: MonadError LocalSlotIndexError m => EpochSlots -> Word16 -> m LocalSlotIndex
mkLocalSlotIndex es idx
  | idx < fromIntegral (unEpochSlots es) = pure (UnsafeLocalSlotIndex idx)
  | otherwise = throwError $ LocalSlotIndexOverflow es (fromIntegral idx)

-- | Shift slot index by given amount, returning a 'LocalSlotIndexError' on
--   overflow
addLocalSlotIndex
  :: EpochSlots
  -> EpochSlots
  -> LocalSlotIndex
  -> Either LocalSlotIndexError LocalSlotIndex
addLocalSlotIndex epochSlots x (UnsafeLocalSlotIndex i)
  | s < unEpochSlots epochSlots = Right $ UnsafeLocalSlotIndex (fromIntegral s)
  | otherwise = Left $ LocalSlotIndexOverflow epochSlots s
 where
  s :: Word64
  s = unEpochSlots x + fromIntegral i
