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
import Data.Ix (Ix)
import Formatting (bprint, build, int)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Slotting.SlotCount (SlotCount)


-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = UnsafeLocalSlotIndex
  { getSlotIndex :: Word16
  } deriving ( Show
             , Eq
             , Ord
             , Ix
             , Generic
             , B.Buildable
             , NFData
             , Num
             , Integral
             , Real
             , Enum
             )

instance Bi LocalSlotIndex where
  encode = encode . getSlotIndex
  decode = UnsafeLocalSlotIndex <$> decode

deriveJSON defaultOptions ''LocalSlotIndex

data LocalSlotIndexError
  = LocalSlotIndexEnumOverflow SlotCount Int
  | LocalSlotIndexEnumUnderflow Int
  | LocalSlotIndexOverflow SlotCount Word64

instance B.Buildable LocalSlotIndexError where
  build = \case
    LocalSlotIndexEnumOverflow epochSlots i -> bprint
      ( "localSlotIndexToEnum: "
      . int
      . " is greater than the maxBound, "
      . build
      )
      i
      (epochSlots - 1)
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
  :: SlotCount -> Int -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexToEnum epochSlots i
  | i >= fromIntegral epochSlots = Left
  $ LocalSlotIndexEnumOverflow epochSlots i
  | i < 0 = Left $ LocalSlotIndexEnumUnderflow i
  | otherwise = Right $ UnsafeLocalSlotIndex (fromIntegral i)

localSlotIndexFromEnum :: LocalSlotIndex -> Int
localSlotIndexFromEnum = fromIntegral . getSlotIndex

localSlotIndexSucc
  :: SlotCount -> LocalSlotIndex -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexSucc epochSlots =
  localSlotIndexToEnum epochSlots . (+ 1) . localSlotIndexFromEnum

localSlotIndexPred
  :: SlotCount -> LocalSlotIndex -> Either LocalSlotIndexError LocalSlotIndex
localSlotIndexPred epochSlots =
  localSlotIndexToEnum epochSlots . subtract 1 . localSlotIndexFromEnum

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: SlotCount -> LocalSlotIndex
localSlotIndexMaxBound epochSlots =
  UnsafeLocalSlotIndex (fromIntegral epochSlots - 1)

-- | All local slot indices for the given number of slots in epoch, in ascending
--   order.
localSlotIndices :: SlotCount -> [LocalSlotIndex]
localSlotIndices slotsInEpoch = fmap UnsafeLocalSlotIndex [0 .. upperBound]
 where
  upperBound :: Word16
  upperBound = fromIntegral slotsInEpoch - 1

mkLocalSlotIndex
  :: MonadError LocalSlotIndexError m => SlotCount -> Word16 -> m LocalSlotIndex
mkLocalSlotIndex es idx
  | idx < fromIntegral es = pure (UnsafeLocalSlotIndex idx)
  | otherwise = throwError $ LocalSlotIndexOverflow es (fromIntegral idx)

-- | Shift slot index by given amount, returning a 'LocalSlotIndexError' on
--   overflow
addLocalSlotIndex
  :: SlotCount
  -> SlotCount
  -> LocalSlotIndex
  -> Either LocalSlotIndexError LocalSlotIndex
addLocalSlotIndex epochSlots x (UnsafeLocalSlotIndex i)
  | s < fromIntegral epochSlots = Right $ UnsafeLocalSlotIndex (fromIntegral s)
  | otherwise = Left $ LocalSlotIndexOverflow epochSlots s
 where
  s :: Word64
  s = fromIntegral x + fromIntegral i
