{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.Index
  ( EpochIndex(..)
  , epochIndex
  , readIndex )
where

import           Data.Binary.Get (getWord64be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word64)
import           System.IO (IOMode (..), SeekMode (..), hSeek, withBinaryFile)

import           Cardano.Prelude

-- Index format:
--
-- EpochIndex := "Epoch Index v1\n\n" *Offset
-- Offset := Word64BE

indexHeader :: LBS.ByteString
indexHeader = "Epoch Index v1\n\n"

newtype EpochIndex = EpochIndex FilePath

epochIndex :: FilePath -> IO (Maybe EpochIndex)
epochIndex file =
    withBinaryFile file ReadMode $ \h -> do
        headerIsGood <- (indexHeader==) <$> LBS.hGet h (fromIntegral $ LBS.length indexHeader)
        if headerIsGood
        then pure (Just (EpochIndex file))
        else pure Nothing

type Slot = Integer

indexHeaderLength :: Int64
indexHeaderLength = LBS.length indexHeader

-- | Returns Nothing if the offset for the given slot is missing from the index.
readIndex
    :: EpochIndex
    -> Slot
    -> IO (Maybe Word64)
readIndex (EpochIndex file) slot =
    let readLocation = fromIntegral indexHeaderLength + fromIntegral slot * 8
    in  withBinaryFile file ReadMode $ \h -> do
            hSeek h AbsoluteSeek readLocation
            bytes <- LBS.hGet h 8
            pure $ case B.runGetOrFail getWord64be bytes of
                Left _               -> Nothing
                Right (_, _, offset) -> Just offset
