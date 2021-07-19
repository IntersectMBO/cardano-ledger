{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.File
  ( mainnetEpochSlots,
    parseEpochFileWithBoundary,
    parseEpochFilesWithBoundary,
    ParseError (..),
  )
where

import Cardano.Binary (DecoderError, decodeFullDecoder, slice)
import Cardano.Chain.Block.Block
  ( ABlockOrBoundary (..),
    fromCBORABlockOrBoundary,
  )
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Prelude
import Control.Monad.Trans.Resource (ResIO)
import qualified Data.Binary as B
import Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import Data.String (String)
import Streaming.Binary (decodedWith)
import qualified Streaming.ByteString as SBS
import Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import System.Directory (doesFileExist)
import System.FilePath ((-<.>))

-- Epoch file format:
--
-- EpochFile := "Epoch data v1\n" *SlotData
-- SlotData := "blnd" BlockLength UndoLength Block Undo
-- BlockLength := Word32BE
-- UndoLength := Word32BE
-- Block := CBOR
-- Undo := CBOR

epochHeader :: LBS.ByteString
epochHeader = "Epoch data v1\n"

data ParseError
  = -- | The CBOR is invalid
    ParseErrorDecoder !DecoderError
  | ParseErrorBinary !FilePath !B.ByteOffset !Text
  | ParseErrorMissingHeader !FilePath
  deriving (Eq, Show)

loadFileWithHeader ::
  FilePath -> LBS.ByteString -> SBS.ByteStream (ExceptT ParseError ResIO) ()
loadFileWithHeader file header =
  let bytes :: SBS.ByteStream (ExceptT ParseError ResIO) ()
      bytes = SBS.readFile file

      len :: Int64
      len = LBS.length header
   in do
        (h :> rest) <- lift $ SBS.toLazy $ SBS.splitAt len bytes
        if h == header
          then rest
          else lift $ throwError (ParseErrorMissingHeader file)

-- | Slots per epoch used in mainnet
--
-- This number has been fixed throughout the Byron era.
mainnetEpochSlots :: EpochSlots
mainnetEpochSlots = EpochSlots 21600

parseEpochFileWithBoundary ::
  EpochSlots ->
  FilePath ->
  Stream
    (Of (ABlockOrBoundary ByteString))
    (ExceptT ParseError ResIO)
    ()
parseEpochFileWithBoundary epochSlots file = do
  s <-
    S.mapM liftDecoderError $
      decodedWith (getSlotData epochSlots) (boundaryBytes <> bytes)
  liftBinaryError s
  where
    boundaryBytes :: SBS.ByteStream (ExceptT ParseError ResIO) ()
    boundaryBytes = do
      let boundaryFile = file -<.> "boundary"
      boundaryExists <- liftIO $ doesFileExist boundaryFile
      when boundaryExists $ SBS.readFile boundaryFile

    bytes = loadFileWithHeader file epochHeader

    liftDecoderError :: Either DecoderError a -> ExceptT ParseError ResIO a
    liftDecoderError = \case
      Right a -> pure a
      Left err -> throwError (ParseErrorDecoder err)

    liftBinaryError ::
      (a, B.ByteOffset, Either String ()) ->
      Stream
        (Of (ABlockOrBoundary ByteString))
        (ExceptT ParseError ResIO)
        ()
    liftBinaryError = \case
      (_, _, Right ()) -> pure ()
      (_, offset, Left message) ->
        throwError (ParseErrorBinary file offset (toS message))

parseEpochFilesWithBoundary ::
  EpochSlots ->
  [FilePath] ->
  Stream
    (Of (ABlockOrBoundary ByteString))
    (ExceptT ParseError ResIO)
    ()
parseEpochFilesWithBoundary epochSlots fs =
  foldr (<>) mempty (parseEpochFileWithBoundary epochSlots <$> fs)

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

getSlotData :: EpochSlots -> B.Get (Either DecoderError (ABlockOrBoundary ByteString))
getSlotData epochSlots = runExceptT $ do
  header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
  lift $ guard (header == slotDataHeader)
  blockSize <- lift getWord32be
  undoSize <- lift getWord32be
  block <- do
    blockBytes <- lift $ B.getLazyByteString (fromIntegral blockSize)
    bb <-
      ExceptT . pure $
        decodeFullDecoder
          "ABlockOrBoundary"
          (fromCBORABlockOrBoundary epochSlots)
          blockBytes
    pure $ map (LBS.toStrict . slice blockBytes) bb
  -- Drop the Undo bytes as we no longer use these
  void . lift $ B.getLazyByteString (fromIntegral undoSize)
  pure block
