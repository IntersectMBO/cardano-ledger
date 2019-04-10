{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.File
  ( mainnetEpochSlots
  , parseEpochFile
  , parseEpochFiles
  , parseEpochFileWithBoundary
  , parseEpochFilesWithBoundary
  , ParseError(..)
  )
where

import Cardano.Prelude

import Control.Monad (guard)
import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Resource (ResIO)
import qualified Data.Binary as B
import Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Streaming as SBS
import Data.Char (isDigit)
import Data.String (String)
import Streaming.Binary (decodedWith)
import Streaming.Prelude (Of(..), Stream)
import qualified Streaming.Prelude as S
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (-<.>))

import Cardano.Binary (DecoderError, decodeFullDecoder, slice)
import Cardano.Chain.Block.Block
  (ABlock, ABlockOrBoundary(..), fromCBORABlockOrBoundary)
import Cardano.Chain.Slotting (EpochSlots(..))


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
  = ParseErrorDecoder !DecoderError
  -- ^ The CBOR is invalid
  | ParseErrorBinary !FilePath !B.ByteOffset !Text
  | ParseErrorMissingHeader !FilePath
  deriving (Eq, Show)

loadFileWithHeader
  :: FilePath -> LBS.ByteString -> SBS.ByteString (ExceptT ParseError ResIO) ()
loadFileWithHeader file header =
  let
    bytes :: SBS.ByteString (ExceptT ParseError ResIO) ()
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

parseEpochFile
  :: EpochSlots
  -> FilePath
  -> Stream (Of (ABlock ByteString)) (ExceptT ParseError ResIO) ()
parseEpochFile epochSlots =
  S.mapMaybe eitherToMaybe . parseEpochFileWithBoundary epochSlots
 where
  eitherToMaybe
    :: ABlockOrBoundary ByteString -> Maybe (ABlock ByteString)
  eitherToMaybe = \case
    ABOBBoundary _      -> Nothing
    ABOBBlock    aBlock -> Just aBlock

parseEpochFileWithBoundary
  :: EpochSlots
  -> FilePath
  -> Stream
       (Of (ABlockOrBoundary ByteString))
       (ExceptT ParseError ResIO)
       ()
parseEpochFileWithBoundary epochSlots file = do
  s <- S.mapM liftDecoderError
    $ decodedWith (getSlotData epochSlots isEpochZero) (boundaryBytes <> bytes)
  liftBinaryError s
 where
  boundaryBytes :: SBS.ByteString (ExceptT ParseError ResIO) ()
  boundaryBytes = do
    let boundaryFile = file -<.> "boundary"
    boundaryExists <- liftIO $ doesFileExist boundaryFile
    when boundaryExists $ SBS.readFile boundaryFile

  bytes = loadFileWithHeader file epochHeader

  isEpochZero :: Bool
  isEpochZero =
    all (== '0') $ takeWhile isDigit (takeFileName file)

  liftDecoderError :: Either DecoderError a -> ExceptT ParseError ResIO a
  liftDecoderError = \case
    Right a   -> pure a
    Left  err -> throwError (ParseErrorDecoder err)

  liftBinaryError
    :: (a, B.ByteOffset, Either String ())
    -> Stream
         (Of (ABlockOrBoundary ByteString))
         (ExceptT ParseError ResIO)
         ()
  liftBinaryError = \case
    (_, _, Right ()) -> pure ()
    (_, offset, Left message) ->
      throwError (ParseErrorBinary file offset (toS message))

parseEpochFilesWithBoundary
  :: EpochSlots
  -> [FilePath]
  -> Stream
       (Of (ABlockOrBoundary ByteString))
       (ExceptT ParseError ResIO)
       ()
parseEpochFilesWithBoundary epochSlots fs =
  foldr (<>) mempty (parseEpochFileWithBoundary epochSlots <$> fs)

parseEpochFiles
  :: EpochSlots
  -> [FilePath]
  -> Stream (Of (ABlock ByteString)) (ExceptT ParseError ResIO) ()
parseEpochFiles epochSlots fs =
  foldr (<>) mempty (parseEpochFile epochSlots <$> fs)

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

getSlotData :: EpochSlots -> Bool -> B.Get (Either DecoderError (ABlockOrBoundary ByteString))
getSlotData epochSlots isEpochZero = runExceptT $ do
  header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
  lift $ guard (header == slotDataHeader)
  blockSize <- lift getWord32be
  undoSize  <- lift getWord32be
  block     <- do
    blockBytes <- lift $ B.getLazyByteString (fromIntegral blockSize)
    bb         <- ExceptT . pure $ decodeFullDecoder
      "ABlockOrBoundary"
      (fromCBORABlockOrBoundary epochSlots isEpochZero)
      blockBytes
    pure $ map (LBS.toStrict . slice blockBytes) bb
  -- Drop the Undo bytes as we no longer use these
  void . lift $ B.getLazyByteString (fromIntegral undoSize)
  pure block
