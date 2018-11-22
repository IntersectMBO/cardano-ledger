{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.File
  ( parseEpochFile
  , parseEpochFiles
  , parseEpochFileWithBoundary
  , parseEpochFilesWithBoundary
  , ParseError(..)
  )
where

import Control.Monad (guard)
import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Resource (ResIO)
import qualified Data.Binary as B
import Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Streaming as SBS
import Data.String (String)
import Streaming.Binary (decodedWith)
import Streaming.Prelude (Of(..), Stream)
import qualified Streaming.Prelude as S

import Cardano.Binary.Class (DecoderError, decodeFull, decodeFullDecoder, slice)
import Cardano.Chain.Block.Block (ABlock, decodeABlockOrBoundary)
import Cardano.Chain.Block.Undo (Undo, ABlund)
import Cardano.Prelude

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

parseEpochFile
  :: FilePath -> Stream (Of (ABlund ByteString)) (ExceptT ParseError ResIO) ()
parseEpochFile = S.mapMaybe eitherToMaybe . parseEpochFileWithBoundary
 where
  eitherToMaybe
    :: (Either ByteString (ABlock ByteString), Undo)
    -> Maybe (ABlund ByteString)
  eitherToMaybe = \case
    (Left  _     , _   ) -> Nothing
    (Right aBlock, undo) -> Just (aBlock, undo)

parseEpochFileWithBoundary
  :: FilePath
  -> Stream
       (Of (Either ByteString (ABlock ByteString), Undo))
       (ExceptT ParseError ResIO)
       ()
parseEpochFileWithBoundary file = do
  s <- S.mapM liftDecoderError $ decodedWith getSlotData bytes
  liftBinaryError s
 where
  bytes = loadFileWithHeader file epochHeader

  liftDecoderError :: Either DecoderError a -> ExceptT ParseError ResIO a
  liftDecoderError = \case
    Right a   -> pure a
    Left  err -> throwError (ParseErrorDecoder err)

  liftBinaryError
    :: (a, B.ByteOffset, Either String ())
    -> Stream
         (Of (Either ByteString (ABlock ByteString), Undo))
         (ExceptT ParseError ResIO)
         ()
  liftBinaryError = \case
    (_, _, Right ()) -> pure ()
    (_, offset, Left message) ->
      throwError (ParseErrorBinary file offset (toS message))

parseEpochFilesWithBoundary
  :: [FilePath]
  -> Stream
       (Of (Either ByteString (ABlock ByteString), Undo))
       (ExceptT ParseError ResIO)
       ()
parseEpochFilesWithBoundary fs =
  foldr (<>) mempty (parseEpochFileWithBoundary <$> fs)

parseEpochFiles
  :: [FilePath] -> Stream (Of (ABlund ByteString)) (ExceptT ParseError ResIO) ()
parseEpochFiles fs = foldr (<>) mempty (parseEpochFile <$> fs)

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

getSlotData
  :: B.Get (Either DecoderError (Either ByteString (ABlock ByteString), Undo))
getSlotData = runExceptT $ do
  header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
  lift $ guard (header == slotDataHeader)
  blockSize <- lift getWord32be
  undoSize  <- lift getWord32be
  block     <- do
    blockBytes <- lift $ B.getLazyByteString (fromIntegral blockSize)
    bb         <- ExceptT . pure $ decodeFullDecoder
      "Block"
      decodeABlockOrBoundary
      blockBytes
    let sliceBytes = LBS.toStrict . slice blockBytes
    pure $ bimap sliceBytes (fmap sliceBytes) bb
  undo <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral undoSize)
  pure (block, undo)
