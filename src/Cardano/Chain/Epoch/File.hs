{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.File
  ( parseEpochFile
  , parseEpochFiles
  , ParseError (..)
  ) where

import           Control.Monad (guard)
import           Control.Monad.Except (MonadError (..), runExceptT)
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Resource (ResIO)
import qualified Data.Binary as B
import           Data.Binary.Get (getWord32be)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Streaming as SBS
import           Streaming.Binary (decodedWith)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S


import           Cardano.Binary.Class (DecoderError, decodeFull,
                     decodeFullDecoder)
import           Cardano.Chain.Block.Block (Block, decodeBlock)
import           Cardano.Chain.Block.Undo (Undo)
import           Cardano.Prelude

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

loadFileWithHeader :: FilePath -> LBS.ByteString -> SBS.ByteString (ExceptT ParseError ResIO) ()
loadFileWithHeader file header =
  let bytes = SBS.readFile file
      len = fromIntegral $ LBS.length header
  in do
    (h :> rest) <- lift $ SBS.toLazy $ SBS.splitAt len bytes
    if h == header
      then rest
      else lift $ throwError (ParseErrorMissingHeader file)

parseEpochFile :: FilePath -> Stream (Of (Block, Undo)) (ExceptT ParseError ResIO) ()
parseEpochFile file = do
  s <- S.mapMaybe sequenceMaybe $ S.mapM liftDecoderError $ decodedWith
    getSlotData
    bytes
  liftBinaryError s
 where
  bytes         = loadFileWithHeader file epochHeader

  sequenceMaybe = \case
    (Nothing, _) -> Nothing
    (Just b , u) -> Just (b, u)

  liftDecoderError = \case
    Right a   -> pure a
    Left  err -> throwError (ParseErrorDecoder err)

  liftBinaryError = \case
    (_, _, Right ()) -> pure ()
    (_, offset, Left message) ->
      throwError (ParseErrorBinary file offset (toS message))

parseEpochFiles :: [FilePath] -> Stream (Of (Block, Undo)) (ExceptT ParseError ResIO) ()
parseEpochFiles fs = foldr (<>) mempty (parseEpochFile <$> fs)

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

getSlotData :: B.Get (Either DecoderError (Maybe Block, Undo))
getSlotData = runExceptT $ do
  header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
  lift $ guard (header == slotDataHeader)
  blockSize <- lift getWord32be
  undoSize  <- lift getWord32be
  block     <-
    ExceptT $ decodeFullDecoder "Block" decodeBlock <$> B.getLazyByteString
      (fromIntegral blockSize)
  undo <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral undoSize)
  pure (block, undo)
