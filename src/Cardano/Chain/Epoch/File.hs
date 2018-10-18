{-# LANGUAGE FlexibleContexts  #-}
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


import           Cardano.Binary.Class (DecoderError, decodeFull)
import           Cardano.Chain.Block.Block (Block)
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
  | ParseErrorBinary !FilePath !B.ByteOffset !String
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
parseEpochFile file =
  let bytes = loadFileWithHeader file epochHeader
      liftDecoderError (Right a) = pure a
      liftDecoderError (Left e)  = throwError (ParseErrorDecoder e)
      liftBinaryError (_,_,Right ()) = pure ()
      liftBinaryError (_,offset,Left message) = throwError (ParseErrorBinary file offset message)
  in S.mapM liftDecoderError (decodedWith getSlotData bytes) >>= liftBinaryError

parseEpochFiles :: [FilePath] -> Stream (Of (Block, Undo)) (ExceptT ParseError ResIO) ()
parseEpochFiles fs = foldr (<>) mempty (parseEpochFile <$> fs)

slotDataHeader :: LBS.ByteString
slotDataHeader = "blnd"

getSlotData :: B.Get (Either DecoderError (Block, Undo))
getSlotData = runExceptT $ do
  header <- lift $ B.getLazyByteString (LBS.length slotDataHeader)
  lift $ guard (header == slotDataHeader)
  blockSize <- lift getWord32be
  undoSize <- lift getWord32be
  block <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral blockSize)
  undo  <- ExceptT $ decodeFull <$> B.getLazyByteString (fromIntegral undoSize)
  pure $ (block, undo)
