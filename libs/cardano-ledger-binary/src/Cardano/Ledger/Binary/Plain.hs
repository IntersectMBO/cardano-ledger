{-# LANGUAGE OverloadedStrings #-}

-- | Module that re-exports everythign from `cardano-binary` package.
--
-- Everything that gets defined in this module should most likely be migrated to
-- `cardano-binary` package.
module Cardano.Ledger.Binary.Plain (
  module Cardano.Binary,
  invalidKey,
  decodeRecordNamed,
  decodeRecordSum,
  decodeListLike,
)
where

import Cardano.Binary
import Control.Monad (unless)
import qualified Data.Text as Text

-- | Report an error when a numeric key of the type constructor doesn't match.
invalidKey :: MonadFail m => Word -> m a
invalidKey k = cborError $ DecoderErrorCustom "Not a valid key:" (Text.pack $ show k)

decodeRecordNamed :: Text.Text -> (a -> Int) -> Decoder s a -> Decoder s a
decodeRecordNamed name getRecordSize decoder =
  decodeListLike name decoder $ \result n ->
    matchSize ("Record " <> name) n (getRecordSize result)
{-# INLINEABLE decodeRecordNamed #-}

decodeRecordSum :: Text.Text -> (Word -> Decoder s (Int, a)) -> Decoder s a
decodeRecordSum name decoder =
  snd <$> do
    decodeListLike name (decodeWord >>= decoder) $ \(size, _) n ->
      matchSize (Text.pack "Sum " <> name) size n
{-# INLINEABLE decodeRecordSum #-}

decodeListLike ::
  -- | Name for error reporting
  Text.Text ->
  -- | Decoder for the datastructure itself
  Decoder s a ->
  -- | In case when length was provided, act upon it.
  (a -> Int -> Decoder s ()) ->
  Decoder s a
decodeListLike name decoder actOnLength = do
  lenOrIndef <- decodeListLenOrIndef
  result <- decoder
  case lenOrIndef of
    Just n -> actOnLength result n
    Nothing -> do
      isBreak <- decodeBreakOr
      unless isBreak $ cborError $ DecoderErrorCustom name "Excess terms in array"
  pure result
{-# INLINEABLE decodeListLike #-}
