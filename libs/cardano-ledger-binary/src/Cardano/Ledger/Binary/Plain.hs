{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Module that re-exports everythign from `cardano-binary` package.
--
-- Everything that gets defined in this module should most likely be migrated to
-- `cardano-binary` package.
module Cardano.Ledger.Binary.Plain (
  module Cardano.Binary,
  module Codec.CBOR.Term,
  showDecoderError,
  invalidKey,
  decodeRecordNamed,
  decodeRecordNamedT,
  decodeRecordSum,
  decodeListLikeT,
  serializeAsHexText,
  decodeFullFromHexText,
  encodeEnum,
  decodeEnumBounded,
  withHexText,

  -- * DSIGN
  C.encodeVerKeyDSIGN,
  C.decodeVerKeyDSIGN,
  C.encodeSignKeyDSIGN,
  C.decodeSignKeyDSIGN,
  C.encodeSigDSIGN,
  C.decodeSigDSIGN,
  C.encodeSignedDSIGN,
  C.decodeSignedDSIGN,

  -- * KES
  C.encodeVerKeyKES,
  C.decodeVerKeyKES,
  C.encodeSignKeyKES,
  C.decodeSignKeyKES,
  C.encodeSigKES,
  C.decodeSigKES,
  C.encodeSignedKES,
  C.decodeSignedKES,

  -- * VRF
  C.encodeVerKeyVRF,
  C.decodeVerKeyVRF,
  C.encodeSignKeyVRF,
  C.decodeSignKeyVRF,
  C.encodeCertVRF,
  C.decodeCertVRF,
)
where

import Cardano.Binary hiding (encodedSizeExpr)
import qualified Cardano.Crypto.DSIGN.Class as C
import qualified Cardano.Crypto.KES.Class as C
import qualified Cardano.Crypto.VRF.Class as C
import Codec.CBOR.Term
import Control.Monad (unless)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Identity (IdentityT (runIdentityT))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable
import Formatting (build, formatToString)
import qualified Formatting.Buildable as B (Buildable (..))

showDecoderError :: B.Buildable e => e -> String
showDecoderError = formatToString build

-- | Encode a type as CBOR and encode it as base16
serializeAsHexText :: ToCBOR a => a -> Text.Text
serializeAsHexText = Text.decodeLatin1 . B16.encode . serialize'

withHexText :: (ByteString -> Either DecoderError b) -> Text.Text -> Either DecoderError b
withHexText f txt =
  case B16.decode (Text.encodeUtf8 txt) of
    Left err -> Left $ DecoderErrorCustom "Invalid Hex encoding:" (Text.pack err)
    Right bs -> f bs

-- | Try decoding base16 encode bytes and then try to decoding them as CBOR
decodeFullFromHexText :: FromCBOR a => Text.Text -> Either DecoderError a
decodeFullFromHexText = withHexText decodeFull'

-- | Report an error when a numeric key of the type constructor doesn't match.
invalidKey :: MonadFail m => Word -> m a
invalidKey k = cborError $ DecoderErrorCustom "Not a valid key:" (Text.pack $ show k)

decodeRecordNamed :: Text.Text -> (a -> Int) -> Decoder s a -> Decoder s a
decodeRecordNamed name getRecordSize decoder =
  runIdentityT $ decodeRecordNamedT name getRecordSize (lift decoder)
{-# INLINE decodeRecordNamed #-}

decodeRecordNamedT ::
  (MonadTrans m, Monad (m (Decoder s))) =>
  Text.Text ->
  (a -> Int) ->
  m (Decoder s) a ->
  m (Decoder s) a
decodeRecordNamedT name getRecordSize decoder =
  decodeListLikeT name decoder $ \result n ->
    lift $ matchSize ("Record " <> name) n (getRecordSize result)
{-# INLINE decodeRecordNamedT #-}

decodeRecordSum :: Text.Text -> (Word -> Decoder s (Int, a)) -> Decoder s a
decodeRecordSum name decoder =
  runIdentityT $
    snd <$> do
      decodeListLikeT name (lift (decodeWord >>= decoder)) $ \(size, _) n ->
        lift $ matchSize (Text.pack "Sum " <> name) size n
{-# INLINE decodeRecordSum #-}

decodeListLikeT ::
  (MonadTrans m, Monad (m (Decoder s))) =>
  -- | Name for error reporting
  Text.Text ->
  -- | Decoder for the datastructure itself
  m (Decoder s) a ->
  -- | In case when length was provided, act upon it.
  (a -> Int -> m (Decoder s) ()) ->
  m (Decoder s) a
decodeListLikeT name decoder actOnLength = do
  lenOrIndef <- lift decodeListLenOrIndef
  result <- decoder
  case lenOrIndef of
    Just n -> actOnLength result n
    Nothing -> lift $ do
      isBreak <- decodeBreakOr
      unless isBreak $ cborError $ DecoderErrorCustom name "Excess terms in array"
  pure result
{-# INLINE decodeListLikeT #-}

encodeEnum :: Enum a => a -> Encoding
encodeEnum = encodeInt . fromEnum

decodeEnumBounded :: forall a s. (Enum a, Bounded a, Typeable a) => Decoder s a
decodeEnumBounded = do
  n <- decodeInt
  if fromEnum (minBound :: a) <= n && n <= fromEnum (maxBound :: a)
    then pure $ toEnum n
    else fail $ "Failed to decode an Enum: " <> show n <> " for TypeRep: " <> show (typeRep (Proxy @a))
{-# INLINE decodeEnumBounded #-}
