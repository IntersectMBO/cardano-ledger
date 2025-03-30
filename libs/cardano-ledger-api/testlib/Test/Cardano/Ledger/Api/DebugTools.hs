{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.DebugTools where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecShareCBOR,
  EncCBOR,
  Version,
  decNoShareCBOR,
  decodeFull',
  decodeFullDecoder',
 )
import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Core (Era, eraProtVerLow)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Test.Cardano.Ledger.Binary.Annotator (decodeFullAnnotator)

readCBORWith ::
  (MonadIO m, Exception e) => (Version -> BS.ByteString -> Either e a) -> Version -> FilePath -> m a
readCBORWith dec version path = liftIO $ do
  dat <- BS.readFile path
  case dec version dat of
    Right x -> pure x
    Left err -> throwIO err

readCBOR :: (DecCBOR a, MonadIO m) => Version -> FilePath -> m a
readCBOR = readCBORWith decodeFull'

readCBORNoShare :: (MonadIO m, DecShareCBOR a) => Version -> FilePath -> m a
readCBORNoShare = readCBORWith (\v bs -> decodeFullDecoder' v "DecodeNoShare" decNoShareCBOR bs)

readCBORAnnotated :: (MonadIO m, DecCBOR (Annotator a)) => Version -> FilePath -> m a
readCBORAnnotated = readCBORWith (\v bs -> decodeFullAnnotator v "DecodeAnnotated" decCBOR (LBS.fromStrict bs))

writeCBOR :: (EncCBOR a, MonadIO m) => Version -> FilePath -> a -> m ()
writeCBOR version path = liftIO . BS.writeFile path . serialize' version

readEraCBOR :: forall t era m. (Era era, DecCBOR (t era), MonadIO m) => FilePath -> m (t era)
readEraCBOR = readCBOR (eraProtVerLow @era)

writeEraCBOR :: forall t era m. (Era era, EncCBOR (t era), MonadIO m) => FilePath -> t era -> m ()
writeEraCBOR = writeCBOR (eraProtVerLow @era)
