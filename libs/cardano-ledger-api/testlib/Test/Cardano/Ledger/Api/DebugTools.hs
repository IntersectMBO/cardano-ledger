{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.DebugTools where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, Version, decodeFull')
import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Core (Era, eraProtVerLow)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS

readCBOR :: (DecCBOR a, MonadIO m) => Version -> FilePath -> m a
readCBOR version path = liftIO $ do
  dat <- BS.readFile path
  case decodeFull' version dat of
    Right x -> pure x
    Left err -> throwIO err

writeCBOR :: (EncCBOR a, MonadIO m) => Version -> FilePath -> a -> m ()
writeCBOR version path = liftIO . BS.writeFile path . serialize' version

readEraCBOR :: forall t era m. (Era era, DecCBOR (t era), MonadIO m) => FilePath -> m (t era)
readEraCBOR = readCBOR (eraProtVerLow @era)

writeEraCBOR :: forall t era m. (Era era, EncCBOR (t era), MonadIO m) => FilePath -> t era -> m ()
writeEraCBOR = writeCBOR (eraProtVerLow @era)
