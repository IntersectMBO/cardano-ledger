{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.DebugTools where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, decodeFull', showDecoderError)
import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Core (eraProtVerLow)
import qualified Data.ByteString as BS

readCBORFromFile :: DecCBOR a => FilePath -> IO a
readCBORFromFile path = do
  dat <- BS.readFile path
  case decodeFull' (eraProtVerLow @Conway) dat of
    Right x -> pure x
    Left err -> error $ showDecoderError err

writeCBORFromFile :: EncCBOR a => FilePath -> a -> IO ()
writeCBORFromFile path val = BS.writeFile path $ serialize' (eraProtVerLow @Conway) val
