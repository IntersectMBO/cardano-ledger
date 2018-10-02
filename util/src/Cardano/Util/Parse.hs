{-# LANGUAGE OverloadedStrings #-}

module Cardano.Util.Parse
       ( parseBase16
       ) where

import           Cardano.Prelude

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS

parseBase16 :: Text -> Either Text ByteString
parseBase16 s = do
    let (bs, suffix) = B16.decode . fromString $ toString s
    unless (BS.null suffix)
        .  Left
        $  "Base16 decoding failed with incorrect suffix: "
        <> fromString (BS.unpack suffix)
    pure bs
