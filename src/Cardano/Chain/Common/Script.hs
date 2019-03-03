{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Common.Script
  ( Script(..)
  , ScriptVersion
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON(..), ToJSON(toJSON), object, withObject, (.:), (.=))
import Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import Formatting (bprint, int)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script
    { scrVersion :: ScriptVersion -- ^ Version
    , scrScript  :: ByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic)
      deriving anyclass NFData

instance B.Buildable Script where
    build script = bprint ("<script v".int.">") (scrVersion script)

instance ToJSON Script where
    toJSON script = object
        [ "version" .= scrVersion script
        , "script" .= makeByteString64 (scrScript script)
        ]

instance FromJSON Script where
    parseJSON = withObject "Script" $ \obj -> Script
        <$> obj .: "version"
        <*> (getByteString64 <$> obj .: "script")

instance Bi Script where
    encode script = encodeListLen 2
        <> encode (scrVersion script)
        <> encode (scrScript script)

    decode = do
        enforceSize "Script" 2
        Script <$> decode <*> decode
