{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Common.Script
       ( Script (..)
       , ScriptVersion
       , Script_v0
       ) where

import           Cardano.Prelude

import           Data.Aeson (FromJSON (..), ToJSON (toJSON), object, withObject,
                     (.:), (.=))
import           Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import           Formatting (bprint, int, (%))
import qualified Formatting.Buildable as B
import qualified PlutusCore.Program as PLCore

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script
    { scrVersion :: ScriptVersion -- ^ Version
    , scrScript  :: ByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)
      deriving anyclass NFData

instance B.Buildable Script where
    build script = bprint ("<script v"%int%">") (scrVersion script)

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

-- | Deserialized script (i.e. an AST), version 0.
type Script_v0 = PLCore.Program
