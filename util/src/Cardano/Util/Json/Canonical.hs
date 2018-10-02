{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Canonical encoding of 'GenesisData'.

module Cardano.Util.Json.Canonical
       ( SchemaError(..)
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import qualified Data.Text.Lazy.Builder as Builder (fromText)
import           Data.Time.Units (Millisecond)
import           Formatting.Buildable (Buildable (..))
import           Text.JSON.Canonical (FromJSON (..), JSValue (..),
                     ReportSchemaErrors (expected), ToJSON (..),
                     expectedButGotValue)

import           Cardano.Util.Json.Parse (tryParseString)

data SchemaError = SchemaError
    { seExpected :: !Text
    , seActual   :: !(Maybe Text)
    } deriving (Show)

instance Buildable SchemaError where
    build se = mconcat
        [ "expected " <> Builder.fromText (seExpected se)
        , case seActual se of
            Nothing     -> mempty
            Just actual -> " but got " <> Builder.fromText actual
        ]

instance
    (Applicative m, Monad m, MonadError SchemaError m)
    => ReportSchemaErrors m
  where
    expected expec actual = throwError SchemaError
        { seExpected = fromString expec
        , seActual = fmap fromString actual
        }

instance Monad m => ToJSON m Int32 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word16 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word32 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word64 where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Integer where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Natural where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Millisecond where
    toJSON = toJSON . toInteger

instance (ReportSchemaErrors m) => FromJSON m Int32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Int32" val

instance (ReportSchemaErrors m) => FromJSON m Word16 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Word16" val

instance (ReportSchemaErrors m) => FromJSON m Word32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Word32" val

instance (ReportSchemaErrors m) => FromJSON m Word64 where
    fromJSON = tryParseString readEither

instance (ReportSchemaErrors m) => FromJSON m Integer where
    fromJSON = tryParseString readEither

instance MonadError SchemaError m => FromJSON m Natural where
    fromJSON = tryParseString readEither

instance ReportSchemaErrors m => FromJSON m Millisecond where
    fromJSON = fmap fromInteger . fromJSON
