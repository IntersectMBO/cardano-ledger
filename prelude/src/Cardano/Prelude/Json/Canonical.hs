{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances used in the canonical JSON encoding of `GenesisData`

module Cardano.Prelude.Json.Canonical
       ( SchemaError(..)
       ) where

import           Cardano.Prelude.Base

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Text.Lazy.Builder as Builder (fromText)
import           Formatting.Buildable (Buildable (build))
import           Text.JSON.Canonical (FromJSON (fromJSON),
                     JSValue (JSNum, JSString), ReportSchemaErrors (expected),
                     ToJSON (toJSON), expectedButGotValue)

import           Cardano.Prelude.Json.Parse (parseJSString)


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
    , seActual   = fmap fromString actual
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

instance ReportSchemaErrors m => FromJSON m Int32 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Int32" val

instance ReportSchemaErrors m => FromJSON m Word16 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Word16" val

instance ReportSchemaErrors m => FromJSON m Word32 where
  fromJSON (JSNum i) = pure . fromIntegral $ i
  fromJSON val       = expectedButGotValue "Word32" val

instance ReportSchemaErrors m => FromJSON m Word64 where
  fromJSON = parseJSString readEither

instance ReportSchemaErrors m => FromJSON m Integer where
  fromJSON = parseJSString readEither

instance MonadError SchemaError m => FromJSON m Natural where
  fromJSON = parseJSString readEither
