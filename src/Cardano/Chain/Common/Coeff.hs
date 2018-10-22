{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Common.Coeff
       ( Coeff (..)
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson
import           Data.Fixed (Fixed (..), Nano, resolution, showFixed)
import qualified Data.Text.Lazy.Builder as Builder
import           Formatting.Buildable (Buildable (..))
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..))

import           Cardano.Binary.Class (Bi (..))


-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Ord, Show, Generic, NFData, Num)

instance Buildable Coeff where
    build (Coeff x) = Builder.fromString (showFixed True x)

instance Bi Coeff where
    encode (Coeff n) = encode n
    decode = Coeff <$> decode @Nano

instance Monad m => ToJSON m Coeff where
    toJSON (Coeff (MkFixed integer)) = toJSON @_ @Integer integer

instance MonadError SchemaError m => FromJSON m Coeff where
    fromJSON = fmap (Coeff . MkFixed) . fromJSON @_ @Integer

instance Aeson.ToJSON Coeff where
    toJSON (Coeff v) = Aeson.toJSON (realToFrac @_ @Double v)

instance Aeson.FromJSON Coeff where
    parseJSON = Aeson.withScientific "Coeff" $ \sc -> do
        -- Code below is resistant to changes in precision of 'Coeff'.
        let
            rat = toRational sc * toRational res
            fxd = MkFixed (numerator rat)
            res = resolution fxd
            bad = denominator rat /= 1
        when bad $ aesonError @Text "Fixed precision for coefficient exceeded"
        return $ Coeff fxd
