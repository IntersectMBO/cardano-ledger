-- | Passing through errors to external libraries that use @MonadFail@

module Cardano.Prelude.Error
       ( toAesonError
       , aesonError
       , toCborError
       , cborError
       ) where

import           Cardano.Prelude.Base

import qualified Codec.CBOR.Decoding as CBOR
import           Control.Monad (fail)
import qualified Data.Aeson.Types as A
import           Formatting (build, formatToString)
import           Formatting.Buildable (Buildable)


-- | Convert an 'Either'-encoded error to an 'aeson' parser error
toAesonError :: Buildable e => Either e a -> A.Parser a
toAesonError = either aesonError pure

-- | Convert a @Buildable@ error into an 'aeson' parser error
aesonError :: Buildable e => e -> A.Parser a
aesonError = fail . formatToString build

-- | Convert an 'Either'-encoded failure to a 'cborg' decoder failure
toCborError :: Buildable e => Either e a -> CBOR.Decoder s a
toCborError = either cborError pure

-- | Convert a @Buildable@ error into a 'cborg' decoder error
cborError :: Buildable e => e -> CBOR.Decoder s a
cborError = fail . formatToString build
