module Cardano.Util.Error
       ( toAesonError
       , aesonError
       , toCborError
       , cborError
       ) where

import           Cardano.Prelude
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.Aeson.Types as A

-- | Convert an 'Either'-encoded failure to an 'aeson' parser failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toAesonError :: Either Text a -> A.Parser a
toAesonError = either (fail . toString) pure

aesonError :: Text -> A.Parser a
aesonError = toAesonError . Left

-- | Convert an 'Either'-encoded failure to a 'cborg' decoder failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toCborError :: Either Text a -> CBOR.Decoder s a
toCborError = either (fail . toString) pure

cborError :: Text -> CBOR.Decoder s a
cborError = toCborError . Left
