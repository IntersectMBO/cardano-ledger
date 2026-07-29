{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.CanonicalState.Conformance (
  propReferenceAcceptsCBOR,
  FailureInfo (..),
  Direction (..),
) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (Evidenced (..), SValidity (..), showValidationTrace)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Custom.Generator (GenConfig (..), runCBORGen)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.AntiGen (runAntiGen)
import Test.QuickCheck (generate)

-- | Direction of conformance test
data Direction
  = -- | Reference CDDL generated the CBOR, Huddle validated it
    HuddleValidating
  | -- | Huddle generated the CBOR, Reference CDDL validated it
    ReferenceValidating
  deriving (Show, Eq)

-- | Information about a validation failure
data FailureInfo = FailureInfo
  { failDirection :: Direction
  , failCborHex :: Text
  , failErrorMessage :: Text
  }
  deriving (Show, Eq)

generateCBORFromCDDL ::
  -- | CDDL spec
  CTreeRoot MonoReferenced ->
  IO BS.ByteString
generateCBORFromCDDL spec = do
  term <-
    generate . runAntiGen $
      runCBORGen (GenConfig {gcTwiddle = False, gcRoot = mapIndex spec}) . generateFromName $
        Name (T.pack "record_entry")
  pure $ toStrictByteString $ encodeTerm term

-- | Test if a reference CDDL accepts CBOR generated from another spec
propReferenceAcceptsCBOR ::
  -- | CDDL spec to generate CBOR from
  CTreeRoot MonoReferenced ->
  -- | Reference CDDL spec to validate against
  CTreeRoot MonoReferenced ->
  -- | Direction of testing
  Direction ->
  IO (Either FailureInfo ())
propReferenceAcceptsCBOR genSpec validateSpec direction = do
  cbor <- generateCBORFromCDDL genSpec

  let result = validateCBOR cbor (Name (T.pack "record_entry")) (mapIndex validateSpec)
  case result of
    Left e -> pure $ Left $ FailureInfo direction (TE.decodeUtf8 $ Base16.encode cbor) (T.pack $ show e)
    Right (Evidenced SValid _) ->
      pure $ Right ()
    Right (Evidenced SInvalid trc) ->
      pure $
        Left $
          FailureInfo direction (TE.decodeUtf8 $ Base16.encode cbor) (T.pack $ showValidationTrace trc)
