{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripCborSpec,
  huddleRoundTripAnnCborSpec,
  huddleAntiCborSpec,
  writeSpec,
  huddleRoundTripGenValidate,
  huddleRoundTripArbitraryValidate,
  huddleDecoderEquivalenceProp,
  huddleRoundTripCborProp,
  huddleRoundTripAnnCborProp,
  huddleAntiCborProp,
  huddleRoundTripGenValidateProp,
  specWithHuddle,
  noTwiddle,
  HuddleEnv (..),
  toGenConfig,
  resolveHuddle,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR,
  EncCBOR,
  Version,
  decodeFull',
  decodeFullAnnotator,
  decodeFullDecoder,
  encodeTerm,
  serialize',
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Decoding (label)
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (ValidateCBORError (..), validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  Evidenced (..),
  SValidity (..),
  TraceOptions (..),
  ValidationTrace,
  defaultTraceOptions,
  prettyValidationTrace,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Custom.Generator (GenConfig (..), runCBORGen)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import qualified Codec.CBOR.Cuddle.CDDL.Resolve as Cuddle
import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import Codec.CBOR.Cuddle.IndexMappable
import Codec.CBOR.Cuddle.Pretty (PrettyStage, renderCDDL)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Pretty (prettyHexEnc)
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack (HasCallStack)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Terminal as Ansi
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), hPutStrLn, withFile)
import Test.AntiGen (ZapResult (..), prettyZapResult, runAntiGen, zapAntiGenResult)
import Test.Cardano.Ledger.Binary (decoderEquivalenceExpectation)
import Test.Cardano.Ledger.Binary.RoundTrip (
  RoundTripFailure (RoundTripFailure),
  Trip (..),
  cborTrip,
  decodeAnnExtra,
  embedTripLabelExtra,
 )
import Test.Hspec (
  Expectation,
  Spec,
  SpecWith,
  beforeAll,
  describe,
  expectationFailure,
  it,
  mapSubject,
  shouldBe,
 )
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  Testable (..),
  counterexample,
  discard,
  forAll,
 )

huddleDecoderEquivalenceProp ::
  forall a.
  (HasCallStack, Eq a, Show a, DecCBOR a, DecCBOR (Annotator a)) =>
  Version ->
  T.Text ->
  HuddleEnv ->
  Property
huddleDecoderEquivalenceProp version ruleName env = property $ do
  term <- runAntiGen . runCBORGen (toGenConfig env) . generateFromName $ Name ruleName
  let encoding = CBOR.encodeTerm term
      initCborBytes = CBOR.toLazyByteString encoding
  pure $ decoderEquivalenceExpectation @a version initCborBytes

huddleDecoderEquivalenceSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, DecCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith HuddleEnv
huddleDecoderEquivalenceSpec version ruleName =
  let lbl = label $ Proxy @a
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $
        huddleDecoderEquivalenceProp @a version ruleName

huddleRoundTripCborProp ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  Version ->
  T.Text ->
  HuddleEnv ->
  Property
huddleRoundTripCborProp version ruleName env =
  let lbl = label $ Proxy @a
      trip = cborTrip @a
   in property $ do
        term <- runAntiGen . runCBORGen (toGenConfig env) . generateFromName $ Name ruleName
        pure $ roundTripExample lbl version version trip term

huddleRoundTripCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith HuddleEnv
huddleRoundTripCborSpec version ruleName =
  let lbl = label $ Proxy @a
   in describe "Generate bytestring from CDDL and decode -> encode" $
        it (T.unpack ruleName <> ": " <> T.unpack lbl) $
          huddleRoundTripCborProp @a version ruleName

huddleRoundTripAnnCborProp ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  Version ->
  T.Text ->
  HuddleEnv ->
  Property
huddleRoundTripAnnCborProp version ruleName env =
  let lbl = label $ Proxy @(Annotator a)
      trip = cborTrip @a
   in property $ do
        term <- runAntiGen . runCBORGen (toGenConfig env) . generateFromName $ Name ruleName
        pure $ roundTripAnnExample lbl version version trip term

huddleRoundTripAnnCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith HuddleEnv
huddleRoundTripAnnCborSpec version ruleName =
  let lbl = label $ Proxy @(Annotator a)
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $
        huddleRoundTripAnnCborProp @a version ruleName

data HuddleEnv = HuddleEnv
  { heTwiddle :: Bool
  , heRoot :: CTreeRoot MonoReferenced
  }

toGenConfig :: HuddleEnv -> GenConfig
toGenConfig HuddleEnv {..} =
  GenConfig
    { gcTwiddle = heTwiddle
    , gcRoot = mapIndex heRoot
    }

huddleAntiCborProp ::
  forall a.
  DecCBOR a =>
  Version ->
  T.Text ->
  HuddleEnv ->
  Property
huddleAntiCborProp version ruleName env@HuddleEnv {heRoot} = property @(Gen Property) $ do
  mTerm <- zapAntiGenResult 1 . runCBORGen (toGenConfig env) . generateFromName $ Name ruleName
  case mTerm of
    zr@ZapResult {..}
      | zrZapped > 0 -> do
          let
            encoding = toPlainEncoding version $ encodeTerm zrValue
            bs = CBOR.toStrictByteString encoding
          case validateCBOR bs (Name ruleName) (mapIndex heRoot) of
            -- cborg-level failure (e.g. twiddler turned a tag into a
            -- bignum tag with a non-bytes body): the decoder will also
            -- fail, which is exactly what a zap test is meant to detect.
            --
            -- It could also be argued that the correct thing to do here would
            -- be to discard instead, since we're not really testing our decoders
            -- but `cborg`'s decoders.
            Left (DecodingFailed _) -> pure $ property ()
            Left e@LeftoverBytes {} -> pure . property . expectationFailure $ show e
            Left e@RuleDoesNotExist {} -> pure . property . expectationFailure $ show e
            Right (Evidenced SInvalid trc) -> do
              let
                errMsg =
                  unlines
                    [ "Generated term:"
                    , prettyHexEnc encoding
                    , mempty
                    , "Validation result:"
                    , T.unpack . Ansi.renderStrict . layoutPretty defaultLayoutOptions $
                        prettyValidationTrace (defaultTraceOptions {toFoldValid = True}) trc
                    , mempty
                    , T.unpack $ prettyZapResult zr
                    , mempty
                    , "Decoding succeeded, expected failure"
                    ]
              pure . counterexample errMsg . isLeft $ decodeFull' @a version bs
            Right (Evidenced SValid _) -> discard
      | otherwise -> discard

huddleAntiCborSpec ::
  forall a.
  DecCBOR a =>
  Version ->
  T.Text ->
  SpecWith HuddleEnv
huddleAntiCborSpec version ruleName =
  let lbl = label $ Proxy @a
   in describe "Decoding fails when term is zapped"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ huddleAntiCborProp @a version ruleName

specWithHuddle :: Cuddle.Huddle -> SpecWith HuddleEnv -> Spec
specWithHuddle h =
  beforeAll $
    case resolveHuddle h of
      Right ct -> pure $ HuddleEnv True ct
      Left err -> error err

noTwiddle :: SpecWith HuddleEnv -> SpecWith HuddleEnv
noTwiddle = mapSubject $ \env -> env {heTwiddle = False}

-- | Verify that random data generated is:
--
-- * Decoded successfully into a Haskell type using the decoder in `Trip` and the version
--   supplied
--
-- * When reencoded produces a valid `FlatTerm`
--
-- * When decoded again from the bytes produced by the encoder matches the type exactly
--   when it was decoded from random bytes
roundTripExample ::
  (HasCallStack, Show a, Eq a) =>
  T.Text ->
  -- | Version to use for decoding
  Version ->
  -- | Version to use for encoding
  Version ->
  -- | Decode/encoder that needs tsting
  Trip a a ->
  -- | Randomly generated data and the CDDL spec
  CBOR.Term ->
  Expectation
roundTripExample lbl encVersion decVersion trip@Trip {tripDecoder} term =
  let
    encoding = CBOR.encodeTerm term
    initCborBytes = CBOR.toLazyByteString encoding
    mkFailure =
      RoundTripFailure encVersion decVersion encoding initCborBytes
   in
    case decodeFullDecoder decVersion lbl tripDecoder initCborBytes of
      Left decErr -> cddlFailure encoding $ mkFailure Nothing Nothing Nothing (Just decErr)
      Right val ->
        case embedTripLabelExtra lbl encVersion decVersion trip val of
          Right (val', _encoding, _encodedBytes) ->
            val' `shouldBe` val
          Left embedErr -> cddlFailure encoding embedErr

-- | Same as `roundTripExample`, but works for decoders that are wrapped into
-- `Annotator`
roundTripAnnExample ::
  (HasCallStack, Show a, Eq a) =>
  T.Text ->
  -- | Version to use for decoding
  Version ->
  -- | Version to use for encoding
  Version ->
  -- | Decode/encoder that needs tsting
  Trip a (Annotator a) ->
  -- | Randomly generated data and the CDDL spec
  CBOR.Term ->
  Expectation
roundTripAnnExample lbl encVersion decVersion Trip {tripEncoder, tripDecoder} term =
  let
    encoding = CBOR.encodeTerm term
    initCborBytes = CBOR.toLazyByteString encoding
    mkFailure =
      RoundTripFailure encVersion decVersion encoding initCborBytes
   in
    case decodeFullAnnotator decVersion lbl tripDecoder initCborBytes of
      Left decErr -> cddlFailure encoding $ mkFailure Nothing Nothing Nothing (Just decErr)
      Right val ->
        let enc = toPlainEncoding encVersion $ tripEncoder val
         in case decodeAnnExtra lbl encVersion decVersion tripDecoder enc of
              Right (val', _encodedBytes) ->
                val' `shouldBe` val
              Left embedErr -> cddlFailure encoding embedErr

cddlFailure :: HasCallStack => CBOR.Encoding -> RoundTripFailure -> Expectation
cddlFailure encoding err =
  expectationFailure $
    unlines
      [ "Failed Cddl RoundTrip verification:"
      , show err
      , "Generated diag: " <> CBOR.prettyHexEnc encoding
      ]

showValidationTrace :: Evidenced ValidationTrace -> String
showValidationTrace (Evidenced _ t) =
  T.unpack . Ansi.renderStrict . layoutPretty defaultLayoutOptions $
    prettyValidationTrace defaultTraceOptions t

huddleRoundTripGenValidateProp ::
  forall a.
  (Show a, EncCBOR a) =>
  Gen a ->
  Version ->
  T.Text ->
  HuddleEnv ->
  Property
huddleRoundTripGenValidateProp gen version ruleName HuddleEnv {heRoot = cddl} =
  property . forAll gen $
    \(val :: a) -> do
      let
        bs = serialize' version val
      case validateCBOR bs (Name ruleName) (mapIndex cddl) of
        Left e -> expectationFailure $ "Validation input error:\n" <> show e
        Right (Evidenced SValid _) -> pure ()
        Right res@(Evidenced SInvalid _) ->
          expectationFailure $ "CBOR Validation failed:\n" <> showValidationTrace res

huddleRoundTripGenValidate ::
  forall a.
  (DecCBOR a, Show a, EncCBOR a) => Gen a -> Version -> T.Text -> SpecWith HuddleEnv
huddleRoundTripGenValidate gen version ruleName =
  let lbl = label $ Proxy @a
   in describe "Encode an arbitrary value and check against CDDL"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ huddleRoundTripGenValidateProp @a gen version ruleName

huddleRoundTripArbitraryValidate ::
  forall a.
  ( DecCBOR a
  , EncCBOR a
  , Arbitrary a
  , Show a
  ) =>
  Version ->
  T.Text ->
  SpecWith HuddleEnv
huddleRoundTripArbitraryValidate = huddleRoundTripGenValidate $ arbitrary @a

--------------------------------------------------------------------------------
-- Writing specs to a file
--------------------------------------------------------------------------------

-- | Write a Huddle specification to a file at the given path
writeSpec :: Cuddle.Huddle -> FilePath -> IO ()
writeSpec hddl path = do
  let cddl = Cuddle.toCDDLNoRoot hddl
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode $ \h -> do
    hPutStrLn
      h
      "; This file was auto-generated using generate-cddl. Please do not modify it directly!\n"
    T.hPutStrLn h . renderCDDL defaultLayoutOptions $ mapIndex @_ @_ @PrettyStage cddl
  -- Write log to stdout
  putStrLn $ "Generated CDDL file at: " <> path

-- | Resolve a Huddle spec into a fully resolved CTree, suitable for CBOR generation.
resolveHuddle :: Cuddle.Huddle -> Either String (CTreeRoot MonoReferenced)
resolveHuddle h =
  case Cuddle.fullResolveCDDL (mapCDDLDropExt (Cuddle.toCDDL h)) of
    Right ct -> Right ct
    Left nrf -> Left (show nrf)
