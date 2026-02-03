{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripCborSpec,
  huddleRoundTripAnnCborSpec,
  huddleAntiCborSpec,
  writeSpec,
  huddleRoundTripGenValidate,
  huddleRoundTripArbitraryValidate,
  specWithHuddle,
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
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (ValidationResult (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import qualified Codec.CBOR.Cuddle.CDDL.Resolve as Cuddle
import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import Codec.CBOR.Cuddle.IndexMappable
import Codec.CBOR.Cuddle.Pretty (PrettyStage)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Pretty (prettyHexEnc)
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as C
import qualified Codec.CBOR.Write as CBOR
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), hPutStrLn, withFile)
import Test.AntiGen (runAntiGen, tryZapAntiGen)
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
  shouldBe,
  shouldSatisfy,
 )
import Test.Hspec.Core.Spec (Example (..), paramsQuickCheckArgs)
import Test.QuickCheck (Arbitrary (..), Args (replay), Gen, Testable (..), forAll)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (QCGen, mkQCGen)
import Text.Pretty.Simple (pShow)

huddleDecoderEquivalenceSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, DecCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleDecoderEquivalenceSpec version ruleName =
  let lbl = label $ Proxy @a
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $ \(mapIndex -> cddl) -> property $ do
        term <- runAntiGen . generateFromName cddl $ Name ruleName
        let encoding = CBOR.encodeTerm term
            initCborBytes = CBOR.toLazyByteString encoding
        pure $ decoderEquivalenceExpectation @a version initCborBytes

huddleRoundTripCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleRoundTripCborSpec version ruleName =
  let lbl = label $ Proxy @a
      trip = cborTrip @a
   in describe "Generate bytestring from CDDL and decode -> encode" $
        it (T.unpack ruleName <> ": " <> T.unpack lbl) $ \(mapIndex -> cddl) -> property $ do
          term <- runAntiGen . generateFromName cddl $ Name ruleName
          pure $ roundTripExample lbl version version trip term

huddleRoundTripAnnCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleRoundTripAnnCborSpec version ruleName =
  let lbl = label $ Proxy @(Annotator a)
      trip = cborTrip @a
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $ \(mapIndex -> cddl) -> property $ do
        term <- runAntiGen . generateFromName cddl $ Name ruleName
        pure $ roundTripAnnExample lbl version version trip term

huddleAntiCborSpec ::
  forall a.
  (DecCBOR a, Show a) =>
  Version ->
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleAntiCborSpec version ruleName =
  let lbl = label $ Proxy @a
   in describe "Decoding fails when term is zapped"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ \(mapIndex -> cddl) -> property $ do
          mTerm <- tryZapAntiGen 1 . generateFromName cddl $ Name ruleName
          case mTerm of
            Just term -> do
              let
                encoding = toPlainEncoding version $ encodeTerm term
                bs = C.toStrictByteString encoding
              pure . counterexample (prettyHexEnc encoding) $
                decodeFull' @a version bs `shouldSatisfy` isLeft
            Nothing -> discard

specWithHuddle :: Cuddle.Huddle -> SpecWith (CTreeRoot MonoReferenced) -> Spec
specWithHuddle h =
  beforeAll $
    let cddl = Cuddle.toCDDL h
        rCddl = Cuddle.fullResolveCDDL (mapCDDLDropExt cddl)
     in case rCddl of
          Right ct ->
            pure ct
          Left nrf -> error $ show nrf

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

huddleRoundTripArbitraryValidate ::
  forall a.
  ( DecCBOR a
  , EncCBOR a
  , Arbitrary a
  , Show a
  ) =>
  Version ->
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleRoundTripArbitraryValidate version ruleName =
  let lbl = label $ Proxy @a
   in describe "Encode an arbitrary value and check against CDDL"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ \cddl -> property $
          \(val :: a) -> do
            let
              bs = serialize' version val
              res = validateCBOR bs (Name ruleName) (mapIndex cddl)
            case res of
              ValidatorSuccess -> pure ()
              ValidatorFail err ->
                expectationFailure $
                  "CBOR Validation failed\nError:\n" <> show errMsg
                where
                  errMsg = pretty $ pShow err

huddleRoundTripArbitraryValidate ::
  forall a.
  ( DecCBOR a
  , EncCBOR a
  , Arbitrary a
  , Show a
  ) =>
  Version ->
  T.Text ->
  SpecWith CuddleData
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
    hPutDoc h (pretty (mapIndex @_ @_ @PrettyStage cddl))
    -- Write an empty line at the end of the file
    hPutStrLn h ""
  -- Write log to stdout
  putStrLn $ "Generated CDDL file at: " <> path
