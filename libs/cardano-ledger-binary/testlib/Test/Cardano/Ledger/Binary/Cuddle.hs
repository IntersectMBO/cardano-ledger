{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Cuddle (
  specWithHuddle,
  huddleRoundTripCborSpec,
  huddleRoundTripAnnCborSpec,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR,
  EncCBOR,
  Version,
  decodeFullAnnotator,
  decodeFullDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Decoding (label)
import qualified Codec.CBOR.Cuddle.CBOR.Gen as Cuddle
import qualified Codec.CBOR.Cuddle.CDDL as Cuddle
import qualified Codec.CBOR.Cuddle.CDDL.CTree as Cuddle
import qualified Codec.CBOR.Cuddle.CDDL.Resolve as Cuddle
import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import System.Random (StdGen, getStdGen)
import Test.Cardano.Ledger.Binary.RoundTrip (
  RoundTripFailure (RoundTripFailure),
  Trip (..),
  cborTrip,
  decodeAnnExtra,
  embedTripLabelExtra,
 )
import Test.Hspec (Expectation, Spec, SpecWith, beforeAll, expectationFailure, it, shouldBe)

data CuddleData = CuddleData
  { cddl :: !(Cuddle.CTreeRoot' Identity Cuddle.MonoRef)
  , numExamples :: !Int
  , randomSeed :: !StdGen
  }

huddleRoundTripCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith CuddleData
huddleRoundTripCborSpec version ruleName =
  let lbl = label $ Proxy @a
      trip = cborTrip @a
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $
        \cddlData ->
          let term = Cuddle.generateCBORTerm (cddl cddlData) (Cuddle.Name ruleName) (randomSeed cddlData)
           in roundTripExample lbl version version trip term

huddleRoundTripAnnCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL rule to test
  T.Text ->
  SpecWith CuddleData
huddleRoundTripAnnCborSpec version ruleName =
  let lbl = label $ Proxy @(Annotator a)
      trip = cborTrip @a
   in it (T.unpack ruleName <> ": " <> T.unpack lbl) $
        \cddlData ->
          let term = Cuddle.generateCBORTerm (cddl cddlData) (Cuddle.Name ruleName) (randomSeed cddlData)
           in roundTripAnnExample lbl version version trip term

specWithHuddle :: Cuddle.Huddle -> Int -> SpecWith CuddleData -> Spec
specWithHuddle h numExamples =
  beforeAll $
    do
      stdGen <- getStdGen
      let cddl = Cuddle.toCDDL h
          rCddl = Cuddle.fullResolveCDDL cddl
       in case rCddl of
            Right ct ->
              pure $
                CuddleData
                  { cddl = ct
                  , numExamples = numExamples
                  , randomSeed = stdGen
                  }
            Left nrf -> error $ show nrf

-- | Verify that random data generated is:
--
-- * Decoded successfully into a Haskell type using the decoder in `Trip` and the version
--   supplied
--
-- * When reencoded conforms produces a valid `FlatTerm`
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
