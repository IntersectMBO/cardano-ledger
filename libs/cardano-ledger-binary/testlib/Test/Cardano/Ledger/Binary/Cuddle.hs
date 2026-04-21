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
  generateCBORMain,
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
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  Evidenced (..),
  SValidity (..),
  TraceOptions (..),
  ValidationTrace,
  defaultTraceOptions,
  isValid,
  prettyValidationTrace,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
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
import qualified Codec.CBOR.Write as CBOR
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import qualified Options.Applicative as Opt
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Terminal as Ansi
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), hPutStrLn, hSetBinaryMode, stderr, stdout, withFile)
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
 )
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  Testable (..),
  counterexample,
  discard,
  forAll,
  generate,
 )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

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
  DecCBOR a =>
  Version ->
  T.Text ->
  SpecWith (CTreeRoot MonoReferenced)
huddleAntiCborSpec version ruleName =
  let lbl = label $ Proxy @a
   in describe "Decoding fails when term is zapped"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ \cddl -> property @(Gen Property) $ do
          mTerm <- tryZapAntiGen 1 . generateFromName (mapIndex cddl) $ Name ruleName
          case mTerm of
            Just term -> do
              let
                encoding = toPlainEncoding version $ encodeTerm term
                bs = CBOR.toStrictByteString encoding
              case validateCBOR bs (Name ruleName) (mapIndex cddl) of
                Evidenced SInvalid trc -> do
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
                        , "Decoding succeeded, expected failure"
                        ]
                  pure . counterexample errMsg . isLeft $ decodeFull' @a version bs
                Evidenced SValid _ -> discard
            Nothing -> discard

specWithHuddle :: Cuddle.Huddle -> SpecWith (CTreeRoot MonoReferenced) -> Spec
specWithHuddle h =
  beforeAll $
    case resolveHuddle h of
      Right ct -> pure ct
      Left err -> error err

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

huddleRoundTripGenValidate ::
  forall a.
  (DecCBOR a, Show a, EncCBOR a) => Gen a -> Version -> T.Text -> SpecWith (CTreeRoot MonoReferenced)
huddleRoundTripGenValidate gen version ruleName =
  let lbl = label $ Proxy @a
   in describe "Encode an arbitrary value and check against CDDL"
        . it (T.unpack ruleName <> ": " <> T.unpack lbl)
        $ \cddl -> property . forAll gen $
          \(val :: a) -> do
            let
              bs = serialize' version val
              res = validateCBOR bs (Name ruleName) (mapIndex cddl)
            unless (isValid res) . expectationFailure $
              "CBOR Validation failed\nError:\n" <> showValidationTrace res

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

--------------------------------------------------------------------------------
-- Generating CBOR from CDDL
--------------------------------------------------------------------------------

-- | Options for CBOR generation CLI
data GenerateCBOROpts = GenerateCBOROpts
  { gcboRuleNames :: ![T.Text]
  , gcboZap :: !(Maybe Int)
  , gcboCount :: !Int
  , gcboSeed :: !(Maybe Int)
  , gcboBinary :: !Bool
  }

generateCBOROptsParser :: Opt.Parser GenerateCBOROpts
generateCBOROptsParser =
  GenerateCBOROpts
    <$> Opt.some
      ( Opt.strArgument
          ( Opt.metavar "RULE_NAME..."
              <> Opt.help "CDDL rule names to generate CBOR for"
          )
      )
    <*> Opt.optional
      ( Opt.option
          Opt.auto
          ( Opt.long "zap"
              <> Opt.metavar "DEPTH"
              <> Opt.help "Generate corrupted (zapped) CBOR with given zap depth"
          )
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "count"
          <> Opt.short 'n'
          <> Opt.metavar "N"
          <> Opt.value 1
          <> Opt.showDefault
          <> Opt.help "Number of samples to generate per rule"
      )
    <*> Opt.optional
      ( Opt.option
          Opt.auto
          ( Opt.long "seed"
              <> Opt.metavar "SEED"
              <> Opt.help "Fixed random seed for reproducibility"
          )
      )
    <*> Opt.switch
      ( Opt.long "binary"
          <> Opt.help "Output raw CBOR bytes instead of hex encoding"
      )

-- | Resolve a Huddle spec into a fully resolved CTree, suitable for CBOR generation.
resolveHuddle :: Cuddle.Huddle -> Either String (CTreeRoot MonoReferenced)
resolveHuddle h =
  let cddl = Cuddle.toCDDL h
   in case Cuddle.fullResolveCDDL (mapCDDLDropExt cddl) of
        Right ct -> Right ct
        Left nrf -> Left (show nrf)

-- | Main entry point for generate-cbor executables.
generateCBORMain :: Cuddle.Huddle -> IO ()
generateCBORMain huddle = do
  opts <-
    Opt.execParser $
      Opt.info
        (generateCBOROptsParser Opt.<**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate CBOR data from CDDL rules"
            <> Opt.header "generate-cbor - CBOR data generator from CDDL specifications"
        )
  case resolveHuddle huddle of
    Left err -> do
      hPutStrLn stderr $ "Failed to resolve CDDL: " <> err
      exitFailure
    Right cddl -> do
      when (gcboBinary opts) $ hSetBinaryMode stdout True
      mapM_ (generateForRule opts cddl) (gcboRuleNames opts)

generateForRule :: GenerateCBOROpts -> CTreeRoot MonoReferenced -> T.Text -> IO ()
generateForRule opts cddl ruleName = do
  let name = Name ruleName
      showHeader = length (gcboRuleNames opts) > 1 && not (gcboBinary opts)
  when showHeader $
    putStrLn $
      "# " <> T.unpack ruleName
  mapM_ (generateOneSample opts cddl name) [0 .. gcboCount opts - 1]

generateOneSample :: GenerateCBOROpts -> CTreeRoot MonoReferenced -> Name -> Int -> IO ()
generateOneSample opts cddl name sampleIndex = do
  let gen = case gcboZap opts of
        Nothing -> fmap Just . runAntiGen $ generateFromName (mapIndex cddl) name
        Just depth -> tryZapAntiGen depth $ generateFromName (mapIndex cddl) name
  mTerm <- case gcboSeed opts of
    Nothing -> generate gen
    Just seed ->
      let qcGen = mkQCGen (seed + sampleIndex)
       in pure $ unGen gen qcGen 30
  case mTerm of
    Nothing ->
      hPutStrLn stderr $
        "Warning: zap produced Nothing for rule "
          <> T.unpack (unName name)
          <> " (sample "
          <> show sampleIndex
          <> ")"
    Just term -> do
      let encoding = CBOR.encodeTerm term
          bs = CBOR.toStrictByteString encoding
      if gcboBinary opts
        then BS.hPut stdout bs
        else
          BS8.putStrLn (Base16.encode bs)
  where
    unName (Name t) = t
