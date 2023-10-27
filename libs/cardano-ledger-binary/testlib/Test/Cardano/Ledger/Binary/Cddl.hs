{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Cddl (
  cddlRoundTripCborSpec,
  cddlRoundTripExpectation,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripAnnExpectation,

  -- * Helper functions and types
  Cddl (..),
  Cbor (..),
  DiagCbor (..),
  CddlData (..),
  CddlVarFile (..),
  beforeAllCddlFile,
  withCddlVarFile,
  genCddlDiagCbor,
  diagCborToCbor,
  validateCddlConformance,
  readProcessNoFailure,
  usingTempFile,
) where

import Cardano.Ledger.Binary
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import System.IO (hClose)
import System.Process.Typed (
  ExitCode (..),
  ProcessConfig,
  byteStringInput,
  proc,
  readProcess,
  setStdin,
 )
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Hspec
import UnliftIO.Temporary (withTempFile)

-- | Contents of CDDL Spec
newtype Cddl = Cddl {unCddl :: BSL.ByteString}

-- | Binary form of CBOR data.
newtype Cbor = Cbor {unCbor :: BSL.ByteString}

-- | Human readable CBOR, which was randomly generated.
newtype DiagCbor = DiagCbor {unDiagCbor :: BSL.ByteString}

data CddlVarFile = CddlVarFile
  { cddlVarFilePath :: !FilePath
  -- ^ File that contains the Cddl data included in this type
  , cddlVarName :: !T.Text
  -- ^ Name of the variable being tested
  , cddlVarData :: !Cddl
  -- ^ Full CDDL spec with @output=`cddlVarName`@ prefix
  , cddlVarDiagCbor :: ![DiagCbor]
  -- ^ Generated CBOR data from the above CDDL spec
  }

data CddlData = CddlData
  { cddlData :: !Cddl
  , cddlNumExamples :: !Int
  -- ^ Number of random cases to generate
  }

-- | Given an action that produces CDDL content, we combine it all into `CddlData` and
-- make it available to every subsequent Spec. Important point about this is that the
-- supplied action will only be executed once.
beforeAllCddlFile ::
  HasCallStack =>
  -- | Number of random cases to generate
  Int ->
  -- | Action that produces a list of valid CDDL specs
  IO [BSL.ByteString] ->
  SpecWith CddlData ->
  Spec
beforeAllCddlFile numExamples getCddlFiles = beforeAll $ do
  cddls <- getCddlFiles
  -- combine all files into one large strict bytestring, while converting it back to the
  -- lazy one for later usage. This is done to reduce overhead of a lazy bytestring
  let cddl = Cddl $ BSL.fromStrict $ BSL.toStrict $ mconcat cddls
  pure $
    CddlData
      { cddlData = cddl
      , cddlNumExamples = numExamples
      }

-- | Given a `CddlData` and a CDDL variable name present in the supplied data, generate a
-- `CddlVarFile`, that contains random data for that variable.
withCddlVarFile ::
  HasCallStack =>
  -- | Name of the variable that will be tested
  T.Text ->
  -- | CddlData that will be used for random data generation
  CddlData ->
  -- | Action that can use the random data for roundtrip and conformance testing
  (CddlVarFile -> IO b) ->
  IO b
withCddlVarFile varName CddlData {..} roundTripTest = do
  let suffix = T.encodeUtf8 $ "output = " <> varName <> "\n"
      varData = Cddl (BSL.fromStrict suffix <> unCddl cddlData)
  diagCbor <- genCddlDiagCbor cddlNumExamples varData
  usingTempFile (unCddl varData) $ \filePath ->
    roundTripTest $
      CddlVarFile
        { cddlVarFilePath = filePath
        , cddlVarName = varName
        , cddlVarData = varData
        , cddlVarDiagCbor = diagCbor
        }

-- | Using the supplied `CddlData` inside the `SpecWith` generate random data and run the
-- `cddlRoundTripExpectation` for the supplied CDDL variable
cddlRoundTripCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  -- | Serialization version
  Version ->
  -- | Name of the CDDL variable to test
  T.Text ->
  SpecWith CddlData
cddlRoundTripCborSpec version varName =
  let lbl = label $ Proxy @a
   in it (T.unpack $ varName <> ": " <> lbl) $ \cddlData ->
        withCddlVarFile varName cddlData $
          cddlRoundTripExpectation lbl version version (cborTrip @a)

-- | Verify that random data generated is:
--
-- * Decoded successfully into a Haskell type using the decoder in `Trip` and the version
--   supplied
--
-- * When reencoded conforms to the CDDL spec and produces valid `FlatTerm`
--
-- * When decoded again from the bytes produced by the encoder matches the type exactly
--   when it was decoded from random bytes
cddlRoundTripExpectation ::
  (HasCallStack, Show a, Eq a) =>
  T.Text ->
  -- | Version to use for decoding
  Version ->
  -- | Version to use for encoding
  Version ->
  -- | Decode/encoder that needs tsting
  Trip a a ->
  -- | Randomly generated data and the CDDL spec
  CddlVarFile ->
  Expectation
cddlRoundTripExpectation lbl encVersion decVersion trip@Trip {tripDecoder} CddlVarFile {..} = do
  forM_ cddlVarDiagCbor $ \diagCbor -> do
    Cbor cbor <- diagCborToCbor diagCbor
    let mkFailure encoding =
          RoundTripFailure encVersion decVersion encoding cbor
    case decodeFullDecoder decVersion lbl tripDecoder cbor of
      Left decErr ->
        cddlFailure diagCbor $ mkFailure mempty Nothing Nothing Nothing (Just decErr)
      Right val ->
        case embedTripLabelExtra lbl encVersion decVersion trip val of
          Right (val', encoding, encodedBytes) ->
            validateCddlConformance cddlVarFilePath encodedBytes >>= \case
              Left confErr ->
                cddlFailure diagCbor $
                  mkFailure encoding (Just encodedBytes) (Just confErr) Nothing Nothing
              Right _bsl -> val' `shouldBe` val
          Left embedErr -> cddlFailure diagCbor embedErr

cddlFailure :: HasCallStack => DiagCbor -> RoundTripFailure -> Expectation
cddlFailure diagCbor err =
  expectationFailure $
    unlines
      [ "Failed to Cddl RoundTrip verification:"
      , show err
      , "Generated diag: " <> BSL8.unpack (unDiagCbor diagCbor)
      ]

-- | Similar to `cddlRoundTripCborSpec`, but for Annotator.
cddlRoundTripAnnCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  -- | Serialization version
  Version ->
  -- | Cddl variable name
  T.Text ->
  SpecWith CddlData
cddlRoundTripAnnCborSpec version varName =
  let lbl = label (Proxy @(Annotator a))
   in it (T.unpack $ varName <> ": " <> lbl) $ \cddlData ->
        withCddlVarFile varName cddlData $
          cddlRoundTripAnnExpectation lbl version version (cborTrip @a)

-- | Same as `cddlRoundTripExpectation`, but works for decoders that are wrapped into
-- `Annotator`
cddlRoundTripAnnExpectation ::
  forall a.
  (HasCallStack, Show a, Eq a) =>
  T.Text ->
  Version ->
  Version ->
  Trip a (Annotator a) ->
  CddlVarFile ->
  Expectation
cddlRoundTripAnnExpectation lbl encVersion decVersion Trip {..} CddlVarFile {..} = do
  forM_ cddlVarDiagCbor $ \diagCbor -> do
    Cbor cbor <- diagCborToCbor diagCbor
    let mkFailure encoding =
          RoundTripFailure encVersion decVersion encoding cbor
    case decodeFullAnnotator decVersion lbl tripDecoder cbor of
      Left decErr ->
        cddlFailure diagCbor $ mkFailure mempty Nothing Nothing Nothing (Just decErr)
      Right val ->
        let encoding = toPlainEncoding encVersion $ tripEncoder val
         in case decodeAnnExtra lbl encVersion decVersion tripDecoder encoding of
              Right (val', encodedBytes) ->
                validateCddlConformance cddlVarFilePath encodedBytes >>= \case
                  Left confErr ->
                    cddlFailure diagCbor $
                      mkFailure encoding (Just encodedBytes) (Just confErr) Nothing Nothing
                  Right _bsl -> val' `shouldBe` val
              Left embedErr -> cddlFailure diagCbor embedErr

genCddlDiagCbor :: HasCallStack => Int -> Cddl -> IO [DiagCbor]
genCddlDiagCbor numCases =
  fmap (either error (map DiagCbor . BSL8.lines))
    . readProcessNoFailure "generating examples" (proc "cddl" ["-", "generate", show numCases])
    . unCddl

-- | Convert randomly generated Cbor in special Diag (human readable) format to binary CBOR
-- format
diagCborToCbor :: HasCallStack => DiagCbor -> IO Cbor
diagCborToCbor =
  fmap (either error Cbor)
    . readProcessNoFailure "converting cbor diagnostic notation to bytes" (proc "diag2cbor.rb" ["-"])
    . unDiagCbor

-- | Run a @cddl@ process and validate that encoded data is in confomrance with the
-- supplied CDDL spec
validateCddlConformance ::
  -- | File path to the file with CDDL spec
  FilePath ->
  -- | CBOR encoded data that should conform with CDDL spec
  BSL8.ByteString ->
  IO (Either String BSL.ByteString)
validateCddlConformance filePath =
  readProcessNoFailure "validating cddl conformace" (proc "cddl" [filePath, "validate", "-"])

-- | Run a process with `readProcess` and return `Left` on failure, which will contain the
-- output produced on @stderr@. Accepts lazy bytestring as input for the spawned
-- process. In case when a process exits successfuly, the output is returned on the
-- `Right`. Upon a successful exit, @stderr@ output is ignored to avoid polluting test
-- suite output with copious amounts of warnings.
readProcessNoFailure ::
  String ->
  ProcessConfig stdin stdout stderr ->
  BSL.ByteString ->
  IO (Either String BSL.ByteString)
readProcessNoFailure procDescr procConfig input =
  readProcess (setStdin (byteStringInput input) procConfig) >>= \case
    (ExitSuccess, stdOut, "") -> pure $ Right stdOut
    (ExitSuccess, stdOut, _stdErr) -> do
      -- Ideally we would only want to use relevant CDDL for particular test, which would
      -- result in stdErr to be empty, but currently there are many warnings about unused
      -- CDDL rules:
      --
      -- putStrLn $
      --   unlines
      --     [ "Process for " <> procDescr
      --     , "received some output on stderr"
      --     , show procConfig
      --     , "StdErr output:"
      --     , bslToString stdErr
      --     ]
      pure $ Right stdOut
    (ExitFailure exitCode, _, stdErr) ->
      pure $
        Left $
          unlines
            [ "Process for " <> procDescr
            , show procConfig
            , "failed with an error code: " <> show exitCode
            , "StdErr output:"
            , bslToString stdErr
            ]
  where
    bslToString = T.unpack . T.decodeUtf8 . BSL.toStrict

-- | Write binary data to a temporary file and apply an action to a temporary file name
-- path that contains that data. File is guaranteed to be alive only until the supplied
-- action is active, therefore make sure not to return the name for the temporary file
-- name.
usingTempFile :: BSL.ByteString -> (FilePath -> IO a) -> IO a
usingTempFile bytes action =
  withTempFile "." "tmp" $ \fileName h -> do
    BSL.hPut h bytes
    hClose h
    action fileName
