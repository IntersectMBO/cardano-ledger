{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Cddl where

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

newtype Cddl = Cddl {unCddl :: BSL.ByteString}

newtype Cbor = Cbor {unCbor :: BSL.ByteString}

newtype DiagCbor = DiagCbor {unDiagCbor :: BSL.ByteString}

data CddlVarFile = CddlVarFile
  { cddlVarFilePath :: !FilePath
  -- ^ File that contains the Cddl data included in this type
  , cddlVarName :: !T.Text
  -- ^ Name of the variable being tested
  , cddlVarData :: !Cddl
  -- ^ Full CDDL spec with @output=varName@ prefix
  , cddlVarDiagCbor :: ![DiagCbor]
  -- ^ Generated CBOR data from the above CDDL spec
  }

data CddlData = CddlData
  { cddlData :: !Cddl
  , cddlNumExamples :: !Int
  }

beforeAllCddlFile :: HasCallStack => Int -> IO [BSL.ByteString] -> SpecWith CddlData -> Spec
beforeAllCddlFile numExamples getCddlFiles = beforeAll $ do
  cddls <- getCddlFiles
  -- combine all files into one large strict bytestring, while converting in back to the
  -- lazy one for later usage. This is done to reduce overhead of a lazy bytestring
  let cddl = Cddl $ BSL.fromStrict $ BSL.toStrict $ mconcat cddls
  pure $
    CddlData
      { cddlData = cddl
      , cddlNumExamples = numExamples
      }

withCddlVarFile :: HasCallStack => T.Text -> CddlData -> (CddlVarFile -> IO b) -> IO b
withCddlVarFile varName CddlData {..} roundTripTest = do
  let suffix = T.encodeUtf8 $ "\noutput = " <> varName <> "\n"
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

cddlRoundTripCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR a) =>
  Version ->
  T.Text ->
  SpecWith CddlData
cddlRoundTripCborSpec version varName =
  let lbl = label $ Proxy @a
   in it (T.unpack $ varName <> ": " <> lbl) $ \cddlData ->
        withCddlVarFile varName cddlData $
          cddlRoundTripExpectation lbl version version (cborTrip @a)

cddlRoundTripExpectation ::
  (HasCallStack, Show a, Eq a) =>
  T.Text ->
  Version ->
  Version ->
  Trip a a ->
  CddlVarFile ->
  Expectation
cddlRoundTripExpectation lbl encVersion decVersion trip@Trip {tripDecoder} CddlVarFile {..} = do
  forM_ cddlVarDiagCbor $ \diagCbor -> do
    Cbor cbor <- diagToCbor diagCbor
    let failure err =
          expectationFailure $ "Failed to Cddl RoundTrip verification:\n" ++ show err
        mkFailure encoding =
          RoundTripFailure encVersion decVersion encoding cbor
    case decodeFullDecoder decVersion lbl tripDecoder cbor of
      Left decErr ->
        failure $ mkFailure mempty Nothing Nothing Nothing (Just decErr)
      Right val ->
        case embedTripLabelExtra lbl encVersion decVersion trip val of
          Right (val', encoding, encodedBytes) ->
            validateCddlConformance cddlVarFilePath encodedBytes >>= \case
              Left confErr ->
                failure $ mkFailure encoding (Just encodedBytes) (Just confErr) Nothing Nothing
              Right _bsl -> val' `shouldBe` val
          embedErr -> failure embedErr

cddlRoundTripAnnCborSpec ::
  forall a.
  (HasCallStack, Eq a, Show a, EncCBOR a, DecCBOR (Annotator a)) =>
  -- | Cddl variable name
  Version ->
  T.Text ->
  SpecWith CddlData
cddlRoundTripAnnCborSpec version varName =
  let lbl = label (Proxy @(Annotator a))
   in it (T.unpack $ varName <> ": " <> lbl) $ \cddlData ->
        withCddlVarFile varName cddlData $
          cddlRoundTripAnnExpectation lbl version version (cborTrip @a)

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
    Cbor cbor <- diagToCbor diagCbor
    let failure err =
          expectationFailure $ "Failed to Cddl RoundTrip verification:\n" ++ show err
        mkFailure encoding =
          RoundTripFailure encVersion decVersion encoding cbor
    -- lbl = label (Proxy @(Annotator a))
    case decodeFullAnnotator decVersion lbl tripDecoder cbor of
      Left decErr ->
        failure $ mkFailure mempty Nothing Nothing Nothing (Just decErr)
      Right val ->
        let encoding = toPlainEncoding encVersion $ tripEncoder val
         in case decodeAnnExtra lbl encVersion decVersion tripDecoder encoding of
              Right (val', encodedBytes) ->
                validateCddlConformance cddlVarFilePath encodedBytes >>= \case
                  Left confErr ->
                    failure $ mkFailure encoding (Just encodedBytes) (Just confErr) Nothing Nothing
                  Right _bsl -> val' `shouldBe` val
              embedErr -> failure embedErr

genCddlDiagCbor :: HasCallStack => Int -> Cddl -> IO [DiagCbor]
genCddlDiagCbor numCases =
  fmap (either error (map DiagCbor . BSL8.lines))
    . readProcessNoFailure "generating examples" (proc "cddl" ["-", "generate", show numCases])
    . unCddl

diagToCbor :: HasCallStack => DiagCbor -> IO Cbor
diagToCbor =
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

-- | Run a process with `readProcess` and explode on failure. Accepts lazy bytestring as
-- input for the spawned process. In case of a successfull process exit, but nonempty
-- stderr, it will be printed out.
readProcessNoFailure ::
  String ->
  ProcessConfig stdin stdout stderr ->
  BSL.ByteString ->
  IO (Either String BSL.ByteString)
readProcessNoFailure procDescr procConfig input =
  readProcess (setStdin (byteStringInput input) procConfig) >>= \case
    (ExitSuccess, stdOut, "") -> pure $ Right stdOut
    (ExitSuccess, stdOut, _stdErr) -> do
      -- Ideally we would only want to use relevant CDDL for particular test, which
      -- would result in stdErr to be empty, but currently there warnings about unused
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
