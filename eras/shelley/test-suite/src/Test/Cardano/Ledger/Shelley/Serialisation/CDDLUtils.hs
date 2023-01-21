{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlTest,
  cddlAnnotatorTest,
  cddlGroupTest,
  cddlTestCommon,
)
where

import Cardano.Ledger.Binary (
  Annotator,
  DecoderError,
  FromCBOR (..),
  FromCBORGroup (..),
  ToCBOR (..),
  ToCBORGroup (..),
  Version,
  decodeFullAnnotator,
  decodeFullDecoder,
  encodeListLen,
  groupRecord,
  serialize,
  serializeEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import Control.Exception hiding (throwIO)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as Char8 (lines, unpack)
import Data.Foldable (forM_)
import Data.Typeable
import GHC.Stack
import System.Exit (ExitCode (..))
import qualified System.IO as Sys
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import UnliftIO (throwIO)
import UnliftIO.Temporary (withTempFile)

-- Round trip test for a type t with instances:
-- ToCBOR t
-- FromCBOR t
cddlTest ::
  forall a.
  (ToCBOR a, FromCBOR a, Show a, HasCallStack) =>
  Version ->
  Int ->
  BSL.ByteString ->
  IO BSL.ByteString ->
  TestTree
cddlTest v = cddlRoundtripTest @a (serialize v) (decodeFullDecoder v "cbor test" fromCBOR)

-- Round trip test for a type t with instances:
-- ToCBOR t
-- FromCBOR (Annotator t)
cddlAnnotatorTest ::
  forall a.
  (Plain.EncCBOR a, FromCBOR (Annotator a), Show a, HasCallStack) =>
  Version ->
  Int ->
  BSL.ByteString ->
  IO BSL.ByteString ->
  TestTree
cddlAnnotatorTest v = cddlRoundtripTest @a Plain.serialize (decodeFullAnnotator v "cbor test" fromCBOR)

-- | Round trip test for a type t with instances:
-- ToCBORGroup t
-- FromCBORGRoup t
cddlGroupTest ::
  forall a.
  (ToCBORGroup a, FromCBORGroup a, Show a, HasCallStack) =>
  Version ->
  Int ->
  BSL.ByteString ->
  IO BSL.ByteString ->
  TestTree
cddlGroupTest v n entryName =
  let serializeGroup x = serializeEncoding v $ encodeListLen (listLen x) <> toCBORGroup x
      desrializeGroup = decodeFullDecoder v "cbor test" groupRecord
   in cddlRoundtripTest @a serializeGroup desrializeGroup n ("[" <> entryName <> "]")

cddlRoundtripTest ::
  forall a.
  (Typeable a, Show a, HasCallStack) =>
  (a -> BSL.ByteString) ->
  (BSL.ByteString -> Either DecoderError a) ->
  Int ->
  BSL.ByteString ->
  IO BSL.ByteString ->
  TestTree
cddlRoundtripTest serializeWith decodeWith n entryName cddlRes =
  testCase ("CDDL roundtrip " <> show (typeRep (Proxy @a))) $ do
    basecddl <- cddlRes
    let cddl = "output = " <> entryName <> "\n" <> basecddl
    cddlTestCommon @a serializeWith decodeWith n cddl

-- Round trip test for a type t, using explicit encoding and decoding functions
cddlTestCommon ::
  (Show a, HasCallStack) =>
  (a -> BSL.ByteString) ->
  (BSL.ByteString -> Either DecoderError a) ->
  Int ->
  BSL.ByteString ->
  IO ()
cddlTestCommon serializer decoder n cddlData = do
  usingTempFile cddlData $ \cddl -> do
    examples <- Char8.lines <$> generateCBORDiagStdIn n cddlData :: IO [BSL.ByteString]
    forM_ examples $ \exampleDiag -> do
      exampleBytes <- diagToBytes exampleDiag
      decoded <- case decoder exampleBytes of
        Right x -> pure x
        Left e ->
          assertFailure $
            unlines
              [ "Failed to deserialize"
              , "Error: " <> show e
              , "Generated diag: " <> Char8.unpack exampleDiag
              , "Generated base16: " <> Char8.unpack (Base16.encode exampleBytes)
              , "terms: " <> case deserialiseFromBytes decodeTerm exampleBytes of
                  Left e' -> show e'
                  Right (_, terms) -> show terms
              ]
      let reencoded = serializer decoded
      verifyConforming reencoded cddl >>= \case
        True -> pure ()
        False ->
          assertFailure $
            unlines
              [ "Serialized data did not conform to the spec"
              , "Generated diag: " <> Char8.unpack exampleDiag
              , "Generated base16: " <> Char8.unpack (Base16.encode exampleBytes)
              , "Decoded value: " <> show decoded
              , "Reencoded base16: " <> Char8.unpack (Base16.encode reencoded)
              ]

data StdErr = StdErr String BSL.ByteString

instance Show StdErr where
  show (StdErr message stdErr) =
    unlines
      [ message
      , Char8.unpack stdErr
      ]

instance Exception StdErr

throwStdErr :: forall a. String -> BSL.ByteString -> IO a
throwStdErr message stdErr = throwIO $ StdErr message stdErr

generateCBORDiagStdIn :: Int -> BSL.ByteString -> IO BSL.ByteString
generateCBORDiagStdIn rounds cddl =
  readProcessWithExitCode2 "cddl" ["-", "generate", show rounds] cddl
    >>= \case
      Right result -> pure result
      Left stdErr -> throwStdErr "Got failing exit code when generating examples" stdErr

diagToBytes :: BSL.ByteString -> IO BSL.ByteString
diagToBytes diag =
  readProcessWithExitCode2 "diag2cbor.rb" ["-"] diag
    >>= \case
      Right result -> pure result
      Left stdErr -> throwStdErr "Got failing exit code when converting cbor diagnostic notation to bytes" stdErr

readProcessWithExitCode2 :: FilePath -> [String] -> BSL.ByteString -> IO (Either BSL.ByteString BSL.ByteString)
readProcessWithExitCode2 exec args stdIn = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode exec args stdIn
  case exitCode of
    ExitSuccess -> pure (Right stdOut)
    ExitFailure _i -> pure (Left stdErr)

usingTempFile :: BSL.ByteString -> (FilePath -> IO a) -> IO a
usingTempFile bytes k = withTempFile "." "tmp" $ \fileName h -> do
  BSL.hPut h bytes
  Sys.hClose h
  k fileName

verifyConforming :: BSL.ByteString -> FilePath -> IO Bool
verifyConforming value cddl =
  readProcessWithExitCode2 "cddl" [cddl, "validate", "-"] value
    >>= \case
      Right _ -> pure True
      Left _ -> pure False
