{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.CDDL
  ( cddlTests
  )
 where

import           Prelude (String)
import qualified Prelude

import           Cardano.Binary (ToCBOR(..), FromCBOR(..), DecoderError, serialize,
                                 decodeFullDecoder, serializeEncoding, encodeListLen,
                                 Annotator, decodeAnnotator)
import           Cardano.Prelude
import           Control.Exception (bracket)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy.Char8 as Char8 (lines, unpack)
import qualified System.Directory as Sys
import qualified System.IO as Sys
import qualified System.IO.Error as Sys
import           System.Process.ByteString.Lazy


import           Test.Tasty
import           Test.Tasty.HUnit

import           Shelley.Spec.Ledger.MetaData (MetaData)
import           Shelley.Spec.Ledger.PParams (PParamsUpdate)
import           Shelley.Spec.Ledger.Serialization

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, BHBody, BHeader, Credential,
                     DCert, LaxBlock, MultiSig, OCert, ProposedPPUpdates, Tx, TxBody, TxIn, TxOut,
                     Update)

cddlTests :: TestTree
cddlTests = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [
      cddlTest' @BHeader          n "header"
    , cddlGroupTest @BHBody       n "header_body"
    , cddlGroupTest @OCert        n "operational_cert"
    , cddlGroupTest @Addr         n "address"
    , cddlTest @Credential        n "credential"
    , cddlTest @TxBody            n "transaction_body"
    , cddlTest @TxOut             n "transaction_output"
    , cddlTest @DCert             n "delegation_certificate"
    , cddlTest @TxIn              n "transaction_input"
    , cddlTest @MetaData          n "transaction_metadata"
    , cddlTest @MultiSig          n "script"
    , cddlTest @Update            n "update"
    , cddlTest @ProposedPPUpdates n "proposed_protocol_parameter_updates"
    , cddlTest @PParamsUpdate     n "protocol_param_update"
    , cddlTest @Tx                n "transaction"
    , cddlTest' @LaxBlock         n "block"
    ] <*> pure cddl
  where
    n = 1

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/shelley.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

cddlTest
  :: forall a. (ToCBOR a, FromCBOR a, Show a)
  => Int
  -> BSL.ByteString
  -> IO BSL.ByteString
  -> TestTree
cddlTest n entryName cddlRes = testCase
  ("cddl roundtrip " <> show (typeRep (Proxy @a)))
  $ do
  basecddl <- cddlRes
  let cddl = "output = " <> entryName <> "\n" <> basecddl
  cddlTestCommon @a serialize (decodeFullDecoder "cbor test" fromCBOR) n cddl

cddlTest'
  :: forall a. (ToCBOR a, FromCBOR (Annotator a), Show a)
  => Int
  -> BSL.ByteString
  -> IO BSL.ByteString
  -> TestTree
cddlTest' n entryName cddlRes = testCase
  ("cddl roundtrip " <> show (typeRep (Proxy @a)))
  $ do
  basecddl <- cddlRes
  let cddl = "output = " <> entryName <> "\n" <> basecddl
  cddlTestCommon @a serialize (decodeAnnotator "cbor test" fromCBOR) n cddl

cddlGroupTest
  :: forall a. (ToCBORGroup a, FromCBORGroup a, Show a)
  => Int
  -> BSL.ByteString
  -> IO BSL.ByteString
  -> TestTree
cddlGroupTest n entryName cddlRes = testCase ("cddl roundtrip " <> show (typeRep (Proxy @a))) $ do
  basecddl <- cddlRes
  let cddl = "output = [" <> entryName <> "]\n" <> basecddl
  cddlTestCommon @a
    (\x -> serializeEncoding $ encodeListLen (listLen x) <> toCBORGroup x)
    (decodeFullDecoder "cbor test" groupRecord)
    n
    cddl

cddlTestCommon
  :: Show a => (a -> BSL.ByteString)
  -> (BSL.ByteString -> Either DecoderError a)
  -> Int
  -> BSL.ByteString
  -> IO ()
cddlTestCommon serializer decoder n cddlData = do
  usingFile cddlData $ \cddl -> do
    examples <- Char8.lines <$> generateCBORDiagStdIn n cddlData :: IO [BSL.ByteString]
    forM_ examples $ \exampleDiag -> do
      exampleBytes <- diagToBytes exampleDiag
      decoded <- case decoder exampleBytes of
        Right x -> pure x
        Left e  ->
          assertFailure $ Prelude.unlines
            [ "Failed to deserialize"
            , "Error: " <> show e
            , "Generated diag: " <> Char8.unpack exampleDiag
            ]
      let reencoded = serializer decoded
      verifyConforming reencoded cddl >>= \case
        True -> pure ()
        False ->
          assertFailure $ Prelude.unlines
            [ "Serialized data did not conform to the spec"
            , "Generated diag: " <> Char8.unpack exampleDiag
            , "Generated base16: " <> Char8.unpack (Base16.encode exampleBytes)
            , "Decoded value: " <> show decoded
            , "Reencoded base16: " <> Char8.unpack (Base16.encode reencoded)
            ]

data StdErr = StdErr Prelude.String BSL.ByteString

instance Show StdErr where
  show (StdErr message stdErr) = Prelude.unlines
    [ message
    , Char8.unpack stdErr
    ]

instance Exception StdErr

throwStdErr :: forall a. Prelude.String -> BSL.ByteString -> IO a
throwStdErr message stdErr = throwIO $ StdErr message stdErr


generateCBORDiagStdIn :: Int -> BSL.ByteString -> IO BSL.ByteString
generateCBORDiagStdIn rounds cddl =
  readProcessWithExitCode2 "cddl" ["-", "generate", Prelude.show rounds] cddl >>=
    \case
      Right result -> pure result
      Left stdErr -> throwStdErr "Got failing exit code when generating examples" stdErr

diagToBytes :: BSL.ByteString -> IO BSL.ByteString
diagToBytes diag = readProcessWithExitCode2 "diag2cbor.rb" ["-"] diag >>=
    \case
      Right result -> pure result
      Left stdErr -> throwStdErr "Got failing exit code when converting cbor diagnostic notation to bytes" stdErr

readProcessWithExitCode2 :: FilePath -> [String] -> BSL.ByteString -> IO (Either BSL.ByteString BSL.ByteString)
readProcessWithExitCode2 exec args stdIn = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode exec args stdIn
  case exitCode of
    ExitSuccess -> pure (Right stdOut)
    ExitFailure _i -> pure (Left stdErr)

withTempFile :: FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile dir nameTemplate k = bracket (Sys.openBinaryTempFile dir nameTemplate) cleanup (uncurry k)
  where
  cleanup (fileName, h) = do
    Sys.hClose h
    catch (Sys.removeFile fileName) $ \err ->
      if Sys.isDoesNotExistError err then return () else throwIO err

usingFile :: BSL.ByteString -> (FilePath -> IO a) -> IO a
usingFile bytes k = withTempFile "." "tmp" $ \fileName h -> do
  BSL.hPut h bytes
  Sys.hClose h
  k fileName

verifyConforming :: BSL.ByteString -> FilePath -> IO Bool
verifyConforming value cddl = readProcessWithExitCode2 "cddl" [cddl, "validate", "-"] value >>=
    \case
      Right _ -> pure True
      Left _ -> pure False
