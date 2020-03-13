{-# LANGUAGE AllowAmbiguousTypes #-}
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

import           Cardano.Binary
import           Cardano.Prelude
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy.Char8 as Char8 (lines, unpack)
import           System.Process.ByteString.Lazy



import           Test.Tasty
import           Test.Tasty.HUnit

import           Shelley.Spec.Ledger.MetaData (MetaData)
import           Shelley.Spec.Ledger.Serialization
import           Shelley.Spec.Ledger.Updates (PParamsUpdate)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (AVUpdate, Addr, BHBody, BHeader,
                     DCert, LaxBlock, MultiSig, OCert, PPUpdate, Tx, TxBody, TxIn, TxOut, Update)

cddlTests :: TestTree
cddlTests = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [
      cddlGroupTest @BHeader  30 "header"
    , cddlGroupTest @BHBody   30 "header_body"
    , cddlGroupTest @OCert    30 "operational_cert"
    , cddlGroupTest @Addr     20 "address"
    , cddlTest @TxBody        30 "transaction_body"
    , cddlTest @TxOut         30 "transaction_output"
    , cddlTest @DCert         30 "delegation_certificate"
    , cddlTest @TxIn          30 "transaction_input"
    , cddlTest @MetaData      10 "transaction_metadata"
    , cddlTest @MultiSig      30 "script"
    , cddlTest @Update        30 "full_update"
    , cddlTest @AVUpdate      30 "application_version_update_votes"
    , cddlTest @PPUpdate      30 "protocol_param_update_votes"
    , cddlTest @PParamsUpdate 30 "protocol_param_update"
    , cddlTest @Tx            30 "transaction"
    , cddlTest @LaxBlock      30 "block"
    ] <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/shelley.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

cddlTest
  :: forall a. (ToCBOR a, FromCBOR a)
  => Int
  -> BSL.ByteString
  -> IO BSL.ByteString
  -> TestTree
cddlTest n entryName cddlRes = testCase
  ("cddl roundtrip " <> show (typeRep (Proxy @a)))
  $ do
  basecddl <- cddlRes
  let cddl = "output = " <> entryName <> "\n" <> basecddl
  examples <- Char8.lines <$> generateCBORDiagStdIn n cddl :: IO [BSL.ByteString]
  forM_ examples $ \exampleDiag -> do
    exampleBytes <- diagToBytes exampleDiag
    decoded <- case decodeFull @a exampleBytes of
      Right x -> pure x
      Left e  ->
        assertFailure $ Prelude.unlines
          [ "Failed to deserialize"
          , "Error: " <> show e
          , "Data: " <> Char8.unpack exampleDiag
          ]
    let reencoded = serialize decoded
    verifyConforming reencoded cddl

cddlGroupTest
  :: forall a. (ToCBORGroup a, FromCBORGroup a)
  => Int
  -> BSL.ByteString
  -> IO BSL.ByteString
  -> TestTree
cddlGroupTest n entryName cddlRes = testCase
  ("cddl roundtrip " <> show (typeRep (Proxy @a)))
  $ do
  basecddl <- cddlRes
  let cddl = "output = [" <> entryName <> "]\n" <> basecddl
  examples <- Char8.lines <$> generateCBORDiagStdIn n cddl :: IO [BSL.ByteString]
  let decoder = groupRecord :: forall s. Decoder s a
  forM_ examples $ \exampleDiag -> do
    exampleBytes <- diagToBytes exampleDiag
    decoded <- case decodeFullDecoder "CBORGroup" decoder exampleBytes of
      Right x -> pure x
      Left e  ->
        assertFailure $ Prelude.unlines
          [ "Failed to deserialize"
          , "Error: " <> show e
          , "Data: " <> Char8.unpack exampleDiag
          ]
    let reencoded = serializeEncoding $ encodeListLen 1 <> toCBORGroup decoded
    verifyConforming reencoded cddl


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

verifyConforming :: BSL.ByteString -> BSL.ByteString -> Assertion
verifyConforming _value _cddl = pure () -- TODO
