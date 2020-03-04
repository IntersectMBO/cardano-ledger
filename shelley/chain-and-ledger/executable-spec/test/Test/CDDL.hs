{-# Language Rank2Types #-}
{-# Language TypeApplications #-}
{-# Language BangPatterns #-}
{-# Language NoImplicitPrelude #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}

module Test.CDDL
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

import           ConcreteCryptoTypes (AVUpdate, DCert, MultiSig, PPUpdate, Tx, TxBody, TxIn, TxOut,
                     Update)
import           MetaData (MetaData)
import           Updates (PParamsUpdate)

cddlTests :: TestTree
cddlTests = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @TxBody        30 "transaction_body"
    , cddlTest @TxOut         30 "transaction_output"
    , cddlTest @DCert         30 "delegation_certificate"
    , cddlTest @TxIn          30 "transaction_input"
    , cddlTest @MetaData      10 "transaction_metadata"
    , cddlTest @MultiSig      30 "script"
    , cddlTest @Update        30 "full_update"
    , cddlTest @AVUpdate      30 "application_version_update_votes"
    , cddlTest @PPUpdate      30 "protocol_param_update_votes"
    , cddlTest @PParamsUpdate 30 "protocol_param_update"
    , cddlTest @PParamsUpdate 30 "protocol_param_update"
    , cddlTest @Tx            30 "transaction"
    -- TODO reenable tests below
    --, cddlTest @Block         30 "block"
    --, cddlTest @BHeader       30 "header"
    --, cddlTest @BHBody 30 "header_body"
    --, cddlTest @OCert  30 "operational_cert"
    --, cddlTest @Addr   20 "address"
    ] <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "../cddl-spec/shelley.cddl"
  crypto <- BSL.readFile "../cddl-spec/mock/crypto.cddl"
  finset <- BSL.readFile "../cddl-spec/mock/finset.cddl"
  pure $ base <> crypto <> finset

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
