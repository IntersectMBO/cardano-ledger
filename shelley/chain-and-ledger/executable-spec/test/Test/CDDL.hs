{-# Language Rank2Types #-}
{-# Language TypeApplications #-}
{-# Language NoImplicitPrelude #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}

module Test.CDDL
  ( cddlTests
  )
 where

import qualified Prelude

import Cardano.Prelude
import Cardano.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as Char8 (lines, unpack)
import System.Process.ByteString.Lazy



import           Test.Tasty
import           Test.Tasty.HUnit

import ConcreteCryptoTypes (TxBody, TxOut, DCert, BHeader, Tx, Block, TxIn)
import MetaData (MetaData)

cddlTests :: TestTree
cddlTests = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @TxBody   300 "transaction_body"
    , cddlTest @TxOut    300 "transaction_output"
    , cddlTest @DCert    300 "delegation_certificate"
    , cddlTest @BHeader  300 "header"
    , cddlTest @TxIn     300 "transaction_input"
    , cddlTest @MetaData 10  "transaction_metadata"
    , cddlTest @Block    30  "block"
    , cddlTest @Tx       300 "transaction"
    ] <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "shelley.cddl"
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
  examples <- Char8.lines <$> generateCBORDiagStdIn n cddl
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

generateCBORDiagStdIn :: Int -> BSL.ByteString -> IO BSL.ByteString
generateCBORDiagStdIn rounds cddl = do
  (_, result,_) <- readProcessWithExitCode "cddl" ["-", "generate", Prelude.show rounds] cddl
  pure result

diagToBytes :: BSL.ByteString -> IO BSL.ByteString
diagToBytes diag =  do
  (_, result,_) <- readProcessWithExitCode "diag2cbor.rb" ["-"] diag
  pure result

verifyConforming :: BSL.ByteString -> BSL.ByteString -> Assertion
verifyConforming _value _cddl = pure () -- TODO
