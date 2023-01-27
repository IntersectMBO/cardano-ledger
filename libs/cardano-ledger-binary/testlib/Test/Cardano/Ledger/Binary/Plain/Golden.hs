{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Binary.Plain.Golden (
  Enc (E),
  expectGoldenEncoding,
  expectGoldenEncCBOR,
  expectGoldenEncBytes,
  expectGoldenEncLazyBytes,
  expectGoldenEncHexBytes,
) where

import Cardano.Ledger.Binary.Plain
import qualified Data.ByteString as BS
import Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Binary.TreeDiff
import Test.Hspec

data Enc where
  E :: EncCBOR a => a -> Enc
  (:<>:) :: Enc -> Enc -> Enc

instance EncCBOR Enc where
  encCBOR (E s) = encCBOR s
  encCBOR (a :<>: b) = encCBOR a <> encCBOR b

instance Semigroup Enc where
  (<>) = (:<>:)

instance Monoid Enc where
  mempty = E (mempty :: Encoding)

expectGoldenEncoding :: (a -> Encoding) -> (b -> Encoding) -> a -> b -> Expectation
expectGoldenEncoding encActual encExpected actual expected =
  expectGoldenEncLazyBytes (encActual actual) (serialize (encExpected expected))

expectGoldenEncCBOR :: (EncCBOR a, EncCBOR b) => a -> b -> Expectation
expectGoldenEncCBOR = expectGoldenEncoding encCBOR encCBOR

expectGoldenEncBytes :: EncCBOR a => a -> BS.ByteString -> Expectation
expectGoldenEncBytes a = expectGoldenEncLazyBytes a . BSL.fromStrict

expectGoldenEncLazyBytes :: EncCBOR a => a -> BSL.ByteString -> Expectation
expectGoldenEncLazyBytes actual =
  expectExprEqualWithMessage "Encoding did not match expectation" actualBytes
  where
    actualBytes = serialize (encCBOR actual)

expectGoldenEncHexBytes :: EncCBOR a => a -> BS.ByteString -> Expectation
expectGoldenEncHexBytes actual hexBytes = do
  case BS16.decode hexBytes of
    Left err -> expectationFailure $ "Unexpected failure during Base16 decoding: " ++ err
    Right expectedBytes ->
      expectGoldenEncBytes actual expectedBytes
      where
