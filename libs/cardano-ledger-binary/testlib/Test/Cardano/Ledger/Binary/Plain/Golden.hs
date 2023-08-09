{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Binary.Plain.Golden (
  Enc (E, Ev, Em),
  DiffView (..),
  expectGoldenEncoding,
  expectGoldenToCBOR,
  expectGoldenEncBytes,
  expectGoldenEncLazyBytes,
  expectGoldenEncHexBytes,
) where

import Cardano.Ledger.Binary (EncCBOR (encCBOR), Version, toPlainEncoding)
import Cardano.Ledger.Binary.Plain
import qualified Data.ByteString as BS
import Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Binary.TreeDiff
import Test.Hspec

data Enc where
  E :: ToCBOR a => a -> Enc
  Ev :: EncCBOR a => Version -> a -> Enc
  Em :: [Enc] -> Enc
  (:<>:) :: Enc -> Enc -> Enc

instance ToCBOR Enc where
  toCBOR (E s) = toCBOR s
  toCBOR (Ev v s) = toPlainEncoding v $ encCBOR s
  toCBOR (Em m) = foldMap toCBOR m
  toCBOR (a :<>: b) = toCBOR a <> toCBOR b

instance Semigroup Enc where
  (<>) = (:<>:)

instance Monoid Enc where
  mempty = E (mempty :: Encoding)

-- | Indicator of the format in which the diff should be displayed.
data DiffView
  = -- | TreeDiff bytes as CBOR Terms
    DiffCBOR
  | -- | TreeDiff bytes as Base64 encoded strings
    DiffHex
  | -- | TreeDiff will be shown on raw bytes.
    DiffRaw
  | -- | Let hspec handle the diffing
    DiffAuto

expectGoldenEncoding ::
  HasCallStack => (a -> Encoding) -> (b -> Encoding) -> DiffView -> a -> b -> Expectation
expectGoldenEncoding encActual encExpected viewDiff actual expected =
  expectGoldenEncBytes viewDiff (encActual actual) (serialize' (encExpected expected))

expectGoldenToCBOR ::
  (HasCallStack, ToCBOR a, ToCBOR b) => DiffView -> a -> b -> Expectation
expectGoldenToCBOR = expectGoldenEncoding toCBOR toCBOR

expectGoldenEncBytes ::
  (HasCallStack, ToCBOR a) => DiffView -> a -> BS.ByteString -> Expectation
expectGoldenEncBytes viewDiff actual expectedBytes = do
  diffAs $ expectExprEqualWithMessage "Encoding did not match expectation"
  -- ensure that it is also valid CBOR
  case decodeFull' actualBytes of
    Left err -> error $ "Type was encoded sucessfully, but as invalid CBOR: " ++ show err
    Right (_ :: Term) -> pure ()
  where
    actualBytes = serialize' (toCBOR actual)
    diffAs ::
      HasCallStack =>
      (forall t. (HasCallStack, Eq t, ToExpr t) => t -> t -> Expectation) ->
      Expectation
    diffAs f =
      case viewDiff of
        DiffCBOR ->
          f (CBORBytes actualBytes) (CBORBytes expectedBytes)
        DiffHex ->
          f (HexBytes actualBytes) (HexBytes expectedBytes)
        DiffRaw -> f actualBytes expectedBytes
        DiffAuto -> actualBytes `shouldBe` expectedBytes

expectGoldenEncLazyBytes ::
  (HasCallStack, ToCBOR a) => DiffView -> a -> BSL.ByteString -> Expectation
expectGoldenEncLazyBytes viewDiff actual = expectGoldenEncBytes viewDiff actual . BSL.toStrict

expectGoldenEncHexBytes ::
  (HasCallStack, ToCBOR a) => DiffView -> a -> BS.ByteString -> Expectation
expectGoldenEncHexBytes viewDiff actual hexBytes = do
  case BS16.decode hexBytes of
    Left err -> expectationFailure $ "Unexpected failure during Base16 decoding: " ++ err
    Right expectedBytes ->
      expectGoldenEncBytes viewDiff actual expectedBytes
