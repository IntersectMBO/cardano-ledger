{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Ledger.Binary.Vintage.Failure (tests) where

import Cardano.Binary hiding (Range)
import qualified Codec.CBOR.Read as CR
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

{- HLINT ignore "Use record patterns" -}

tests :: IO Bool
tests = checkParallel $$(discover)

----------------------------------------------------------------------
-------------------------   Generators   -----------------------------

genInvalidNonEmptyCBOR :: Gen Encoding -- NonEmpty Bool
genInvalidNonEmptyCBOR = pure (toCBOR ([] :: [Bool]))

genInvalidEitherCBOR :: Gen Encoding -- Either Bool Bool
genInvalidEitherCBOR = do
  b <- Gen.bool
  pure (encodeListLen 2 <> encodeWord 3 <> toCBOR b)

genNegativeInteger :: Gen Integer
genNegativeInteger =
  negate . toInteger <$> Gen.word64 (Range.exponential 1 maxBound)

----------------------------------------------------------------------
-------------------------   Properties   -----------------------------

prop_shouldFailNonEmpty :: Property
prop_shouldFailNonEmpty = property $ do
  ne <- forAll genInvalidNonEmptyCBOR
  assertIsLeft (decode ne :: Either DecoderError (NonEmpty Bool))

prop_shouldFailEither :: Property
prop_shouldFailEither = property $ do
  e <- forAll genInvalidEitherCBOR
  assertIsLeft (decode e :: Either DecoderError (Either Bool Bool))

prop_shouldFailMaybe :: Property
prop_shouldFailMaybe = property $ do
  e <- forAll genInvalidEitherCBOR
  assertIsLeft (decode e :: Either DecoderError (Maybe Bool))

prop_shouldFailSetTag :: Property
prop_shouldFailSetTag = property $ do
  set <- forAll genInvalidEitherCBOR
  let wrongTag = encodeTag 266
  assertIsLeft (decode (wrongTag <> set) :: Either DecoderError (Set Int))

prop_shouldFailSet :: Property
prop_shouldFailSet = property $ do
  ls <- forAll $ Gen.list (Range.constant 0 20) (Gen.int Range.constantBounded)
  let set =
        encodeTag 258
          <> encodeListLen (fromIntegral (length ls + 2))
          <> mconcat (toCBOR <$> (4 : 3 : ls))
  assertIsLeft (decode set :: Either DecoderError (Set Int))

prop_shouldFailNegativeNatural :: Property
prop_shouldFailNegativeNatural = property $ do
  n <- forAll genNegativeInteger
  assertIsLeft (decode (toCBOR n) :: Either DecoderError Natural)

---------------------------------------------------------------------
------------------------------- helpers -----------------------------

assertIsLeft :: (HasCallStack, MonadTest m) => Either DecoderError b -> m ()
assertIsLeft (Right _) = withFrozenCallStack $ failWith Nothing "This should have Left : failed"
assertIsLeft (Left !x) = case x of
  DecoderErrorDeserialiseFailure _ (CR.DeserialiseFailure _ str) | not (null str) -> success
  DecoderErrorCanonicityViolation _ -> success
  DecoderErrorCustom _ _ -> success
  DecoderErrorEmptyList _ -> success
  DecoderErrorLeftover _ _ -> success
  DecoderErrorSizeMismatch _ _ _ -> success
  DecoderErrorUnknownTag _ i | i > 0 -> success
  _ -> success

decode :: FromCBOR a => Encoding -> Either DecoderError a
decode enc =
  let encoded = serializeEncoding enc
   in decodeFull encoded
