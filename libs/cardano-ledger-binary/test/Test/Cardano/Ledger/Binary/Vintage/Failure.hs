{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Ledger.Binary.Vintage.Failure (tests) where

import Cardano.Ledger.Binary hiding (Range)
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

data VEncoding = VEncoding Version Encoding

mkByronEncoding :: Encoding -> VEncoding
mkByronEncoding = VEncoding byronProtVer

instance Show VEncoding where
  show (VEncoding v enc) = show (toPlainEncoding v enc)

genInvalidNonEmptyCBOR :: Gen VEncoding -- NonEmpty Bool
genInvalidNonEmptyCBOR = pure (mkByronEncoding (encCBOR ([] :: [Bool])))

genInvalidEitherCBOR :: Gen VEncoding -- Either Bool Bool
genInvalidEitherCBOR = do
  b <- Gen.bool
  pure (mkByronEncoding (encodeListLen 2 <> encodeWord 3 <> encCBOR b))

genNegativeInteger :: Gen Integer
genNegativeInteger =
  negate . toInteger <$> Gen.word64 (Range.exponential 1 maxBound)

----------------------------------------------------------------------
-------------------------   Properties   -----------------------------

prop_shouldFailNonEmpty :: Property
prop_shouldFailNonEmpty = property $ do
  VEncoding v ne <- forAll genInvalidNonEmptyCBOR
  assertIsLeft (decode v ne :: Either DecoderError (NonEmpty Bool))

prop_shouldFailEither :: Property
prop_shouldFailEither = property $ do
  VEncoding v e <- forAll genInvalidEitherCBOR
  assertIsLeft (decode v e :: Either DecoderError (Either Bool Bool))

prop_shouldFailMaybe :: Property
prop_shouldFailMaybe = property $ do
  VEncoding v e <- forAll genInvalidEitherCBOR
  assertIsLeft (decode v e :: Either DecoderError (Maybe Bool))

prop_shouldFailSetTag :: Property
prop_shouldFailSetTag = property $ do
  VEncoding v set <- forAll genInvalidEitherCBOR
  let wrongTag = encodeTag 266
  assertIsLeft (decode v (wrongTag <> set) :: Either DecoderError (Set Int))

prop_shouldFailSet :: Property
prop_shouldFailSet = property $ do
  ls <- forAll $ Gen.list (Range.constant 0 20) (Gen.int Range.constantBounded)
  let set =
        encodeTag 258
          <> encodeListLen (fromIntegral (length ls + 2))
          <> mconcat (encCBOR <$> (4 : 3 : ls))
  assertIsLeft (decode byronProtVer set :: Either DecoderError (Set Int))

prop_shouldFailNegativeNatural :: Property
prop_shouldFailNegativeNatural = property $ do
  n <- forAll genNegativeInteger
  assertIsLeft (decode byronProtVer (encCBOR n) :: Either DecoderError Natural)

---------------------------------------------------------------------
------------------------------- helpers -----------------------------

assertIsLeft :: (HasCallStack, MonadTest m) => Either DecoderError b -> m ()
assertIsLeft (Right _) = withFrozenCallStack $ failWith Nothing "This should have Left : failed"
assertIsLeft (Left !x) = case x of
  DecoderErrorDeserialiseFailure _ (DeserialiseFailure _ str) | not (null str) -> success
  DecoderErrorCanonicityViolation _ -> success
  DecoderErrorCustom _ _ -> success
  DecoderErrorEmptyList _ -> success
  DecoderErrorLeftover _ _ -> success
  DecoderErrorSizeMismatch _ _ _ -> success
  DecoderErrorUnknownTag _ i | i > 0 -> success
  _ -> success

decode :: DecCBOR a => Version -> Encoding -> Either DecoderError a
decode version enc =
  let encoded = serialize version enc
   in decodeFull version encoded
