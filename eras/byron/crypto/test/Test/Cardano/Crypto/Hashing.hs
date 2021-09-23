{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Hashing
  ( tests,
  )
where

import Cardano.Binary (ToCBOR)
import Cardano.Crypto (decodeAbstractHash, hashHexF, serializeCborHash)
import Cardano.Prelude
import Formatting (sformat)
import Hedgehog
  ( Gen,
    Property,
    checkParallel,
    discover,
    forAll,
    property,
    withTests,
    (/==),
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover

--------------------------------------------------------------------------------
-- Hashing Properties
--------------------------------------------------------------------------------

prop_hashInequalityBool :: Property
prop_hashInequalityBool = hashInequality Gen.bool

prop_hashInequalityUnitList :: Property
prop_hashInequalityUnitList =
  hashInequality $ Gen.list (Range.constant 0 50) (pure ())

prop_hashInequalityListOfList :: Property
prop_hashInequalityListOfList =
  hashInequality $
    Gen.list
      (Range.constant 0 10)
      (Gen.list (Range.constant 0 20) (Gen.maybe $ Gen.int Range.constantBounded))

-- | A golden test so that tests fail if the hash function changes
prop_goldenHash :: Property
prop_goldenHash =
  withTests 1
    . property
    $ sformat hashHexF (serializeCborHash (1 :: Word64))
      === "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"

-- | Check that 'decodeAbstractHash' correctly decodes hash values
prop_decodeAbstractHash :: Property
prop_decodeAbstractHash = property $ do
  a <- serializeCborHash <$> forAll (Gen.int Range.constantBounded)
  decodeAbstractHash (sformat hashHexF a) === Right a

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Check that hashing two different @a@ values gives different hashes
hashInequality :: (Eq a, Show a, ToCBOR a) => Gen a -> Property
hashInequality genA = property $ do
  a <- forAll genA
  b <- forAll $ Gen.filter (/= a) genA
  serializeCborHash a /== serializeCborHash b
