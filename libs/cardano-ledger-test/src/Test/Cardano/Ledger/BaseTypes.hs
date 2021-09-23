{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.BaseTypes where

import qualified Cardano.Binary as Binary
import Cardano.Ledger.BaseTypes
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.GenValidity (genValid)
import Data.GenValidity.Scientific ()
import Data.Scientific
import Data.Typeable
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

expectLeft :: Either a b -> Assertion
expectLeft = assertBool "Expected Left" . isLeft

boundedRationalTests ::
  forall a.
  ( BoundedRational a,
    ToJSON a,
    FromJSON a,
    Binary.ToCBOR a,
    Binary.FromCBOR a,
    Arbitrary a,
    Show a,
    Ord a
  ) =>
  [(String, ByteString)] ->
  TestTree
boundedRationalTests badJSONValues =
  testGroup
    (showsTypeRep (typeRep (Proxy :: Proxy a)) "")
    [ testGroup
        "Rational roundtrip"
        [ testProperty "(boundRational . unboundRational)" $ \(bi :: a) ->
            Just bi === boundRational (unboundRational bi),
          testProperty "(unboundRational . boundRational)" $
            forAll genValid $ \r ->
              maybe
                (property True)
                ((r ===) . unboundRational)
                (boundRational r :: Maybe a),
          testProperty "bounding produces valid values within bounds" $
            forAll genValid $ \r ->
              case boundRational r of
                Nothing -> property True
                Just (br :: a) ->
                  conjoin
                    [ minBound <= br,
                      br <= maxBound,
                      unboundRational (minBound :: a) <= r,
                      r <= unboundRational (maxBound :: a)
                    ]
        ],
      testGroup "JSON" $
        [ testProperty "ToJSON/FromJSON roundtrip up to an epsilon" $ \(br :: a) ->
            within 500000 $
              case eitherDecode (encode br) of
                Left err -> error err
                Right (br' :: a) ->
                  epsilonMaybeEq 1e-18 (unboundRational br) (unboundRational br'),
          testProperty "Roundtrip to Scientific and back up to an epsilon" $ \ui ->
            within 500000 $
              case fromRationalRepetendLimited 20 (unboundRational ui) of
                Left (s, r) -> Just ui === boundRational (toRational s + r)
                Right (s, Nothing) ->
                  classify
                    True
                    "no-repeat digits"
                    (Right ui === boundedFromJSON (encode s))
                Right (s, Just r) ->
                  Just ui === boundRational (toRationalRepetend s r),
          testProperty "Roundtrip from valid Scientific and back exactly" $
            within 500000 $
              forAll genValid $ \(s :: Scientific) ->
                case eitherDecode (encode s) of
                  Right (ui :: a) -> s === fromRational (unboundRational ui)
                  Left _ -> property True,
          localOption (mkTimeout 500000) $
            testCase "Check divergence" $
              expectLeft (boundedFromJSON "10e1234567893456")
        ]
          ++ [ testCase testName $ expectLeft $ boundedFromJSON invalidInput
               | (testName, invalidInput) <- badJSONValues
             ],
      testProperty "CBOR roundtrip" $ \(br :: a) ->
        either (error . show) (br ==) $ Binary.decodeFull (Binary.serialize br),
      testProperty "CBOR BoundedRational roundtrip" $ \(br :: a) ->
        either (error . show) (br ==) $
          Binary.decodeFullDecoder "BoundedRational" boundedRationalFromCBOR $
            Binary.serializeEncoding (boundedRationalToCBOR br)
    ]
  where
    boundedFromJSON = eitherDecode :: ByteString -> Either String a

baseTypesTests :: TestTree
baseTypesTests = do
  let badJSONValues =
        [("Word64 denominator overflow", "3.14159265358979323e-7"), ("Negative value", "-1e-3")]
  testGroup
    "BoundedRational"
    [ boundedRationalTests @UnitInterval $ badJSONValues ++ [("Too big", "1.01")],
      boundedRationalTests @PositiveUnitInterval $
        badJSONValues ++ [("Zero", "0"), ("Too big", "1.01")],
      boundedRationalTests @PositiveInterval $ badJSONValues ++ [("Zero", "0")],
      boundedRationalTests @NonNegativeInterval badJSONValues
    ]

-- | This test for equality takes into account magnitude of the arguments
epsilonMaybeEq ::
  -- | Epsilon, a maximum tolerated error. Sign is ignored.
  Rational ->
  -- | Expected result.
  Rational ->
  -- | Tested value.
  Rational ->
  Property
epsilonMaybeEq epsilon x y =
  counterexample
    ( concat
        [show x, " /= ", show y, " (Tolerance: ", show diff, " > ", show n, ")"]
    )
    (classify True "Exactly" (x === y) .||. diff <= n)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)
