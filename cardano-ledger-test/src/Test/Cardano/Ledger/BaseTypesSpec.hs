{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.BaseTypesSpec where

import Cardano.Ledger.BaseTypes
import Data.Aeson
import Data.Either
import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.Maybe
import Data.Ratio
import Data.Scientific
import Data.Word
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary UnitInterval where
  arbitrary = do
    w2 <- genValid
    if w2 == 0
      then arbitrary
      else do
        w1 <- genValid
        pure $
          fromMaybe (error "Impossible: Abitrary UnitInterval") $
            mkUnitInterval $
              if w1 > w2
                then w2 % w1
                else w1 % w2

partialUnitIntervalFromRational :: Rational -> UnitInterval
partialUnitIntervalFromRational r
  | toInteger n /= numerator r || toInteger d /= denominator r =
    error $ "Overflow detected: " ++ show r
  | otherwise =
    fromMaybe (error "Unexpected negative interval") $ mkUnitInterval (n % d)
  where
    n = fromIntegral (numerator r)
    d = fromIntegral (denominator r)

baseTypesSpec :: TestTree
baseTypesSpec = do
  testGroup
    "UnitInterval"
    [ testGroup
        "fromScientificUnitInterval"
        [ localOption (mkTimeout 500000) $
            testCase "Check divergence" $
              expectLeft (fromScientificUnitInterval (scientific 10 1234567893456)),
          testCase "Check overflow" $
            expectLeft (fromScientificUnitInterval 3.00141592653589793e-7),
          testCase "Check too big" $
            expectLeft (fromScientificUnitInterval 1.01),
          testCase "Check negative" $
            expectLeft (fromScientificUnitInterval (-1e-3)),
          testProperty "Rational roundtrip (mkUnitInterval . intervalValue)" $ \ui ->
            Just ui === mkUnitInterval (intervalValue ui),
          testProperty "Scientific valid roundtrip" $ \ui ->
            case fromRationalRepetendLimited 20 (unitIntervalToRational ui) of
              Left (s, r) ->
                ui === partialUnitIntervalFromRational (toRational s + r)
              Right (s, Nothing) ->
                classify
                  True
                  "no-repeat digits"
                  (Right ui === fromScientificUnitInterval s)
              Right (s, Just r) ->
                ui === partialUnitIntervalFromRational (toRationalRepetend s r),
          localOption (mkTimeout 500000) $
            testProperty
              "Scientific roundtrip (fromRational . unitIntervalToRational . fromScientific)"
              $ forAll genValid $ \s ->
                case fromScientificUnitInterval s of
                  Right ui -> s === fromRational (unitIntervalToRational ui)
                  Left _ ->
                    s < 0 .||. s > 1
                      .||. isNothing
                        (toBoundedInteger (s * 10 ^ (19 :: Int)) :: Maybe Word64)
        ],
      testGroup
        "JSON"
        [ testProperty "ToJSON/FromJSON roundtrip up to an epsilon" $ \ui ->
            within 500000 $
              case eitherDecode (encode ui) of
                Left err -> error err
                Right ui' ->
                  abs (unitIntervalToRational ui - unitIntervalToRational ui')
                    < 1e-18
        ]
    ]
  where
    expectLeft = assertBool "Expected Left" . isLeft
