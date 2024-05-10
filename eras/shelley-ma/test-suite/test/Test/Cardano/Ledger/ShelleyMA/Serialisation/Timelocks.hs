{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks (
  timelockTests,
  MultiSig,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Core (NativeScript, eraProtVerHigh, eraProtVerLow)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.SafeHash (originalBytes)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  pattern RequireAllOf,
 )
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Sequence.Strict (fromList)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation, roundTripAnnExpectation)
import Test.Cardano.Ledger.Binary.TreeDiff (HexBytes (HexBytes), expectExprEqual)
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

-- ================================================================

s1 :: AllegraEraScript era => NativeScript era
s1 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire 18])

s2 :: AllegraEraScript era => NativeScript era
s2 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire (SlotNo 23)])

s3 :: AllegraEraScript era => NativeScript era
s3 = RequireAllOf (fromList [s1, s2])

-- ================================================================

timelockTests :: TestTree
timelockTests =
  testGroup
    "Timelock tests"
    $ ( testCase "Timelock examples roundtrip - Allegra" . roundTripAnnExpectation
          <$> [s1 @Allegra, s2 @Allegra, s3 @Allegra]
      )
      ++ ( testCase "Timelock examples roundtrip - Mary" . roundTripAnnExpectation
            <$> [s1 @Mary, s2 @Mary, s3 @Mary]
         )
      ++ [ testProperty "roundtripTimelock prop - Allegra" $ roundTripAnnExpectation @(Timelock Allegra)
         , testProperty "roundtripTimelock prop - Mary" $ roundTripAnnExpectation @(Timelock Mary)
         , testProperty "MultiSig deserialises as Timelock" $
            embedTripAnnExpectation @(MultiSig Shelley)
              @(Timelock Allegra)
              (eraProtVerHigh @Shelley)
              (eraProtVerLow @Allegra)
              ( \timelock multiSig ->
                  expectExprEqual (HexBytes (originalBytes timelock)) (HexBytes (originalBytes multiSig))
              )
         ]
