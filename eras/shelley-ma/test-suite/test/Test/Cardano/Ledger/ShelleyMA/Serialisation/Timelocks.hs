{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks
  ( timelockTests,
    MultiSig,
  )
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Core (eraProtVerHigh, eraProtVerLow)
import Cardano.Ledger.SafeHash (originalBytes)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Sequence.Strict (fromList)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation, roundTripAnnExpectation)
import Test.Cardano.Ledger.Binary.TreeDiff (HexBytes (HexBytes), expectExprEqual)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

-- ================================================================

s1 :: Timelock Shelley
s1 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire 18])

s2 :: Timelock Shelley
s2 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire (SlotNo 23)])

s3 :: Timelock Shelley
s3 = RequireAllOf (fromList [s1, s2])

-- ================================================================

timelockTests :: TestTree
timelockTests =
  testGroup
    "Timelock tests"
    [ testCase "s1" $ roundTripAnnExpectation (eraProtVerHigh @Allegra) s1,
      testCase "s2" $ roundTripAnnExpectation (eraProtVerHigh @Allegra) s2,
      testCase "s3" $ roundTripAnnExpectation (eraProtVerHigh @Allegra) s3,
      testProperty "roundtripTimelock" $
        roundTripAnnExpectation @(Timelock Shelley) (eraProtVerHigh @Allegra),
      testProperty "MultiSig deserialises as Timelock" $
        embedTripAnnExpectation @(MultiSig Shelley)
          @(Timelock Allegra)
          (eraProtVerHigh @Shelley)
          (eraProtVerLow @Allegra)
          ( \timelock multiSig ->
              expectExprEqual (HexBytes (originalBytes timelock)) (HexBytes (originalBytes multiSig))
          )
    ]
