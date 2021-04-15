{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Mary.Golden
-- Description : Golden Tests for the Mary era
module Test.Cardano.Ledger.Alonzo.Golden
  ( goldenUTxOEntryMinAda
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (hashScript)
import Test.Cardano.Ledger.EraBuffet (StandardCrypto)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Cardano.Ledger.Mary.Golden
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)

-- | ada cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
adaPerUTxOWordLocal :: Coin
adaPerUTxOWordLocal = Coin $ quot minUTxOValueShelleyMA utxoEntrySizeWithoutValLocal
  where
    utxoEntrySizeWithoutValLocal = 29
    minUTxOValueShelleyMA = 1000000

calcMinUTxO :: TxOut era -> Coin
calcMinUTxO tout = Coin (utxoEntrySize tout * adaPerUTxOWordLocal)

goldenUTxOEntryMinAda :: TestTree
goldenUTxOEntryMinAda =
  testGroup
    "golden tests - UTxOEntryMinAda"
    [ testCase "one policy, one (smallest) name, no datum hash" $
        scaledMinDeposit
          ( Value 1407406 $
              Map.singleton pid1 (Map.fromList [(smallestName, 1)])
          )
          minUTxO
          @?= Coin 1407406,
      testCase "one policy, one (smallest) name, yes datum hash" $
          scaledMinDeposit
            ( Value 1407406 $
                Map.singleton pid1 (Map.fromList [(smallestName, 1)])
            )
            minUTxO
            @?= Coin 1407406,
      testCase "one policy, one (small) name" $
        scaledMinDeposit
          ( Value 1444443 $
              Map.singleton
                pid1
                (Map.fromList [(smallName '1', 1)])
          )
          minUTxO
          @?= Coin 1444443,
      testCase "one policy, three (small) name" $
        scaledMinDeposit
          ( Value 1555554 $
              Map.singleton
                pid1
                ( Map.fromList
                    [ (smallName '1', 1),
                      (smallName '2', 1),
                      (smallName '3', 1)
                    ]
                )
          )
          minUTxO
          @?= Coin 1555554,
      testCase "one policy, one (largest) name" $
        scaledMinDeposit
          ( Value 1555554 $
              Map.singleton
                pid1
                (Map.fromList [(largestName 'a', 1)])
          )
          minUTxO
          @?= Coin 1555554,
      testCase "one policy, three (largest) name" $
        scaledMinDeposit
          ( Value 1962961 $
              Map.singleton
                pid1
                ( Map.fromList
                    [ (largestName 'a', 1),
                      (largestName 'b', 1),
                      (largestName 'c', 1)
                    ]
                )
          )
          minUTxO
          @?= Coin 1962961,
      testCase "two policies, one (smallest) name" $
        scaledMinDeposit
          ( Value 1592591 $
              Map.fromList
                [ ( pid1,
                    (Map.fromList [(smallestName, 1)])
                  ),
                  ( pid2,
                    (Map.fromList [(smallestName, 1)])
                  )
                ]
          )
          minUTxO
          @?= Coin 1592591,
      testCase "two policies, two (small) names" $
        scaledMinDeposit
          ( Value 1629628 $
              Map.fromList
                [ ( pid1,
                    (Map.fromList [(smallName '1', 1)])
                  ),
                  ( pid2,
                    (Map.fromList [(smallName '2', 1)])
                  )
                ]
          )
          minUTxO
          @?= Coin 1629628,
      testCase "three policies, ninety-six (small) names" $
        scaledMinDeposit
          ( Value 7407400 $
              Map.fromList
                [ ( pid1,
                    (Map.fromList $ map ((,1) . smallName . chr) [32 .. 63])
                  ),
                  ( pid2,
                    (Map.fromList $ map ((,1) . smallName . chr) [64 .. 95])
                  ),
                  ( pid3,
                    (Map.fromList $ map ((,1) . smallName . chr) [96 .. 127])
                  )
                ]
          )
          minUTxO
          @?= Coin 7407400
    ]
