{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Mary.Golden
-- Description : Golden Tests for the Mary era
module Test.Cardano.Ledger.Mary.Golden
  ( goldenScaledMinDeposit,
    pid1,
    pid2,
    pid3,
    smallName,
    smallestName,
    realName,
    minUTxO,
    largestName,
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.ShelleyMA.Rules.Utxo (scaledMinDeposit)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Slot (SlotNo (..))
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Shelley.Spec.Ledger.Tx (hashScript)
import Test.Cardano.Ledger.EraBuffet (StandardCrypto)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- Golden Tests for the scaled MinUTxO function
--

pid1 :: PolicyID StandardCrypto
pid1 =
  PolicyID $
    hashScript @(MaryEra StandardCrypto) $
      RequireAllOf (StrictSeq.fromList [])

pid2 :: PolicyID StandardCrypto
pid2 =
  PolicyID $
    hashScript @(MaryEra StandardCrypto) $
      RequireAllOf (StrictSeq.fromList [RequireTimeStart (SlotNo 1)])

pid3 :: PolicyID StandardCrypto
pid3 =
  PolicyID $
    hashScript @(MaryEra StandardCrypto) $
      RequireAllOf (StrictSeq.fromList [RequireTimeExpire (SlotNo 1)])

-- | The smallest asset name has length zero
smallestName :: AssetName
smallestName = AssetName $ BS.pack ""

-- | The small asset names have length one
smallName :: Char -> AssetName
smallName c = AssetName . BS.pack $ [c]

-- | The largest asset names have length thirty-two
largestName :: Char -> AssetName
largestName c = AssetName . BS.pack $ c : "0123456789ABCDEFGHIJ0123456789A"

-- | try using a real asset name the way the CLI handles input
realName :: AssetName
realName = AssetName $ (Text.encodeUtf8 . Text.pack) "ATADAcoin"

-- | This is the current value of the protocol parameter
--  at the time this comment was written, namely one Ada.
minUTxO :: Coin
minUTxO = Coin $ 1000 * 1000

goldenScaledMinDeposit :: TestTree
goldenScaledMinDeposit =
  testGroup
    "golden tests - scaledMinDeposit"
    [ testCase "one policy, one (smallest) name" $
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
      testCase "one policy, one (real) name" $
        scaledMinDeposit
          ( Value 1444443 $
              Map.singleton
                pid1
                (Map.fromList [(realName, 1)])
          )
          minUTxO
          @?= Coin 1481480,
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
                    Map.fromList [(smallestName, 1)]
                  ),
                  ( pid2,
                    Map.fromList [(smallestName, 1)]
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
                    Map.fromList [(smallName '1', 1)]
                  ),
                  ( pid2,
                    Map.fromList [(smallName '2', 1)]
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
                    Map.fromList $ map ((,1) . smallName . chr) [32 .. 63]
                  ),
                  ( pid2,
                    Map.fromList $ map ((,1) . smallName . chr) [64 .. 95]
                  ),
                  ( pid3,
                    Map.fromList $ map ((,1) . smallName . chr) [96 .. 127]
                  )
                ]
          )
          minUTxO
          @?= Coin 7407400
    ]
