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
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.ShelleyMA.Rules (scaledMinDeposit)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Slot (SlotNo (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
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
smallestName = AssetName $ SBS.pack []

-- | The small asset names have length one
smallName :: Word8 -> AssetName
smallName c = AssetName $ SBS.pack [c]

-- | The largest asset names have length thirty-two
largestName :: Word8 -> AssetName
largestName c = AssetName . SBS.pack $ c : [1 .. 31]

-- | try using a real asset name the way the CLI handles input
realName :: AssetName
realName = AssetName . SBS.toShort . Text.encodeUtf8 . Text.pack $ "ATADAcoin"

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
          ( MaryValue 1407406 $
              MultiAsset $
                Map.singleton pid1 (Map.fromList [(smallestName, 1)])
          )
          minUTxO
          @?= Coin 1407406,
      testCase "one policy, one (small) name" $
        scaledMinDeposit
          ( MaryValue 1444443 $
              MultiAsset $
                Map.singleton
                  pid1
                  (Map.fromList [(smallName 1, 1)])
          )
          minUTxO
          @?= Coin 1444443,
      testCase "one policy, one (real) name" $
        scaledMinDeposit
          ( MaryValue 1444443 $
              MultiAsset $
                Map.singleton
                  pid1
                  (Map.fromList [(realName, 1)])
          )
          minUTxO
          @?= Coin 1481480,
      testCase "one policy, three (small) name" $
        scaledMinDeposit
          ( MaryValue 1555554 $
              MultiAsset $
                Map.singleton
                  pid1
                  ( Map.fromList
                      [ (smallName 1, 1),
                        (smallName 2, 1),
                        (smallName 3, 1)
                      ]
                  )
          )
          minUTxO
          @?= Coin 1555554,
      testCase "one policy, one (largest) name" $
        scaledMinDeposit
          ( MaryValue 1555554 $
              MultiAsset $
                Map.singleton
                  pid1
                  (Map.fromList [(largestName 65, 1)])
          )
          minUTxO
          @?= Coin 1555554,
      testCase "one policy, three (largest) name" $
        scaledMinDeposit
          ( MaryValue 1962961 $
              MultiAsset $
                Map.singleton
                  pid1
                  ( Map.fromList
                      [ (largestName 65, 1),
                        (largestName 66, 1),
                        (largestName 67, 1)
                      ]
                  )
          )
          minUTxO
          @?= Coin 1962961,
      testCase "two policies, one (smallest) name" $
        scaledMinDeposit
          ( MaryValue 1592591 $
              MultiAsset $
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
          ( MaryValue 1629628 $
              MultiAsset $
                Map.fromList
                  [ ( pid1,
                      Map.fromList [(smallName 1, 1)]
                    ),
                    ( pid2,
                      Map.fromList [(smallName 2, 1)]
                    )
                  ]
          )
          minUTxO
          @?= Coin 1629628,
      testCase "three policies, ninety-six (small) names" $
        scaledMinDeposit
          ( MaryValue 7407400 $
              MultiAsset $
                Map.fromList
                  [ ( pid1,
                      Map.fromList $ map ((,1) . smallName) [32 .. 63]
                    ),
                    ( pid2,
                      Map.fromList $ map ((,1) . smallName) [64 .. 95]
                    ),
                    ( pid3,
                      Map.fromList $ map ((,1) . smallName) [96 .. 127]
                    )
                  ]
          )
          minUTxO
          @?= Coin 7407400
    ]
