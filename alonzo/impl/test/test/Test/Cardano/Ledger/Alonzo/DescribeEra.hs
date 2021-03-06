module Test.Cardano.Ledger.Alonzo.DescribeEra (describeTest) where

import Cardano.Ledger.DescribeEras (Checks (..), Evidence (..), Witness (..))
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase)

describes :: Checks era => Witness era -> TestTree
describes w = testCase (show w) (assertBool "Does not meets its description" (checks w))

describeTest :: TestTree
describeTest =
  testGroup
    "Era Self-Description Tests by Era"
    [ describes (Shelley Test),
      describes (Allegra Test),
      describes (Mary Test),
      describes (Alonzo Test),
      describes (Shelley Standard),
      describes (Allegra Standard),
      describes (Mary Standard),
      describes (Alonzo Standard)
    ]
