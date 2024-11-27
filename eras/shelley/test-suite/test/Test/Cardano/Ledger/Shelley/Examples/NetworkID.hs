{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Examples.NetworkID (
  testPoolNetworkId,
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  Network (..),
  PoolEnv (..),
  PoolParams (..),
  RewardAccount (..),
  ShelleyPOOL,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Slot (EpochNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default (def)
import Lens.Micro
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

type ShelleyTest = ShelleyEra

shelleyPV :: ProtVer
shelleyPV = ProtVer (natVersion @2) 0

alonzoPV :: ProtVer
alonzoPV = ProtVer (natVersion @5) 0

data Expectation = ExpectSuccess | ExpectFailure
  deriving (Show, Eq)

testPoolNetworkID ::
  ProtVer ->
  PoolParams ->
  Expectation ->
  Assertion
testPoolNetworkID pv poolParams e = do
  let st =
        runShelleyBase $
          applySTSTest @(ShelleyPOOL ShelleyTest)
            ( TRC
                ( PoolEnv (EpochNo 0) $ emptyPParams & ppProtocolVersionL .~ pv
                , def
                , RegPool poolParams
                )
            )
  case (st, e) of
    (Right _, ExpectSuccess) -> assertBool "" True
    (Left _, ExpectFailure) -> assertBool "" True
    (Right _, ExpectFailure) -> assertBool "We expected failure." False
    (Left _, ExpectSuccess) -> assertBool "We expected success." False

matchingNetworkIDPoolParams :: PoolParams
matchingNetworkIDPoolParams =
  Cast.alicePoolParams {ppRewardAccount = RewardAccount Testnet Cast.aliceSHK}

-- test globals use Testnet

mismatchingNetworkIDPoolParams :: PoolParams
mismatchingNetworkIDPoolParams =
  Cast.alicePoolParams {ppRewardAccount = RewardAccount Mainnet Cast.aliceSHK}

-- test globals use Testnet

testPoolNetworkId :: TestTree
testPoolNetworkId =
  testGroup
    "Network IDs"
    [ testCase "Incorrect Network ID is allowed pre-Alonzo" $
        testPoolNetworkID
          shelleyPV
          mismatchingNetworkIDPoolParams
          ExpectSuccess
    , testCase "Incorrect Network ID is NOT allowed in Alonzo" $
        testPoolNetworkID
          alonzoPV
          mismatchingNetworkIDPoolParams
          ExpectFailure
    , testCase "Correct Network ID is allowed pre-Alonzo" $
        testPoolNetworkID
          shelleyPV
          matchingNetworkIDPoolParams
          ExpectSuccess
    , testCase "Correct Network ID is allowed in Alonzo" $
        testPoolNetworkID
          alonzoPV
          matchingNetworkIDPoolParams
          ExpectSuccess
    ]
