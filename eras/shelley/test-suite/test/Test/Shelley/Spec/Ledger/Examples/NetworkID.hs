{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.NetworkID
  ( testPoolNetworkId,
  )
where

import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (def)
import Shelley.Spec.Ledger.API
  ( DCert (..),
    Network (..),
    POOL,
    PParams' (..),
    PoolCert (..),
    PoolEnv (..),
    PoolParams (..),
    ProtVer (..),
    RewardAcnt (..),
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

type ShelleyTest = ShelleyEra C_Crypto

shelleyPV :: ProtVer
shelleyPV = ProtVer 2 0

alonzoPV :: ProtVer
alonzoPV = ProtVer 5 0

data Expectation = ExpectSuccess | ExpectFailure
  deriving (Show, Eq)

testPoolNetworkID ::
  ProtVer ->
  PoolParams C_Crypto ->
  Expectation ->
  Assertion
testPoolNetworkID pv poolParams e = do
  let st =
        runShelleyBase $
          applySTSTest @(POOL ShelleyTest)
            (TRC (PoolEnv (SlotNo 0) def {_protocolVersion = pv}, def, DCertPool (RegPool poolParams)))
  case (st, e) of
    (Right _, ExpectSuccess) -> assertBool "" True
    (Left _, ExpectFailure) -> assertBool "" True
    (Right _, ExpectFailure) -> assertBool "We expected failure." False
    (Left _, ExpectSuccess) -> assertBool "We expected success." False

matchingNetworkIDPoolParams :: PoolParams C_Crypto
matchingNetworkIDPoolParams =
  Cast.alicePoolParams {_poolRAcnt = RewardAcnt Testnet Cast.aliceSHK}

-- test globals use Testnet

mismatchingNetworkIDPoolParams :: PoolParams C_Crypto
mismatchingNetworkIDPoolParams =
  Cast.alicePoolParams {_poolRAcnt = RewardAcnt Mainnet Cast.aliceSHK}

-- test globals use Testnet

testPoolNetworkId :: TestTree
testPoolNetworkId =
  testGroup
    "Network IDs"
    [ testCase "Incorrect Network ID is allowed pre-Alonzo" $
        testPoolNetworkID
          shelleyPV
          mismatchingNetworkIDPoolParams
          ExpectSuccess,
      testCase "Incorrect Network ID is NOT allowed in Alonzo" $
        testPoolNetworkID
          alonzoPV
          mismatchingNetworkIDPoolParams
          ExpectFailure,
      testCase "Correct Network ID is allowed pre-Alonzo" $
        testPoolNetworkID
          shelleyPV
          matchingNetworkIDPoolParams
          ExpectSuccess,
      testCase "Correct Network ID is allowed in Alonzo" $
        testPoolNetworkID
          alonzoPV
          matchingNetworkIDPoolParams
          ExpectSuccess
    ]
