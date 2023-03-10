{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody (
  txBodyTest,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.TxBody (MaryEraTxBody (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.MemoBytes (getMemoRawBytes)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Withdrawals (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (fromList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (empty)
import Data.String (fromString)
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.HUnit

-- ====================================================================================================
-- Make a TxBody to test with

txM :: TxBody Mary
txM =
  mkBasicTxBody
    & feeTxBodyL .~ Coin 6
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42))
    & mintTxBodyL .~ testMint

testMint :: MultiAsset StandardCrypto
testMint = MultiAsset $ Map.singleton policyId (Map.singleton aname 2)
  where
    policyId = PolicyID . hashScript @Mary . RequireAnyOf $ fromList []
    aname = AssetName $ fromString "asset name"

fieldTests :: TestTree
fieldTests =
  testGroup
    "Tests for lenses"
    [ testCase "inputs" (assertEqual "inputs" (txM ^. inputsTxBodyL) empty)
    , testCase "outputs" (assertEqual "outputs" (txM ^. outputsTxBodyL) StrictSeq.empty)
    , testCase "certs" (assertEqual "certs" (txM ^. certsTxBodyL) StrictSeq.empty)
    , testCase "withdrawals" (assertEqual "withdrawals" (txM ^. withdrawalsTxBodyL) (Withdrawals Map.empty))
    , testCase "txfree" (assertEqual "txfree" (txM ^. feeTxBodyL) (Coin 6))
    , testCase "vldt" $
        assertEqual "vldt" (txM ^. vldtTxBodyL) $
          ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42))
    , testCase "update" (assertEqual "update" (txM ^. updateTxBodyL) SNothing)
    , testCase "adHash" (assertEqual "adHash" (txM ^. auxDataHashTxBodyL) SNothing)
    , testCase "mint" (assertEqual "mint" (txM ^. mintTxBodyL) testMint)
    ]

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests
    , testCase "length" (assertEqual "length" 57 (Short.length (getMemoRawBytes txM)))
    ]
