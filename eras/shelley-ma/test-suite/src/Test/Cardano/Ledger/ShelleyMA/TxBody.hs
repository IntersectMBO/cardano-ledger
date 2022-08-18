{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Arbitrary instances
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody
  ( txBodyTest,
    TestEra,
  )
where

import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..), ShelleyMAEraTxBody (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import Data.MemoBytes (MemoBytes (Memo))
import Data.Sequence.Strict (fromList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (empty)
import Data.String (fromString)
import Lens.Micro
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.HUnit

-- ============================================================================================
-- make an example
-- ============================================================================================

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID. We use
-- TestCrypto from Test.Cardano.Ledger.ShelleyMA.TestEra(TestCrypto)

type TestEra = MaryEra TestCrypto

-- ====================================================================================================
-- Make a TxBody to test with

txM :: MATxBody TestEra
txM =
  MATxBody
    empty
    StrictSeq.empty
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 6)
    (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
    SNothing
    SNothing
    testmint

testmint :: MultiAsset TestCrypto
testmint = MultiAsset $ Map.singleton policyId (Map.singleton aname 2)
  where
    policyId = PolicyID . hashScript @TestEra . RequireAnyOf $ fromList []
    aname = AssetName $ fromString "asset name"

bytes :: MATxBody era -> ShortByteString
bytes (TxBodyConstr (Memo _ b)) = b

fieldTests :: TestTree
fieldTests =
  testGroup
    "getField tests"
    [ testCase "inputs" (assertEqual "inputs" (txM ^. inputsTxBodyL) empty),
      testCase "outputs" (assertEqual "outputs" (txM ^. outputsTxBodyL) StrictSeq.empty),
      testCase "certs" (assertEqual "certs" (txM ^. certsTxBodyL) StrictSeq.empty),
      testCase "wdrls" (assertEqual "wdrls" (txM ^. wdrlsTxBodyL) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (txM ^. feeTxBodyL) (Coin 6)),
      testCase "vldt" $
        assertEqual "vldt" (txM ^. vldtTxBodyL) $
          ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)),
      testCase "update" (assertEqual "update" (txM ^. updateTxBodyL) SNothing),
      testCase "adHash" (assertEqual "adHash" (txM ^. auxDataHashTxBodyL) SNothing),
      testCase "mint" (assertEqual "mint" (txM ^. mintTxBodyL) testmint)
    ]

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests,
      testCase "length" (assertEqual "length" 57 (Short.length (bytes txM)))
    ]
