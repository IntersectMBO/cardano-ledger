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
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.Shelley.Tx (hashScript)
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import Data.MemoBytes (MemoBytes (Memo))
import Data.Sequence.Strict (fromList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (empty)
import Data.String (fromString)
import GHC.Records
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

txM :: Mary.TxBody TestEra
txM =
  Mary.TxBody
    empty
    StrictSeq.empty
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 6)
    (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
    SNothing
    SNothing
    testmint

testmint :: Value TestCrypto
testmint = Value 0 (Map.singleton policyId (Map.singleton aname 2))
  where
    policyId = PolicyID . hashScript @TestEra . RequireAnyOf $ fromList []
    aname = AssetName $ fromString "asset name"

bytes :: Mary.TxBody era -> ShortByteString
bytes (Mary.TxBodyConstr (Memo _ b)) = b

fieldTests :: TestTree
fieldTests =
  testGroup
    "getField tests"
    [ testCase "inputs" (assertEqual "inputs" (getField @"inputs" txM) empty),
      testCase
        "outputs"
        ( assertEqual
            "outputs"
            (getField @"outputs" txM)
            StrictSeq.empty
        ),
      testCase "certs" (assertEqual "certs" (getField @"certs" txM) StrictSeq.empty),
      testCase "wdrls" (assertEqual "wdrls" (getField @"wdrls" txM) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (getField @"txfee" txM) (Coin 6)),
      testCase
        "vldt"
        ( assertEqual
            "vldt"
            (getField @"vldt" txM)
            (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
        ),
      testCase "update" (assertEqual "update" (getField @"update" txM) SNothing),
      testCase "adHash" (assertEqual "adHash" (getField @"adHash" txM) SNothing),
      testCase "mint" (assertEqual "mint" (getField @"mint" txM) testmint)
    ]

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests,
      testCase "length" (assertEqual "length" 57 (Short.length (bytes txM)))
    ]
