{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest, TestEra) where

import Cardano.Ledger.Core (Script, TxBody, Value)
import qualified Cardano.Ledger.Mary.Value ()
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import Data.MemoBytes (MemoBytes (Memo), roundTripMemo)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (empty)
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.TxBody (Wdrl (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Cardano.Ledger.ShelleyMA.TestEra(TestEra,TestScript)

-- ============================================================================================
-- make an example
-- ============================================================================================

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID. We use
--the tools supplied by Test.Cardano.Ledger.ShelleyMA.TestEra(TestEra,TestScript)

type instance Value TestEra = ConcreteValue.Value TestEra
type instance Script TestEra = TestScript
type instance TxBody TestEra = Mary.TxBody TestEra

-- ====================================================================================================
-- Make a TxBody to test with

eseq :: StrictSeq a
eseq = fromList []

tx :: Mary.TxBody TestEra
tx =
  Mary.TxBody
    empty
    eseq
    eseq
    (Wdrl Map.empty)
    (Coin 6)
    (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
    SNothing
    SNothing
    (inject (Coin 2))

bytes :: Mary.TxBody era -> ShortByteString
bytes (Mary.STxBody (Memo _ b)) = b

fieldTests :: TestTree
fieldTests =
  testGroup
    "getField tests"
    [ testCase "inputs" (assertEqual "inputs" (getField @"inputs" tx) empty),
      testCase "outputs" (assertEqual "outputs" (getField @"outputs" tx) eseq),
      testCase "certs" (assertEqual "certs" (getField @"certs" tx) eseq),
      testCase "wdrls" (assertEqual "wdrls" (getField @"wdrls" tx) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (getField @"txfee" tx) (Coin 6)),
      testCase "vldt" (assertEqual "vldt" (getField @"vldt" tx) (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))),
      testCase "update" (assertEqual "update" (getField @"update" tx) SNothing),
      testCase "mdHash" (assertEqual "mdHash" (getField @"mdHash" tx) SNothing),
      testCase "forge" (assertEqual "forge" (getField @"forge" tx) (inject (Coin 2)))
    ]

roundtrip :: Mary.TxBody TestEra -> Bool
roundtrip (Mary.STxBody memo) =
  case roundTripMemo memo of
    Right ("", new) -> new == memo
    _other -> False

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests,
      testCase "length" (assertEqual "length" (Short.length (bytes tx)) 19),
      testCase "roundtrip" (assertBool "rountrip" (roundtrip tx))
    ]
