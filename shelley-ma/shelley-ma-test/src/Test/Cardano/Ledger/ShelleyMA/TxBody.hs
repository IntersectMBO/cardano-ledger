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
    genShelleyBody,
    genMaryBody,
  )
where

import Cardano.Binary (ToCBOR (..))
-- Arbitrary instances
-- Arbitrary instances

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody
  ( TxBodyRaw (..),
    bodyFields,
    initial,
    txSparse,
  )
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import Data.Coders
  ( Decode (..),
    Density (..),
    Wrapped (..),
    decode,
    encode,
  )
import qualified Data.Map.Strict as Map
import Data.MemoBytes (MemoBytes (Memo), roundTripMemo)
import Data.Sequence.Strict (fromList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (empty)
import Data.String (fromString)
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Tx (hashScript)
import Shelley.Spec.Ledger.TxBody (Wdrl (..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
  ( RoundTripResult,
    embedTrip',
    roundTrip',
    roundTripAnn,
  )
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Cardano.Ledger.Constraints (UsesValue,UsesScript,UsesAuxiliary)

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

roundtrip :: Mary.TxBody TestEra -> Assertion
roundtrip (Mary.TxBodyConstr memo) =
  case roundTripMemo memo of
    Right ("", new) -> new @?= memo
    Right (extra, _new) -> error ("extra bytes: " <> show extra)
    Left s -> error (show s)

-- =====================================================================
-- Now some random property tests

checkSparse :: TxBodyRaw TestEra -> Bool
checkSparse tx = case oldStyleRoundTrip tx of
  Right ("", _) -> True
  Right (left, _) -> error ("left over input: " ++ show left)
  Left s -> error (show s)

embedTest :: Gen Bool
embedTest = do
  shelleybody <- genShelleyBody
  case embedTrip' toCBOR (decode (getTxSparse @TestEra)) shelleybody of
    Right ("", _) -> pure True
    Right (left, _) -> error ("left over input: " ++ show left)
    Left s -> error (show s)

getTxSparse ::
  (UsesValue era, UsesScript era, UsesAuxiliary era) =>
  Decode ('Closed 'Dense) (TxBodyRaw era)
getTxSparse =
  SparseKeyed
    "TxBodyRaw"
    initial
    bodyFields
    [(0, "inputs"), (1, "outputs"), (2, "txfee")]

oldStyleRoundTrip :: TxBodyRaw TestEra -> RoundTripResult (TxBodyRaw TestEra)
oldStyleRoundTrip = roundTrip' (encode . txSparse) (decode getTxSparse)

genShelleyBody :: Gen (Shelley.TxBody TestEra)
genShelleyBody =
  Shelley.TxBody
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genMaryBody :: Gen (TxBodyRaw TestEra)
genMaryBody =
  TxBodyRaw
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genMintValues

instance Arbitrary (TxBodyRaw TestEra) where
  arbitrary = genMaryBody

checkSparseAnn :: Mary.TxBody TestEra -> Bool
checkSparseAnn tx = case roundTripAnn tx of
  Right ("", _) -> True
  Right (left, _) -> error ("left over input: " ++ show left)
  Left s -> error (show s)

-- ======================================================

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests,
      testCase "length" (assertEqual "length" 36 (Short.length (bytes txM))),
      testCase "roundtrip txM" (roundtrip txM),
      testProperty "roundtrip sparse TxBodyRaw" checkSparse,
      testProperty "embed Shelley sparse TxBodyRaw" embedTest,
      testProperty "roundtrip sparse TxBody" checkSparseAnn
    ]
