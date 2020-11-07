{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-} -- Arbitrary instances
-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody
  ( txBodyTest,
    TestEra,
    genShelleyBody,
    genMaryBody,
    genMaryTxBody,
    oldStyleRoundTrip,
  ) where

import Cardano.Binary(ToCBOR(..))
import Cardano.Ledger.Core (Value)
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
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Test.Tasty
import Test.Tasty.HUnit
import Test.Cardano.Ledger.ShelleyMA.TestEra(TestCrypto)
import Cardano.Ledger.ShelleyMA.TxBody
  ( TxBodyRaw(..),
    FamsFrom,
    txSparse,
    bodyFields,
    initial,
   )
import Data.Coders
  ( Wrapped(..),
    Density(..),
    encode,
    Decode(..),
    decode,
  )
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators() -- Arbitrary instances
import Test.Shelley.Spec.Ledger.Serialisation.Generators() -- Arbitrary instances
import Test.Tasty.QuickCheck
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
  ( roundTrip',
    embedTrip',
    roundTripAnn,
    RoundTripResult
  )
import Cardano.Ledger.Mary (MaryEra)
-- ============================================================================================
-- make an example
-- ============================================================================================

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID. We use
-- TestCrypto from Test.Cardano.Ledger.ShelleyMA.TestEra(TestCrypto)

type TestEra = MaryEra TestCrypto

-- ====================================================================================================
-- Make a TxBody to test with

eseq :: StrictSeq a
eseq = fromList []

txM :: Mary.TxBody TestEra
txM =
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
    [ testCase "inputs" (assertEqual "inputs" (getField @"inputs" txM) empty),
      testCase "outputs" (assertEqual "outputs" (getField @"outputs" txM) eseq),
      testCase "certs" (assertEqual "certs" (getField @"certs" txM) eseq),
      testCase "wdrls" (assertEqual "wdrls" (getField @"wdrls" txM) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (getField @"txfee" txM) (Coin 6)),
      testCase "vldt" (assertEqual "vldt" (getField @"vldt" txM) (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))),
      testCase "update" (assertEqual "update" (getField @"update" txM) SNothing),
      testCase "mdHash" (assertEqual "mdHash" (getField @"mdHash" txM) SNothing),
      testCase "forge" (assertEqual "forge" (getField @"forge" txM) (inject (Coin 2)))
    ]

roundtrip :: Mary.TxBody TestEra -> Bool
roundtrip (Mary.STxBody memo) =
  case roundTripMemo memo of
    Right ("", new) -> new == memo
    _other -> False

-- =====================================================================
-- Now some random property tests

checkSparse :: TxBodyRaw TestEra -> Bool
checkSparse tx = case oldStyleRoundTrip tx of
    Right("",_) -> True
    Right(left,_) -> error ("left over input: "++show left)
    Left s -> error (show s)

embedTest :: Gen Bool
embedTest = do
  shelleybody <- genShelleyBody
  case embedTrip' toCBOR (decode (getTxSparse @TestEra)) shelleybody of
     Right("",_) -> pure True
     Right(left,_) -> error ("left over input: "++show left)
     Left s -> error (show s)

getTxSparse ::  (Val (Value era),FamsFrom era) => Decode ('Closed 'Dense) (TxBodyRaw era)
getTxSparse = SparseKeyed "TxBodyRaw" initial bodyFields [(0,"inputs"),(1,"outputs"),(2,"txfee")]

oldStyleRoundTrip:: TxBodyRaw TestEra -> RoundTripResult (TxBodyRaw TestEra)
oldStyleRoundTrip x = roundTrip' (encode . txSparse) (decode getTxSparse) x

genShelleyBody :: Gen (Shelley.TxBody TestEra)
genShelleyBody = Shelley.TxBody <$> arbitrary <*> pure eseq <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genMaryBody :: Gen (TxBodyRaw TestEra)
genMaryBody = TxBodyRaw <$> arbitrary <*> pure eseq <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TxBodyRaw TestEra) where
  arbitrary = genMaryBody

genMaryTxBody :: Gen (Mary.TxBody TestEra)
genMaryTxBody = Mary.TxBody <$> arbitrary <*> pure eseq <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

checkSparseAnn :: Mary.TxBody TestEra -> Bool
checkSparseAnn tx = case roundTripAnn tx of
    Right("",_) -> True
    Right(left,_) -> error ("left over input: "++show left)
    Left s -> error (show s)

-- ======================================================

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests
    , testCase "length" (assertEqual "length" 16 (Short.length (bytes txM)))
    , testCase "roundtrip txM" (assertBool "rountrip" (roundtrip txM))
    , testProperty "roundtrip sparse TxBodyRaw" checkSparse
    , testProperty "embed Shelley sparse TxBodyRaw" embedTest
    , testProperty "routrip sparse TxBody" checkSparseAnn
    ]
