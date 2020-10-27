{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest, TestEra) where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import Cardano.Crypto.KES
import Cardano.Crypto.VRF.Praos
import Cardano.Ledger.Core (Script, TxBody, Value)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
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

-- ============================================================================================
-- make an example
-- ============================================================================================

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID.

data TestCrypto

instance CryptoClass.Crypto TestCrypto where
  type DSIGN TestCrypto = Ed25519DSIGN
  type KES TestCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF TestCrypto = PraosVRF
  type HASH TestCrypto = Blake2b_256
  type ADDRHASH TestCrypto = Blake2b_224

-- Now make a new Era instance. We needtochoose type family instances for Value, Script, and TxBody

data TestEra

instance Era TestEra where
  type Crypto TestEra = TestCrypto

type instance Value TestEra = ConcreteValue.Value TestEra

type instance Script TestEra = TestScript

type instance TxBody TestEra = Mary.TxBody TestEra

data TestScript = TestScript

instance FromCBOR TestScript where fromCBOR = pure TestScript

instance ToCBOR TestScript where toCBOR TestScript = mempty

instance FromCBOR (Annotator TestScript) where fromCBOR = pure <$> fromCBOR

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
      testCase "dcerts" (assertEqual "dcerts" (getField @"certs" tx) eseq),
      testCase "wdrls" (assertEqual "wdrls" (getField @"wdrls" tx) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (getField @"txfee" tx) (Coin 6)),
      testCase "vldt" (assertEqual "vldt" (getField @"vldt" tx) (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))),
      testCase "txupdate" (assertEqual "update" (getField @"update" tx) SNothing),
      testCase "mdhash" (assertEqual "mdhash" (getField @"mdHash" tx) SNothing),
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
