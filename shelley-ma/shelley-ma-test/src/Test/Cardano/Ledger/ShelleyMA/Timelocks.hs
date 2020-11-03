{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Ledger.ShelleyMA.Timelocks
  ( timelockTests,
    testT,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    FullByteString (Full),
    ToCBOR (toCBOR),
  )
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Sequence.Strict (fromList)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Test.Cardano.Ledger.ShelleyMA.TxBody (TestEra)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)




-- ================================================================

s1 :: Timelock TestEra
s1 = Interval (ValidityInterval (SJust (SlotNo 12)) SNothing)

s2 :: Timelock TestEra
s2 = Interval (ValidityInterval (SJust (SlotNo 12)) (SJust (SlotNo 23)))

s4 :: Timelock TestEra
s4 = TimelockAnd (fromList [s1, s2])

-- ================================================================

testT :: (ToCBOR t, FromCBOR t) => t -> Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testT s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

testAnn :: (ToCBOR t, FromCBOR (Annotator t)) => t -> Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testAnn s =
  let bytes = (toLazyByteString (toCBOR s))
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> Left err
        Right (leftover, Annotator f) -> Right (leftover, f (Full bytes))

annTest :: (FromCBOR (Annotator t), ToCBOR t, Show t) => String -> t -> TestTree
annTest nm t = testProperty ("RoundTrip: " ++ nm) $
  case testAnn t of
    Right _ -> True
    Left s -> error ("Fail to roundtrip " ++ show t ++ " with error " ++ show s)

timelocktests :: TestTree
timelocktests =
  testGroup
    "Timelock tests"
    [ annTest ("s1 " ++ showTimelock s1) s1,
      annTest ("s2 " ++ showTimelock s2) s2,
      annTest ("s4 " ++ showTimelock s4) s4
    ]

timelockTests :: TestTree
timelockTests =
  testGroup
    "MemoBytesTest"
    [ timelocktests
    ]
