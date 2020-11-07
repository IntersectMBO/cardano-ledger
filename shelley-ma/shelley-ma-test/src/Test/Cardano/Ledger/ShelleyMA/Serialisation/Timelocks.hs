{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks
  ( timelockTests,
    MultiSig,
  )
where

import Cardano.Binary (Annotator (..), FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Era (PreviousEra)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    showTimelock,
    translate,
    pattern Timelock,
  )
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Lazy as Lazy
import Data.MemoBytes (MemoBytes (Memo))
import Data.Sequence.Strict (fromList)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Shelley.Spec.Ledger.Scripts (MultiSig, getMultiSigBytes)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (embedTripAnn, roundTripAnn)
import Test.Cardano.Ledger.ShelleyMA.TxBody (TestEra)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- ================================================================

s1 :: Timelock TestEra
s1 = RequireAllOf (fromList [RequireTimeStart (SJust (SlotNo 12)), RequireTimeExpire SNothing])

s2 :: Timelock TestEra
s2 = RequireAllOf (fromList [RequireTimeStart (SJust (SlotNo 12)), RequireTimeExpire (SJust (SlotNo 23))])

s4 :: Timelock TestEra
s4 = RequireAllOf (fromList [s1, s2])

-- ================================================================

checkOne :: (FromCBOR (Annotator t), ToCBOR t, Show t) => String -> t -> TestTree
checkOne nm t = testProperty ("RoundTrip: " ++ nm) $
  case roundTripAnn t of
    Right _ -> True
    Left s -> error ("Fail to roundtrip " ++ show t ++ " with error " ++ show s)

checkAnn :: Timelock TestEra -> Bool
checkAnn t =
  case roundTripAnn t of
    Right _ -> True
    Left s -> error (show s)

checkEmbed :: MultiSig TestEra -> Bool
checkEmbed multi =
  case embedTripAnn @(Timelock TestEra) multi of
    Right (left, _) | left == Lazy.empty -> True
    Right (left, _) -> error ("Bytes left over: " ++ show left)
    Left s -> error (show s)

-- The translate tests depend upon translating from a previous era
-- to the current era. We arbitrarily set the TestEra to be its own
-- PreviousEra. TestEra is only used in Serialisations tests, so
-- this should not have any wider effect.

type instance PreviousEra (TestEra) = TestEra

checkTranslate :: MultiSig TestEra -> Bool
checkTranslate multi = bytes == bytes2
  where
    bytes = getMultiSigBytes multi
    (Timelock (Memo _ bytes2)) = translate @TestEra multi

timelockTests :: TestTree
timelockTests =
  testGroup
    "Timelock tests"
    [ checkOne ("s1 " ++ showTimelock s1) s1,
      checkOne ("s2 " ++ showTimelock s2) s2,
      checkOne ("s4 " ++ showTimelock s4) s4,
      testProperty "roundtripTimelock" checkAnn,
      testProperty "MultiSig deserialises as Timelock" checkEmbed,
      testProperty "Translate preserves bytes" checkTranslate
    ]
