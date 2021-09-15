{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks
  ( timelockTests,
    MultiSig,
  )
where

import Cardano.Binary (Annotator (..), FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    showTimelock,
    translate,
    pattern TimelockConstr,
  )
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Lazy as Lazy
import Data.MemoBytes (MemoBytes (Memo))
import Data.Sequence.Strict (fromList)
import Shelley.Spec.Ledger.Scripts (MultiSig, getMultiSigBytes)
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (embedTripAnn, roundTripAnn)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- ================================================================

s1 :: Timelock TestCrypto
s1 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire 18])

s2 :: Timelock TestCrypto
s2 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire (SlotNo 23)])

s4 :: Timelock TestCrypto
s4 = RequireAllOf (fromList [s1, s2])

-- ================================================================

checkOne :: (FromCBOR (Annotator t), ToCBOR t, Show t) => String -> t -> TestTree
checkOne nm t = testProperty ("RoundTrip: " ++ nm) $
  case roundTripAnn t of
    Right _ -> True
    Left s -> error ("Fail to roundtrip " ++ show t ++ " with error " ++ show s)

checkAnn :: Timelock TestCrypto -> Bool
checkAnn t =
  case roundTripAnn t of
    Right _ -> True
    Left s -> error (show s)

checkEmbed :: MultiSig TestCrypto -> Bool
checkEmbed multi =
  case embedTripAnn @(Timelock TestCrypto) multi of
    Right (left, _) | left == Lazy.empty -> True
    Right (left, _) -> error ("Bytes left over: " ++ show left)
    Left s -> error (show s)

checkTranslate :: MultiSig TestCrypto -> Bool
checkTranslate multi = bytes == bytes2
  where
    bytes = getMultiSigBytes multi
    (TimelockConstr (Memo _ bytes2)) = translate @TestCrypto multi

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
