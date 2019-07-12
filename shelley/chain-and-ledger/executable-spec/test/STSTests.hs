{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module STSTests (stsTests) where

import           Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Crypto.Random (drgNewTest, withDRG)
import           MockTypes

import           BaseTypes (Seed (..), mkUnitInterval)
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof, bhbHash)
import           Control.State.Transition
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (pattern KeyPair, pattern SKeyES, pattern VKeyES, sKey, sign, signKES, vKey)
import           LedgerState (pattern NewEpochState, emptyEpochState)
import           OCert (KESPeriod (..), pattern OCert)
import           Slot (Epoch (..), Slot (..))
import           STS.Updn


-- | The UPDN transition should update both the evolving nonce and
-- the candidate nonce during the first two-thirds of the epoch.
-- Note that the number of slots per epoch is hard-coded in the Slot module.
testUPNEarly :: Assertion
testUPNEarly =
  let
    st = applySTS @UPDN (TRC (Nonce 1, (Nonce 2, Nonce 3), Slot.Slot 5))
  in
    st @?= Right (SeedOp (Nonce 2) (Nonce 1), SeedOp (Nonce 3) (Nonce 1))

-- | The UPDN transition should update only the evolving nonce
-- in the last thirds of the epoch.
-- Note that the number of slots per epoch is hard-coded in the Slot module.
testUPNLate :: Assertion
testUPNLate =
  let
    st = applySTS @UPDN (TRC (Nonce 1, (Nonce 2, Nonce 3), Slot.Slot 85))
  in
    st @?= Right (SeedOp (Nonce 2) (Nonce 1), Nonce 3)

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

-- | This is a very simple test demonstrating that we have everything in place
-- in order to run the CHAIN STS transition.
-- TODO replace this test with one that does more than just apply the rule.
testApplyChain :: Assertion
testApplyChain =
  let
    initChainSt =
      ( NewEpochState
          (Epoch 0)
          (Nonce 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          Nothing
          (PoolDistr Map.empty)
          Map.empty
      , Nonce 0
      , Nonce 0
      , Nothing
      , Slot 0
      )
    kp = KeyPair 1 1
    half = fromMaybe (error "could not construct unit interval") $ mkUnitInterval 0.5
    (sKeyES, vKeyES) = mkKESKeyPair (0, 0, 0, 0, 0)
    bhb = BHBody
            Nothing
            (vKey kp)
            (Slot 0)
            (Nonce 0)
            (Proof (vKey kp) (Nonce 0))
            half
            (Proof (vKey kp) half)
            (sign (sKey kp) [])
            100
            (bhbHash [])
            (OCert
              vKeyES
              (vKey kp)
              0
              (KESPeriod 0)
              (sign (sKey kp) (vKeyES, 0, KESPeriod 0))
            )
    block = Block (BHeader bhb (Keys.signKES sKeyES bhb 0)) []
    newSt = applySTS @CHAIN (TRC (Slot 0, initChainSt, block))
  in
    isLeft newSt @?= True

stsTests :: TestTree
stsTests = testGroup "STS Tests"
  [ testCase "update nonce early in the epoch" testUPNEarly
  , testCase "update nonce late in the epoch" testUPNLate
  , testCase "apply CHAIN transition" testApplyChain
  ]
