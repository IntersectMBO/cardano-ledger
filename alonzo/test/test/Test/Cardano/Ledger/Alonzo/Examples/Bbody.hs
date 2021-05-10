{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples.Bbody
  ( bbodyExamples,
  )
where

import Cardano.Crypto.VRF (evalCertified)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBBODY)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..), hashTxSeq)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Coerce (coerce)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Shelley.Spec.Ledger.API
  ( BHBody (..),
    BHeader (..),
    Block (..),
    DPState (..),
    DState (..),
    KESPeriod (..),
    LedgerState (..),
    Nonce (NeutralNonce),
    OCert (..),
    PrevHash (GenesisHash),
    ProtVer (..),
    UTxO (..),
  )
import Shelley.Spec.Ledger.BlockChain (bBodySize, mkSeed, seedEta, seedL)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Keys (KeyPair (..), KeyRole (..), coerceKeyRole, hashKey, signedDSIGN, signedKES)
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.OCert (OCertSignable (..))
import Shelley.Spec.Ledger.STS.Bbody (BbodyEnv (..), BbodyState (..))
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.TxBody (TxIn (..))
import Shelley.Spec.Ledger.UTxO (txid)
import qualified Test.Cardano.Ledger.Alonzo.Examples.Utxow as UTXOW
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Shelley.Spec.Ledger.Utils
  ( applySTSTest,
    mkKESKeyPair,
    mkKeyPair,
    mkVRFKeyPair,
    runShelleyBase,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

type A = AlonzoEra C_Crypto

-- =======================
-- Setup the initial state
-- =======================

bbodyEnv :: BbodyEnv A
bbodyEnv = BbodyEnv UTXOW.pp def

-- =======
--  Tests
-- =======

dpstate :: DPState C_Crypto
dpstate = def {_dstate = def {_rewards = Map.singleton UTXOW.scriptStakeCredSuceed (Coin 1000)}}

initialBBodyState :: BbodyState A
initialBBodyState = BbodyState (LedgerState UTXOW.initialUtxoSt dpstate) (BlocksMade mempty)

coldKeys :: KeyPair 'BlockIssuer C_Crypto
coldKeys = KeyPair skCold vkCold
  where
    (vkCold, skCold) = mkKeyPair @C_Crypto (0, 0, 0, 0, 1)

makeNaiveBlock :: [ValidatedTx A] -> Block A
makeNaiveBlock txs = Block (BHeader bhb sig) txs'
  where
    bhb =
      BHBody
        { bheaderBlockNo = BlockNo 0,
          bheaderSlotNo = SlotNo 0,
          bheaderPrev = GenesisHash,
          bheaderVk = vKey coldKeys,
          bheaderVrfVk = vvrf,
          bheaderEta = coerce $ evalCertified () nonceNonce svrf,
          bheaderL = coerce $ evalCertified () leaderNonce svrf,
          bsize = fromIntegral $ bBodySize txs',
          bhash = hashTxSeq txs',
          bheaderOCert =
            OCert
              vkes
              0
              (KESPeriod 0)
              (signedDSIGN @C_Crypto (sKey coldKeys) (OCertSignable vkes 0 (KESPeriod 0))),
          bprotver = ProtVer 5 0
        }
    sig = signedKES () 0 bhb skes
    nonceNonce = mkSeed seedEta (SlotNo 0) NeutralNonce
    leaderNonce = mkSeed seedL (SlotNo 0) NeutralNonce
    txs' = TxSeq . StrictSeq.fromList $ txs
    (svrf, vvrf) = mkVRFKeyPair @(VRF C_Crypto) (0, 0, 0, 0, 2)
    (skes, vkes) = mkKESKeyPair @(KES C_Crypto) (0, 0, 0, 0, 3)

testBlock :: Block A
testBlock =
  makeNaiveBlock
    [ UTXOW.validatingTx,
      UTXOW.notValidatingTx,
      UTXOW.validatingTxWithWithdrawal,
      UTXOW.notValidatingTxWithWithdrawal,
      UTXOW.validatingTxWithCert,
      UTXOW.notValidatingTxWithCert,
      UTXOW.validatingTxWithMint,
      UTXOW.notValidatingTxWithMint
    ]

example1UTxO :: UTxO A
example1UTxO =
  UTxO $
    Map.fromList
      [ (TxIn (txid @A UTXOW.validatingBody) 0, UTXOW.outEx1),
        (TxIn (txid @A UTXOW.validatingBodyWithCert) 0, UTXOW.outEx3),
        (TxIn (txid @A UTXOW.validatingBodyWithWithdrawal) 0, UTXOW.outEx5),
        (TxIn (txid @A UTXOW.validatingBodyWithMint) 0, UTXOW.outEx7),
        (TxIn genesisId 11, UTXOW.collateralOutput),
        (TxIn genesisId 2, UTXOW.alwaysFailsOutput),
        (TxIn genesisId 13, UTXOW.collateralOutput),
        (TxIn genesisId 4, UTXOW.someOutput),
        (TxIn genesisId 15, UTXOW.collateralOutput),
        (TxIn genesisId 6, UTXOW.someOutput),
        (TxIn genesisId 17, UTXOW.collateralOutput),
        (TxIn genesisId 8, UTXOW.someOutput)
      ]

example1UtxoSt :: UTxOState A
example1UtxoSt = UTxOState example1UTxO (Coin 0) (Coin 4020) def

example1BBodyState :: BbodyState A
example1BBodyState =
  BbodyState (LedgerState example1UtxoSt def) (BlocksMade $ Map.singleton poolID 1)
  where
    poolID = hashKey . vKey . coerceKeyRole $ coldKeys

testBBODY ::
  BbodyState A ->
  Block A ->
  Either [[PredicateFailure (AlonzoBBODY A)]] (BbodyState A) ->
  Assertion
testBBODY initialSt block (Right expectedSt) =
  checkTrace @(AlonzoBBODY A) runShelleyBase bbodyEnv $
    pure initialSt .- block .-> expectedSt
testBBODY initialSt block predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(AlonzoBBODY A) (TRC (bbodyEnv, initialSt, block))
  st @?= predicateFailure

bbodyExamples :: TestTree
bbodyExamples =
  testGroup
    "bbody examples"
    [ testCase "eight plutus scripts cases" $
        testBBODY initialBBodyState testBlock (Right example1BBodyState)
    ]
