{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import qualified Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Pretty (PrettyA (..), ppList)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
    updateStakeDistribution,
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Trans.RWS.Strict (gets)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import qualified Data.Map as Map
import Debug.Trace
import Test.Cardano.Ledger.Generic.Fields (abstractTx, abstractTxBody, abstractTxOut, abstractWitnesses)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (..), isValid')
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenSize (startSlot),
    GenState (..),
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockChainState (..))
import Test.Cardano.Ledger.Generic.ModelState
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyC (..), pcLedgerState, pcTx, txSummary)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Trace (Gen1, traceProp)
import Test.Cardano.Ledger.Generic.TxGen
  ( Box (..),
    applySTSByProof,
    assembleWits,
    coreTx,
    coreTxBody,
    coreTxOut,
    genUTxO,
    genValidatedTx,
  )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- =====================================
-- Top level generators of TRC

genTxAndUTXOState :: Reflect era => Proof era -> GenSize -> Gen (TRC (Core.EraRule "UTXOW" era), GenState era)
genTxAndUTXOState proof@(Babbage _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Alonzo _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Mary _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Allegra _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Shelley _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)

genTxAndLEDGERState ::
  forall era.
  ( Reflect era,
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era
  ) =>
  Proof era ->
  GenSize ->
  Gen (Box era)
genTxAndLEDGERState proof sizes = do
  let slotNo = SlotNo (startSlot sizes)
  txIx <- arbitrary
  let genT = do
        (initial, _) <- genUTxO -- Generate a random UTxO, so mUTxO is not empty
        modifyModel (\m -> m {mUTxO = initial})
        (_utxo, tx) <- genValidatedTx proof
        model <- gets gsModel
        pp <- gets (gePParams . gsGenEnv)
        let ledgerState = extract @(LedgerState era) model
            ledgerEnv = LedgerEnv slotNo txIx pp (AccountState (Coin 0) (Coin 0))
        pure $ TRC (ledgerEnv, ledgerState, tx)
  (trc, genstate) <- runGenRS proof def genT
  pure (Box proof trc genstate)

-- =============================================
-- Now a test

testTxValidForLEDGER ::
  ( Reflect era,
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    PrettyA (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Box era ->
  Property
testTxValidForLEDGER proof (Box _ (trc@(TRC (_, ledgerState, vtx))) _genstate) =
  ( if False
      then trace (show (txSummary proof vtx))
      else id
  )
    $ case applySTSByProof proof trc of -- trc encodes the initial (generated) state, vtx is the transaction
      Right ledgerState' ->
        -- UTxOState and DPState after applying the transaction $$$
        classify (coerce (isValid' proof vtx)) "TxValid" $
          totalAda ledgerState' === totalAda ledgerState
      Left errs ->
        counterexample
          ( show (pcLedgerState proof ledgerState) ++ "\n\n"
              ++ show (pcTx proof vtx)
              ++ "\n\n"
              ++ show (ppList prettyA errs)
          )
          (property False)

-- =============================================
-- Make some property tests

-- =========================================================================
-- The generic types make a roundtrip without adding or losing information

txOutRoundTrip ::
  (Eq (Core.TxOut era), Era era) => Proof era -> Core.TxOut era -> Bool
txOutRoundTrip proof x = coreTxOut proof (abstractTxOut proof x) == x

txRoundTrip ::
  Eq (Core.Tx era) => Proof era -> Core.Tx era -> Bool
txRoundTrip proof x = coreTx proof (abstractTx proof x) == x

txBodyRoundTrip ::
  (Eq (Core.TxBody era), Era era) => Proof era -> Core.TxBody era -> Bool
txBodyRoundTrip proof x = coreTxBody proof (abstractTxBody proof x) == x

txWitRoundTrip ::
  (Eq (Core.Witnesses era), Era era) => Proof era -> Core.Witnesses era -> Bool
txWitRoundTrip proof x = assembleWits proof (abstractWitnesses proof x) == x

coreTypesRoundTrip :: TestTree
coreTypesRoundTrip =
  testGroup
    "Core types make generic roundtrips"
    [ testGroup
        "Witnesses roundtrip"
        [ testProperty "Babbage era" $ txWitRoundTrip (Babbage Mock),
          testProperty "Alonzo era" $ txWitRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txWitRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txWitRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txWitRoundTrip (Shelley Mock)
        ],
      testGroup
        "TxBody roundtrips"
        [ testProperty "Babbage era" $ txBodyRoundTrip (Babbage Mock),
          testProperty "Alonzo era" $ txBodyRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txBodyRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txBodyRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txBodyRoundTrip (Shelley Mock)
        ],
      testGroup
        "TxOut roundtrips"
        [ testProperty "Babbage era" $ txOutRoundTrip (Babbage Mock),
          testProperty "Alonzo era" $ txOutRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txOutRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txOutRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txOutRoundTrip (Shelley Mock)
        ],
      testGroup
        "Tx roundtrips"
        [ testProperty "Babbage era" $ txRoundTrip (Babbage Mock),
          testProperty "Alonzo era" $ txRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txRoundTrip (Shelley Mock)
        ]
    ]

-- | A single Tx preserves Ada
txPreserveAda :: TestTree
txPreserveAda =
  testGroup
    "Individual Tx's preserve Ada"
    [ testProperty "Shelley Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Shelley Mock) def) (testTxValidForLEDGER (Shelley Mock)),
      testProperty "Allegra Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Allegra Mock) def) (testTxValidForLEDGER (Allegra Mock)),
      testProperty "Mary Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Mary Mock) def) (testTxValidForLEDGER (Mary Mock)),
      testProperty "Alonzo ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState (Alonzo Mock) def) (testTxValidForLEDGER (Alonzo Mock)),
      testProperty "Babbage ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState (Babbage Mock) def) (testTxValidForLEDGER (Babbage Mock))
    ]

-- | Ada is preserved over a trace of length 100
adaIsPreserved :: (Reflect era, HasTrace (MOCKCHAIN era) (Gen1 era)) => Proof era -> TestTree
adaIsPreserved proof =
  testProperty ("In era (" ++ show proof ++ "). Trace length = 100") $
    withMaxSuccess 30 $
      traceProp proof 100 def (\firstSt lastSt -> totalAda firstSt === totalAda lastSt)

tracePreserveAda :: TestTree
tracePreserveAda =
  testGroup
    "Total Ada is preserved over traces of length 100"
    [ adaIsPreserved (Babbage Mock),
      adaIsPreserved (Alonzo Mock),
      adaIsPreserved (Mary Mock),
      adaIsPreserved (Allegra Mock),
      adaIsPreserved (Shelley Mock)
    ]

-- | The incremental Stake invaraint is preserved over a trace of length 100
stakeInvariant :: Era era => MockChainState era -> MockChainState era -> Property
stakeInvariant (MockChainState _ _ _) (MockChainState nes _ _) =
  case (lsUTxOState . esLState . nesEs) nes of
    (UTxOState utxo _ _ _ istake) -> istake === updateStakeDistribution mempty mempty utxo

incrementStakeInvariant :: (Reflect era, HasTrace (MOCKCHAIN era) (Gen1 era)) => Proof era -> TestTree
incrementStakeInvariant proof =
  testProperty ("In era (" ++ show proof ++ "). Trace length = 100") $
    withMaxSuccess 30 $
      traceProp proof 100 def stakeInvariant

incrementalStake :: TestTree
incrementalStake =
  testGroup
    "Incremental Stake invariant holds"
    [ incrementStakeInvariant (Babbage Mock),
      incrementStakeInvariant (Alonzo Mock),
      incrementStakeInvariant (Mary Mock),
      incrementStakeInvariant (Allegra Mock),
      incrementStakeInvariant (Shelley Mock)
    ]

genericProperties :: TestTree
genericProperties =
  testGroup
    "Generic Property tests"
    [ coreTypesRoundTrip,
      txPreserveAda,
      tracePreserveAda,
      incrementalStake
    ]

-- ==============================================================
-- Infrastrucure for running individual tests, with easy replay.
-- In ghci just type
-- :main --quickcheck-replay=205148

main :: IO ()
main = test 100 (Babbage Mock)

test :: ReflectC (Crypto era) => Int -> Proof era -> IO ()
test n proof = defaultMain $
  case proof of
    Babbage _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof)))
    Alonzo _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof)))
    Shelley _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof)))
    other -> error ("NO Test in era " ++ show other)

-- ===============================================================
-- Tools for generating other things from a GenEnv. This way one can
-- test individual functions in this file. These are intended for use
-- when developng and debugging.

-- | Construct a random (Gen b)
makeGen :: Reflect era => Proof era -> (Proof era -> GenRS era b) -> Gen b
makeGen proof computeWith = fst <$> runGenRS proof def (computeWith proof)

runTest :: (Reflect era, PrettyC a era) => (Proof era -> GenRS era a) -> (a -> IO ()) -> Proof era -> IO ()
runTest computeWith action proof = do
  ans <- generate (makeGen proof computeWith)
  putStrLn (show (prettyC proof ans))
  action ans

main2 :: IO ()
main2 = runTest (\x -> fst <$> genValidatedTx x) (const (pure ())) (Alonzo Mock)

main3 :: IO ()
main3 = runTest (\_x -> (fromMUtxo . fst) <$> genUTxO) action (Alonzo Mock)
  where
    action (UTxO x) = putStrLn ("Size = " ++ show (Map.size x))
