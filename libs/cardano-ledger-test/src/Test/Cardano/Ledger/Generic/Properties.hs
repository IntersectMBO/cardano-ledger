{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import qualified Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
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
import qualified Cardano.Ledger.Shelley.PParams as Shelley (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLedgerEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Trans.RWS.Strict (gets)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (Trace (..), lastState)
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Generic.Fields (abstractTx, abstractTxBody, abstractTxOut, abstractWitnesses)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (totalAda), isValid')
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenSize (..),
    GenState (..),
    blocksizeMax,
    initStableFields,
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockChainState (..))
import Test.Cardano.Ledger.Generic.ModelState
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyC (..), pcLedgerState, pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Trace (Gen1, forEachEpochTrace, testTraces, traceProp)
import Test.Cardano.Ledger.Generic.TxGen
  ( Box (..),
    applySTSByProof,
    assembleWits,
    coreTx,
    coreTxBody,
    coreTxOut,
    genAlonzoTx,
    genUTxO,
  )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- =====================================
-- Top level generators of TRC

genTxAndUTXOState :: Reflect era => Proof era -> GenSize -> Gen (TRC (EraRule "UTXOW" era), GenState era)
genTxAndUTXOState proof@(Conway _) gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
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
    Signal (EraRule "LEDGER" era) ~ Tx era,
    State (EraRule "LEDGER" era) ~ LedgerState era,
    Environment (EraRule "LEDGER" era) ~ ShelleyLedgerEnv era
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
        (_utxo, tx) <- genAlonzoTx proof slotNo
        model <- gets gsModel
        pp <- gets (gePParams . gsGenEnv)
        let ledgerState = extract @(LedgerState era) model
            ledgerEnv = LedgerEnv slotNo txIx pp (AccountState (Coin 0) (Coin 0))
        pure $ TRC (ledgerEnv, ledgerState, tx)
  (trc, genstate) <- runGenRS proof sizes (initStableFields proof >> genT)
  pure (Box proof trc genstate)

-- =============================================
-- Now a test

testTxValidForLEDGER ::
  ( Reflect era,
    Signal (EraRule "LEDGER" era) ~ Tx era,
    State (EraRule "LEDGER" era) ~ LedgerState era,
    PrettyA (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Box era ->
  Property
testTxValidForLEDGER proof (Box _ trc@(TRC (_, ledgerState, vtx)) _genstate) =
  -- trc encodes the initial (generated) state, vtx is the transaction
  case applySTSByProof proof trc of
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
  EraTxOut era => Proof era -> TxOut era -> Bool
txOutRoundTrip proof x = coreTxOut proof (abstractTxOut proof x) == x

txRoundTrip ::
  EraTx era => Proof era -> Tx era -> Bool
txRoundTrip proof x = coreTx proof (abstractTx proof x) == x

txBodyRoundTrip ::
  EraTxBody era => Proof era -> TxBody era -> Bool
txBodyRoundTrip proof x = coreTxBody proof (abstractTxBody proof x) == x

txWitRoundTrip ::
  EraTxWits era => Proof era -> TxWits era -> Bool
txWitRoundTrip proof x = assembleWits proof (abstractWitnesses proof x) == x

coreTypesRoundTrip :: TestTree
coreTypesRoundTrip =
  testGroup
    "Core types make generic roundtrips"
    [ testGroup
        "TxWits roundtrip"
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
txPreserveAda :: GenSize -> TestTree
txPreserveAda genSize =
  testGroup
    "Individual Tx's preserve Ada"
    [ testProperty "Shelley Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Shelley Mock) genSize) (testTxValidForLEDGER (Shelley Mock)),
      testProperty "Allegra Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Allegra Mock) genSize) (testTxValidForLEDGER (Allegra Mock)),
      testProperty "Mary Tx preserves ADA" $
        forAll (genTxAndLEDGERState (Mary Mock) genSize) (testTxValidForLEDGER (Mary Mock)),
      testProperty "Alonzo ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState (Alonzo Mock) genSize) (testTxValidForLEDGER (Alonzo Mock)),
      testProperty "Babbage ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState (Babbage Mock) genSize) (testTxValidForLEDGER (Babbage Mock)),
      testProperty "Conway ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState (Conway Mock) genSize) (testTxValidForLEDGER (Conway Mock))
    ]

-- | Ada is preserved over a trace of length 100
adaIsPreserved ::
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
adaIsPreserved proof numTx gensize =
  testProperty (show proof ++ " era. Trace length = " ++ show numTx) $
    traceProp
      proof
      numTx
      gensize
      (\firstSt lastSt -> totalAda (mcsNes firstSt) === totalAda (mcsNes lastSt))

tracePreserveAda :: Int -> GenSize -> TestTree
tracePreserveAda numTx gensize =
  testGroup
    ("Total Ada is preserved over traces of length " ++ show numTx)
    [ adaIsPreservedBabbage numTx gensize,
      adaIsPreserved (Alonzo Mock) numTx gensize,
      adaIsPreserved (Mary Mock) numTx gensize,
      adaIsPreserved (Allegra Mock) numTx gensize,
      adaIsPreserved (Shelley Mock) numTx gensize
    ]

adaIsPreservedBabbage :: Int -> GenSize -> TestTree
adaIsPreservedBabbage numTx gensize = adaIsPreserved (Babbage Mock) numTx gensize

-- | The incremental Stake invaraint is preserved over a trace of length 100=
stakeInvariant :: EraTxOut era => MockChainState era -> MockChainState era -> Property
stakeInvariant (MockChainState _ _ _) (MockChainState nes _ _) =
  case (lsUTxOState . esLState . nesEs) nes of
    (UTxOState utxo _ _ _ istake) -> istake === updateStakeDistribution mempty mempty utxo

incrementStakeInvariant :: (Reflect era, HasTrace (MOCKCHAIN era) (Gen1 era)) => Proof era -> GenSize -> TestTree
incrementStakeInvariant proof gensize =
  testProperty (show proof ++ " era. Trace length = 100") $
    traceProp proof 100 gensize stakeInvariant

incrementalStake :: GenSize -> TestTree
incrementalStake genSize =
  testGroup
    "Incremental Stake invariant holds"
    [ incrementStakeInvariant (Conway Mock) genSize,
      incrementStakeInvariant (Babbage Mock) genSize,
      incrementStakeInvariant (Alonzo Mock) genSize,
      incrementStakeInvariant (Mary Mock) genSize,
      incrementStakeInvariant (Allegra Mock) genSize,
      incrementStakeInvariant (Shelley Mock) genSize
    ]

genericProperties :: GenSize -> TestTree
genericProperties genSize =
  testGroup
    "Generic Property tests"
    [ coreTypesRoundTrip,
      txPreserveAda genSize,
      tracePreserveAda 45 genSize,
      incrementalStake genSize,
      testTraces 45,
      epochPreserveAda genSize
    ]

epochPreserveAda :: GenSize -> TestTree
epochPreserveAda genSize =
  testGroup
    "Ada is preserved in each epoch"
    [ adaIsPreservedInEachEpoch (Babbage Mock) genSize,
      adaIsPreservedInEachEpoch (Alonzo Mock) genSize,
      adaIsPreservedInEachEpoch (Mary Mock) genSize,
      adaIsPreservedInEachEpoch (Allegra Mock) genSize,
      adaIsPreservedInEachEpoch (Shelley Mock) genSize
    ]

adaIsPreservedInEachEpoch ::
  forall era.
  ( Reflect era
  ) =>
  Proof era ->
  GenSize ->
  TestTree
adaIsPreservedInEachEpoch proof genSize =
  testProperty (show proof) $
    forEachEpochTrace proof 200 genSize withTrace
  where
    withTrace :: Trace (MOCKCHAIN era) -> Property
    withTrace trc = totalAda (mcsNes trcInit) === totalAda (mcsNes trcLast)
      where
        trcInit = _traceInitState trc
        trcLast = lastState trc

-- ==============================================================
-- Infrastrucure for running individual tests, with easy replay.
-- In ghci just type
-- :main --quickcheck-replay=205148

main :: IO ()
main = defaultMain $ adaIsPreservedBabbage 100 (def {blocksizeMax = 4})

main8 :: IO ()
main8 = test 100 (Babbage Mock)

test :: ReflectC (Crypto era) => Int -> Proof era -> IO ()
test n proof = defaultMain $
  case proof of
    Conway _ ->
      testProperty "Conway ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Babbage _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Alonzo _ ->
      testProperty "Alonzo ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Shelley _ ->
      testProperty "Shelley ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    other -> error ("NO Test in era " ++ show other)

-- ===============================================================
-- Tools for generating other things from a GenEnv. This way one can
-- test individual functions in this file. These are intended for use
-- when developng and debugging.

-- | Construct a random (Gen b)
makeGen :: Reflect era => Proof era -> (Proof era -> GenRS era b) -> Gen b
makeGen proof computeWith = fst <$> runGenRS proof def (computeWith proof)

runTest ::
  (Reflect era, PrettyC a era) =>
  (Proof era -> GenRS era a) ->
  (a -> IO ()) ->
  Proof era ->
  IO ()
runTest computeWith action proof = do
  ans <- generate (makeGen proof computeWith)
  print (prettyC proof ans)
  action ans

main2 :: IO ()
main2 = runTest (\x -> fst <$> genAlonzoTx x (SlotNo 0)) (const (pure ())) (Babbage Mock)

main3 :: IO ()
main3 = runTest (\_x -> UTxO . fst <$> genUTxO) action (Alonzo Mock)
  where
    action (UTxO x) = putStrLn ("Size = " ++ show (Map.size x))
