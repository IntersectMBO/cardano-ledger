{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo.Tx (AlonzoTxBody (..), IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  updateStakeDistribution,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), UtxoEnv (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Trans.RWS.Strict (gets)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle, twiddleInvariantProp)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Generic.Fields (
  abstractTx,
  abstractTxBody,
  abstractTxOut,
  abstractWitnesses,
 )
import Test.Cardano.Ledger.Generic.Functions (TotalAda (totalAda), isValid')
import Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
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
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..), pcLedgerState, pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Trace (
  Gen1,
  forEachEpochTrace,
  testPropMax,
  testTraces,
  traceProp,
 )
import Test.Cardano.Ledger.Generic.TxGen (
  Box (..),
  applySTSByProof,
  assembleWits,
  coreTx,
  coreTxBody,
  coreTxOut,
  genAlonzoTx,
  genUTxO,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Control.State.Transition.Trace (Trace (..), lastState)
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)

-- =====================================
-- Top level generators of TRC

genTxAndUTXOState ::
  Reflect era => Proof era -> GenSize -> Gen (TRC (EraRule "UTXOW" era), GenState era)
genTxAndUTXOState proof@Conway gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@Babbage gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@Alonzo gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@Mary gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@Allegra gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@Shelley gsize = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <-
    genTxAndLEDGERState proof gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)

genTxAndLEDGERState ::
  forall era.
  ( Reflect era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
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
  (trc, genstate) <- runGenRS proof sizes (initStableFields >> genT)
  pure (Box proof trc genstate)

-- =============================================
-- Now a test

testTxValidForLEDGER ::
  ( Reflect era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , PrettyA (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Box era ->
  Property
testTxValidForLEDGER proof (Box _ trc@(TRC (_, ledgerState, vtx)) _genstate) =
  -- trc encodes the initial (generated) state, vtx is the transaction
  case applySTSByProof proof trc of
    Right ledgerState' ->
      -- UTxOState and CertState after applying the transaction $$$
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda ledgerState' === totalAda ledgerState
    Left errs ->
      counterexample
        ( show (pcLedgerState proof ledgerState)
            ++ "\n\n"
            ++ show (pcTx proof vtx)
            ++ "\n\n"
            ++ show (prettyA errs)
        )
        (property False)

-- =============================================
-- Make some property tests

-- =========================================================================
-- The generic types make a roundtrip without adding or losing information

txOutRoundTrip ::
  EraTxOut era => Proof era -> TxOut era -> Property
txOutRoundTrip proof x = coreTxOut proof (abstractTxOut proof x) === x

txRoundTrip ::
  EraTx era => Proof era -> Tx era -> Property
txRoundTrip proof x = coreTx proof (abstractTx proof x) === x

txBodyRoundTrip ::
  EraTxBody era => Proof era -> TxBody era -> Property
txBodyRoundTrip proof x = coreTxBody proof (abstractTxBody proof x) === x

txWitRoundTrip ::
  EraTxWits era => Proof era -> TxWits era -> Property
txWitRoundTrip proof x = assembleWits proof (abstractWitnesses proof x) === x

coreTypesRoundTrip :: TestTree
coreTypesRoundTrip =
  testGroup
    "Core types make generic roundtrips"
    [ testGroup
        "TxWits roundtrip"
        [ testPropMax 30 "Babbage era" $ txWitRoundTrip Babbage
        , testPropMax 30 "Alonzo era" $ txWitRoundTrip Alonzo
        , testPropMax 30 "Mary era" $ txWitRoundTrip Mary
        , testPropMax 30 "Allegra era" $ txWitRoundTrip Allegra
        , testPropMax 30 "Shelley era" $ txWitRoundTrip Shelley
        ]
    , testGroup
        "TxBody roundtrips"
        [ testPropMax 30 "Babbage era" $ txBodyRoundTrip Babbage
        , testPropMax 30 "Alonzo era" $ txBodyRoundTrip Alonzo
        , testPropMax 30 "Mary era" $ txBodyRoundTrip Mary
        , testPropMax 30 "Allegra era" $ txBodyRoundTrip Allegra
        , testPropMax 30 "Shelley era" $ txBodyRoundTrip Shelley
        ]
    , testGroup
        "TxOut roundtrips"
        [ testPropMax 30 "Babbage era" $ txOutRoundTrip Babbage
        , testPropMax 30 "Alonzo era" $ txOutRoundTrip Alonzo
        , testPropMax 30 "Mary era" $ txOutRoundTrip Mary
        , testPropMax 30 "Allegra era" $ txOutRoundTrip Allegra
        , testPropMax 30 "Shelley era" $ txOutRoundTrip Shelley
        ]
    , testGroup
        "Tx roundtrips"
        [ testPropMax 30 "Babbage era" $ txRoundTrip Babbage
        , testPropMax 30 "Alonzo era" $ txRoundTrip Alonzo
        , testPropMax 30 "Mary era" $ txRoundTrip Mary
        , testPropMax 30 "Allegra era" $ txRoundTrip Allegra
        , testPropMax 30 "Shelley era" $ txRoundTrip Shelley
        ]
    ]

-- | A single Tx preserves Ada
txPreserveAda :: GenSize -> TestTree
txPreserveAda genSize =
  testGroup
    "Individual Tx's preserve Ada"
    [ testPropMax 30 "Shelley Tx preserves ADA" $
        forAll (genTxAndLEDGERState Shelley genSize) (testTxValidForLEDGER Shelley)
    , testPropMax 30 "Allegra Tx preserves ADA" $
        forAll (genTxAndLEDGERState Allegra genSize) (testTxValidForLEDGER Allegra)
    , testPropMax 30 "Mary Tx preserves ADA" $
        forAll (genTxAndLEDGERState Mary genSize) (testTxValidForLEDGER Mary)
    , testPropMax 30 "Alonzo ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState Alonzo genSize) (testTxValidForLEDGER Alonzo)
    , testPropMax 30 "Babbage ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState Babbage genSize) (testTxValidForLEDGER Babbage)
        -- TODO
        -- testPropMax 30 "Conway ValidTx preserves ADA" $
        --  forAll (genTxAndLEDGERState Conway genSize) (testTxValidForLEDGER Conway)
    ]

-- | Ada is preserved over a trace of length 100
adaIsPreserved ::
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
adaIsPreserved proof numTx gensize =
  testPropMax 30 (show proof ++ " era. Trace length = " ++ show numTx) $
    traceProp
      proof
      numTx
      gensize
      (\firstSt lastSt -> totalAda (mcsNes firstSt) === totalAda (mcsNes lastSt))

tracePreserveAda :: Int -> GenSize -> TestTree
tracePreserveAda numTx gensize =
  testGroup
    ("Total Ada is preserved over traces of length " ++ show numTx)
    [ adaIsPreservedBabbage numTx gensize
    , adaIsPreserved Alonzo numTx gensize
    , adaIsPreserved Mary numTx gensize
    , adaIsPreserved Allegra numTx gensize
    , adaIsPreserved Shelley numTx gensize
    ]

adaIsPreservedBabbage :: Int -> GenSize -> TestTree
adaIsPreservedBabbage numTx gensize = adaIsPreserved Babbage numTx gensize

-- | The incremental Stake invaraint is preserved over a trace of length 100=
stakeInvariant :: EraTxOut era => MockChainState era -> MockChainState era -> Property
stakeInvariant (MockChainState _ _ _ _) (MockChainState nes _ _ _) =
  case (lsUTxOState . esLState . nesEs) nes of
    (UTxOState utxo _ _ _ istake _) -> istake === updateStakeDistribution def mempty mempty utxo

incrementStakeInvariant ::
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  GenSize ->
  TestTree
incrementStakeInvariant proof gensize =
  testPropMax 30 (show proof ++ " era. Trace length = 100") $
    traceProp proof 100 gensize stakeInvariant

incrementalStake :: GenSize -> TestTree
incrementalStake genSize =
  testGroup
    "Incremental Stake invariant holds"
    [ -- TODO re-enable this once we have added all the new rules to Conway
      -- incrementStakeInvariant Conway genSize,
      incrementStakeInvariant Babbage genSize
    , incrementStakeInvariant Alonzo genSize
    , incrementStakeInvariant Mary genSize
    , incrementStakeInvariant Allegra genSize
    , incrementStakeInvariant Shelley genSize
    ]

genericProperties :: GenSize -> TestTree
genericProperties genSize =
  testGroup
    "Generic Property tests"
    [ coreTypesRoundTrip
    , txPreserveAda genSize
    , tracePreserveAda 45 genSize
    , incrementalStake genSize
    , testTraces 45
    , epochPreserveAda genSize
    , twiddleInvariantHoldsEras
    ]

epochPreserveAda :: GenSize -> TestTree
epochPreserveAda genSize =
  testGroup
    "Ada is preserved in each epoch"
    [ adaIsPreservedInEachEpoch Babbage genSize
    , adaIsPreservedInEachEpoch Alonzo genSize
    , adaIsPreservedInEachEpoch Mary genSize
    , adaIsPreservedInEachEpoch Allegra genSize
    , adaIsPreservedInEachEpoch Shelley genSize
    ]

adaIsPreservedInEachEpoch ::
  forall era.
  Reflect era =>
  Proof era ->
  GenSize ->
  TestTree
adaIsPreservedInEachEpoch proof genSize =
  testPropMax 30 (show proof) $
    forEachEpochTrace proof 200 genSize withTrace
  where
    withTrace :: Trace (MOCKCHAIN era) -> Property
    withTrace trc = totalAda (mcsNes trcInit) === totalAda (mcsNes trcLast)
      where
        trcInit = _traceInitState trc
        trcLast = lastState trc

twiddleInvariantHolds ::
  forall a.
  ( Arbitrary a
  , Show a
  , Twiddle a
  ) =>
  String ->
  TestTree
twiddleInvariantHolds name =
  testPropMax 30 name $ twiddleInvariantProp @a

twiddleInvariantHoldsEras :: TestTree
twiddleInvariantHoldsEras =
  testGroup
    "Twiddle invariant holds for TxBody"
    [ twiddleInvariantHolds @(AlonzoTxBody (AlonzoEra StandardCrypto)) "Alonzo"
    , twiddleInvariantHolds @(BabbageTxBody (BabbageEra StandardCrypto)) "Babbage"
    ]

-- ==============================================================
-- Infrastrucure for running individual tests, with easy replay.
-- In ghci just type
-- :main --quickcheck-replay=205148

main :: IO ()
main = defaultMain $ adaIsPreservedBabbage 100 (def {blocksizeMax = 4})

main8 :: IO ()
main8 = test 100 Babbage

test :: Int -> Proof era -> IO ()
test n proof = defaultMain $
  case proof of
    -- TODO
    -- Conway ->
    --  testPropMax 30 "Conway ValidTx preserves ADA" $
    --    withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Babbage ->
      testPropMax 30 "Babbage ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Alonzo ->
      testPropMax 30 "Alonzo ValidTx preserves ADA" $
        withMaxSuccess n (forAll (genTxAndLEDGERState proof def) (testTxValidForLEDGER proof))
    Shelley ->
      testPropMax 30 "Shelley ValidTx preserves ADA" $
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
  (Reflect era, PrettyA a) =>
  (Proof era -> GenRS era a) ->
  (a -> IO ()) ->
  Proof era ->
  IO ()
runTest computeWith action proof = do
  ans <- generate (makeGen proof computeWith)
  print (prettyA ans)
  action ans

main2 :: IO ()
main2 = runTest (\x -> fst <$> genAlonzoTx x (SlotNo 0)) (const (pure ())) Babbage

main3 :: IO ()
main3 = runTest (\_x -> UTxO . fst <$> genUTxO) action Alonzo
  where
    action (UTxO x) = putStrLn ("Size = " ++ show (Map.size x))
