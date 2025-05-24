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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  NewEpochState,
  PulsingRewUpdate,
  UTxOState,
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  RupdEnv,
  ShelleyLedgersEnv,
  ShelleyTICK,
  UtxoEnv (..),
 )
import Cardano.Ledger.Shelley.State
import Cardano.Slotting.Slot (EpochNo, SlotNo (..))
import Control.Monad.Trans.RWS.Strict (gets)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence (Seq)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.ImpTest ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle, twiddleInvariantProp)
import Test.Cardano.Ledger.Common (ToExpr (..), showExpr)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Era (EraTest)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (totalAda), isValid')
import Test.Cardano.Ledger.Generic.GenState (
  EraGenericGen,
  GenEnv (..),
  GenSize (..),
  GenState (..),
  initStableFields,
  modifyModel,
  runGenRS,
 )
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockChainState (..))
import Test.Cardano.Ledger.Generic.ModelState
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Trace (
  Gen1,
  forEachEpochTrace,
  testPropMax,
  testTraces,
  traceProp,
 )
import Test.Cardano.Ledger.Generic.TxGen (
  genAlonzoTx,
  genUTxO,
  runSTSWithContext,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Control.State.Transition.Trace (Trace (..), lastState)
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)

-- =====================================
-- Top level generators of TRC

genTxAndUTXOState ::
  ( Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , EraGenericGen era
  ) =>
  GenSize -> Gen (TRC (EraRule "UTXOW" era), GenState era)
genTxAndUTXOState gsize = do
  (TRC (LedgerEnv slotNo _ _ pp _, ledgerState, vtx), genState) <- genTxAndLEDGERState gsize
  pure (TRC (UtxoEnv slotNo pp def, lsUTxOState ledgerState, vtx), genState)

genTxAndLEDGERState ::
  forall era.
  ( Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , EraGenericGen era
  ) =>
  GenSize ->
  Gen (TRC (EraRule "LEDGER" era), GenState era)
genTxAndLEDGERState sizes = do
  let slotNo = SlotNo (startSlot sizes)
  txIx <- arbitrary
  let genT = do
        (initial, _) <- genUTxO -- Generate a random UTxO, so mUTxO is not empty
        modifyModel (\m -> m {mUTxO = initial})
        (_utxo, tx) <- genAlonzoTx slotNo
        model <- gets gsModel
        pp <- gets (gePParams . gsGenEnv)
        let ledgerState = extract @(LedgerState era) model
            ledgerEnv = LedgerEnv slotNo Nothing txIx pp (ChainAccountState (Coin 0) (Coin 0))
        pure $ TRC (ledgerEnv, ledgerState, tx)
  (trc, genstate) <- runGenRS sizes (initStableFields >> genT)
  pure (trc, genstate)

-- =============================================
-- Now a test

testTxValidForLEDGER ::
  forall era.
  ( Reflect era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , EraTest era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , STS (EraRule "LEDGER" era)
  , ToExpr (Environment (EraRule "LEDGER" era))
  ) =>
  (TRC (EraRule "LEDGER" era), GenState era) ->
  Property
testTxValidForLEDGER (trc@(TRC (env, ledgerState, vtx)), _genstate) =
  -- trc encodes the initial (generated) state, vtx is the transaction
  case runSTSWithContext @era trc of
    Right ledgerState' ->
      -- UTxOState and CertState after applying the transaction $$$
      classify (coerce (isValid' (reify @era) vtx)) "TxValid" $
        totalAda ledgerState' === totalAda ledgerState
    Left errs ->
      counterexample
        ( showExpr env
            ++ "\n\n"
            ++ showExpr ledgerState
            ++ "\n\n"
            ++ showExpr vtx
            ++ "\n\n"
            ++ showExpr errs
        )
        (property False)

-- =============================================
-- Make some property tests

-- =========================================================================
-- The generic types make a roundtrip without adding or losing information

-- | A single Tx preserves Ada
txPreserveAda :: GenSize -> TestTree
txPreserveAda genSize =
  testGroup
    "Individual Tx's preserve Ada"
    [ testPropMax 30 "Shelley Tx preserves Ada" $
        forAll (genTxAndLEDGERState @ShelleyEra genSize) testTxValidForLEDGER
    , testPropMax 30 "Allegra Tx preserves ADA" $
        forAll (genTxAndLEDGERState @AllegraEra genSize) testTxValidForLEDGER
    , testPropMax 30 "Mary Tx preserves ADA" $
        forAll (genTxAndLEDGERState @MaryEra genSize) testTxValidForLEDGER
    , testPropMax 30 "Alonzo ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState @AlonzoEra genSize) testTxValidForLEDGER
    , testPropMax 30 "Babbage ValidTx preserves ADA" $
        forAll (genTxAndLEDGERState @BabbageEra genSize) testTxValidForLEDGER
    ]

-- | Ada is preserved over a trace of length 100
adaIsPreserved ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  TestTree
adaIsPreserved numTx gensize =
  testPropMax 30 (eraName @era ++ " era. Trace length = " ++ show numTx) $
    traceProp @era
      numTx
      gensize
      (\firstSt lastSt -> totalAda (mcsNes firstSt) === totalAda (mcsNes lastSt))

tracePreserveAda :: Int -> GenSize -> TestTree
tracePreserveAda numTx gensize =
  testGroup
    ("Total Ada is preserved over traces of length " ++ show numTx)
    [ adaIsPreserved @BabbageEra numTx gensize
    , adaIsPreserved @AlonzoEra numTx gensize
    , adaIsPreserved @MaryEra numTx gensize
    , adaIsPreserved @AllegraEra numTx gensize
    , adaIsPreserved @ShelleyEra numTx gensize
    ]

-- | The incremental Stake invaraint is preserved over a trace of length 100=
stakeInvariant :: EraStake era => MockChainState era -> MockChainState era -> Property
stakeInvariant (MockChainState {}) (MockChainState nes _ _ _) =
  let utxo = nes ^. utxoL
   in nes ^. instantStakeL === addInstantStake utxo mempty

incrementStakeInvariant ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  GenSize ->
  TestTree
incrementStakeInvariant gensize =
  testPropMax 30 (eraName @era ++ " era. Trace length = 100") $
    traceProp @era 100 gensize stakeInvariant

incrementalStake :: GenSize -> TestTree
incrementalStake genSize =
  testGroup
    "Incremental Stake invariant holds"
    [ -- TODO re-enable this once we have added all the new rules to Conway
      -- incrementStakeInvariant Conway genSize,
      incrementStakeInvariant @BabbageEra genSize
    , incrementStakeInvariant @AlonzoEra genSize
    , incrementStakeInvariant @MaryEra genSize
    , incrementStakeInvariant @AllegraEra genSize
    , incrementStakeInvariant @ShelleyEra genSize
    ]

genericProperties :: GenSize -> TestTree
genericProperties genSize =
  testGroup
    "Generic Property tests"
    [ txPreserveAda genSize
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
    [ adaIsPreservedInEachEpoch @BabbageEra genSize
    , adaIsPreservedInEachEpoch @AlonzoEra genSize
    , adaIsPreservedInEachEpoch @MaryEra genSize
    , adaIsPreservedInEachEpoch @AllegraEra genSize
    , adaIsPreservedInEachEpoch @ShelleyEra genSize
    ]

adaIsPreservedInEachEpoch ::
  forall era.
  ( ShelleyEraAccounts era
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , State (EraRule "RUPD" era) ~ StrictMaybe PulsingRewUpdate
  , State (EraRule "TICK" era) ~ NewEpochState era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Environment (EraRule "TICK" era) ~ ()
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , BaseM (EraRule "NEWEPOCH" era) ~ ShelleyBase
  , Embed (EraRule "TICK" era) (MOCKCHAIN era)
  , Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era)
  , Embed (EraRule "RUPD" era) (ShelleyTICK era)
  , Embed (EraRule "LEDGERS" era) (MOCKCHAIN era)
  , EraGenericGen era
  , ToExpr (PredicateFailure (EraRule "NEWEPOCH" era))
  , ToExpr (PredicateFailure (EraRule "RUPD" era))
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  GenSize ->
  TestTree
adaIsPreservedInEachEpoch genSize =
  testPropMax 30 (eraName @era) $
    forEachEpochTrace @era 200 genSize withTrace
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
    [ twiddleInvariantHolds @(TxBody AlonzoEra) "Alonzo"
    , twiddleInvariantHolds @(TxBody BabbageEra) "Babbage"
    ]
