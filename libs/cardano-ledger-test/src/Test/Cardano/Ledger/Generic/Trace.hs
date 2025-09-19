{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.Trace where

-- =========================================================================

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals)
import Cardano.Ledger.Coin (CompactForm (CompactCoin))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  prevPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyUtxowPredFailure (ScriptWitnessNotValidatingUTXOW),
 )
import Cardano.Ledger.Shelley.State
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad (forM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (get, gets)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Control.State.Transition as STS
import Control.State.Transition.Extended (IRC (), STS (..), TRC (..))
import Data.Default (Default (def))
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SS
import qualified Data.Set as Set
import Data.TreeDiff
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Debug.Trace as Debug
import GHC.Word (Word64)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Examples.STSTestUtils (EraModel (..))
import Test.Cardano.Ledger.Generic.Functions (
  adaPots,
  totalAda,
 )
import Test.Cardano.Ledger.Generic.GenState (
  EraGenericGen,
  GenEnv (..),
  GenRS,
  GenSize (..),
  GenState (..),
  defaultGenSize,
  getBlocksizeMax,
  getReserves,
  getSlot,
  getSlotDelta,
  getTreasury,
  initStableFields,
  initialLedgerState,
  modifyModel,
  runGenRS,
 )
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.MockChain
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, stashedAVVMAddressesZero)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)
import Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (stakeDistr)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.Control.State.Transition.Trace (Trace (..), lastState, splitTrace)
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..), traceFromInitState)

-- ===========================================

-- | Generate a Tx and an internal Model of the state after the tx
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel ::
  forall era.
  EraGenericGen era =>
  Int -> SlotNo -> GenRS era (Tx TopTx era)
genRsTxAndModel n slot = do
  (_, tx) <- genAlonzoTx slot
  modifyModel (\model -> applyTx n slot model tx)
  pure tx

-- | Generate a Vector of (StrictSeq (Tx era))  representing a (Vector Block)
genRsTxSeq ::
  forall era.
  EraGenericGen era =>
  Int ->
  Int ->
  [(StrictSeq (Tx TopTx era), SlotNo)] ->
  SlotNo ->
  GenRS era (Vector (StrictSeq (Tx TopTx era), SlotNo))
genRsTxSeq this lastN ans _slot | this >= lastN = do
  pure (Vector.fromList (reverse ans))
genRsTxSeq this lastN ans slot = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  txs <- forM [0 .. n - 1] (\i -> genRsTxAndModel (this + i) slot)
  newSlotRange <- gets getSlotDelta
  nextSlotNo <- lift $ SlotNo . (+ unSlotNo slot) <$> choose newSlotRange
  genRsTxSeq (this + n) lastN ((SS.fromList txs, slot) : ans) nextSlotNo

-- | Generate a Vector of Blocks, and an initial LedgerState
genTxSeq ::
  forall era.
  EraGenericGen era =>
  GenSize -> -- Size of things the generated code should adhere to
  Int -> -- The number of Tx in the sequence
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Vector (StrictSeq (Tx TopTx era), SlotNo), GenState era)
genTxSeq gensize numTx initialize = do
  runGenRS gensize (initialize >> genRsTxSeq 0 numTx [] (SlotNo 1))

runTest :: IO ()
runTest = do
  (v, _) <- generate $ genTxSeq @BabbageEra defaultGenSize 20 (pure ())
  print (Vector.length v)

-- ==================================================================
-- Constructing the "real", initial NewEpochState, from the GenState

initialMockChainState ::
  forall era.
  (Reflect era, ShelleyEraAccounts era) =>
  GenState era ->
  MockChainState era
initialMockChainState gstate =
  MockChainState newepochstate newepochstate (getSlot gstate) 0
  where
    ledgerstate = initialLedgerState gstate
    newepochstate =
      NewEpochState
        { nesEL = EpochNo 0
        , nesBprev = BlocksMade Map.empty
        , nesBcur = BlocksMade Map.empty
        , nesEs = makeEpochState gstate ledgerstate
        , nesRu = SNothing
        , nesPd = PoolDistr (gsInitialPoolDistr gstate) (CompactCoin 1)
        , stashedAVVMAddresses = stashedAVVMAddressesZero (reify @era)
        }

makeEpochState ::
  (Reflect era, ShelleyEraAccounts era) => GenState era -> LedgerState era -> EpochState era
makeEpochState gstate ledgerstate =
  EpochState
    { esChainAccountState =
        ChainAccountState
          { casTreasury = getTreasury gstate
          , casReserves = getReserves gstate
          }
    , esSnapshots = snaps ledgerstate
    , esLState = ledgerstate
    , esNonMyopic = def
    }
    & prevPParamsEpochStateL .~ gePParams (gsGenEnv gstate)
    & curPParamsEpochStateL .~ gePParams (gsGenEnv gstate)

snaps :: (EraTxOut era, EraCertState era, ShelleyEraAccounts era) => LedgerState era -> SnapShots
snaps (LedgerState UTxOState {utxosUtxo = u, utxosFees = f} certState) =
  SnapShots snap (calculatePoolDistr snap) snap snap f
  where
    pstate = certState ^. certPStateL
    dstate = certState ^. certDStateL
    snap = stakeDistr u dstate pstate

-- ==============================================================================

raiseMockError ::
  forall era.
  Reflect era =>
  Word64 ->
  SlotNo ->
  EpochState era ->
  NonEmpty (MockChainFailure era) ->
  [Tx TopTx era] ->
  GenState era ->
  String
raiseMockError slot (SlotNo next) epochstate _pdfs _txs _ =
  let _ssPoolParams = epochstate ^. esLStateL . lsCertStateL . certPStateL . psStakePoolsL
   in show
        [ toExpr $ adaPots reify epochstate
        , toExpr slot
        , toExpr next
        , -- ppString "===================================",
          -- ppString "Real Pool Params\n" <> ppMap pcKeyHash pcPoolParams poolParams,
          toExpr (epochstate ^. curPParamsEpochStateL)
        ]

badScripts :: Proof era -> [MockChainFailure era] -> Set.Set ScriptHash
badScripts proof xs = Fold.foldl' (\s mcf -> Set.union s (getw proof mcf)) Set.empty xs
  where
    getw :: Proof era -> MockChainFailure era -> Set.Set ScriptHash
    getw
      Babbage
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  ( AlonzoInBabbageUtxowPredFailure
                      ( ShelleyInAlonzoUtxowPredFailure
                          (ScriptWitnessNotValidatingUTXOW set)
                        )
                    )
                )
            )
        ) = set
    getw
      Alonzo
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  ( ShelleyInAlonzoUtxowPredFailure
                      (ScriptWitnessNotValidatingUTXOW set)
                    )
                )
            )
        ) = set
    getw
      Mary
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  (ScriptWitnessNotValidatingUTXOW set)
                )
            )
        ) = set
    getw
      Allegra
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  (ScriptWitnessNotValidatingUTXOW set)
                )
            )
        ) = set
    getw _ _ = Set.empty

shortTxOut :: EraTxOut era => TxOut era -> Expr
shortTxOut out = case out ^. addrTxOutL of
  Addr _ pay _ -> toExpr (pay, out ^. coinTxOutL)
  _ -> error "Bootstrap Address in shortTxOut"

smartTxBody :: EraTest era => MUtxo era -> TxBody TopTx era -> Expr
smartTxBody u txbody = toExpr (u, txbody)

-- =====================================================================
-- HasTrace instance of MOCKCHAIN depends on STS(MOCKCHAIN era) instance
-- We show the type family instances here for reference.
{-
instance STS (MOCKCHAIN era)
  where
  type State (MOCKCHAIN era) = MockChainState era
  type Signal (MOCKCHAIN era) = (MockBlock era)
  type Environment (MOCKCHAIN era) = ()
-}
-- ==============================================================

data Gen1 era = Gen1 (Vector (StrictSeq (Tx TopTx era), SlotNo)) (GenState era)

instance
  ( STS (MOCKCHAIN era)
  , Reflect era
  , EraTest era
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  HasTrace (MOCKCHAIN era) (Gen1 era)
  where
  type BaseEnv (MOCKCHAIN era) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals

  envGen _gstate = pure ()

  sigGen (Gen1 txss gs) () mcs@(MockChainState newepoch _ (SlotNo lastSlot) count) = do
    let NewEpochState epochnum _ _ epochstate _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer epochnum lastSlot count pooldistr
    let (txs, nextSlotNo) = txss ! count
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal

    case runShelleyBase (applySTSTest (TRC @(MOCKCHAIN era) ((), mcs, mockblock))) of
      Left pdfs ->
        let txsl = Fold.toList txs
         in Debug.trace
              (raiseMockError lastSlot nextSlotNo epochstate pdfs txsl gs)
              ( error . unlines $
                  ["EpochState:"]
                    <> [showExpr epochstate]
                    <> ["Tx:"]
                    <> toList (showExpr <$> txs)
                    <> ["sigGen in (HasTrace (MOCKCHAIN era) (Gen1 era)) FAILS"]
                    <> map showExpr (Fold.toList pdfs)
              )
      Right mcs2 -> seq mcs2 (pure mockblock)

  shrinkSignal _ = []

-- TODO, perhaps we might add shrinking like this. Just a thought. Failure is usually caused by just one Tx
-- in the Block. If the first or last Tx, is independent of the failing Tx, we can probably throw them away.
--  shrinkSignal (MockBlock _ _ xs) | SS.null xs = []
--  shrinkSignal (MockBlock i s xs) = [MockBlock i s (SS.drop 1 xs), MockBlock i s (SS.take (SS.length xs - 1) xs)]

mapProportion :: EpochNo -> Word64 -> Int -> Map.Map k Int -> Gen k
mapProportion epochnum lastSlot count m =
  case pairs of
    [] ->
      -- TODO, we need to figure out why this occurs. It always occurs near SlotNo 300, So I am assuming that
      -- sometimes as we move into the 3rd epoch, however stakeDistr is computed becomes empty. This is probably
      -- because there is no action in Test.Cardano.Ledger.Constrained.Trace.Actions for the epoch boundary.
      -- This temporary fix is good enough for now. But the annoying trace message reminds us to fix this.
      Debug.trace
        ( "There are no stakepools to choose an issuer from"
            ++ ", epoch="
            ++ show epochnum
            ++ ", last slot="
            ++ show lastSlot
            ++ ", index of Tx="
            ++ show count
        )
        discard
    (w : _) ->
      if all (\(n, _k) -> n == 0) pairs
        then snd w -- All stakepools have zero Stake, choose issuer arbitrarily. possible, but rare.
        else frequency pairs
  where
    pairs = [(n, pure k) | (k, n) <- Map.toList m]

chooseIssuer :: EpochNo -> Word64 -> Int -> PoolDistr -> Gen (KeyHash 'StakePool)
chooseIssuer epochnum lastSlot count (PoolDistr m _) = mapProportion epochnum lastSlot count (getInt <$> m)
  where
    getInt x = floor (individualPoolStake x * 1000)

-- ===================================================================================

-- Generating Traces, and making properties out of a Trace
-- =========================================================================

genTrace ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Trace (MOCKCHAIN era))
genTrace numTxInTrace gsize initialize = do
  (vs, genstate) <- genTxSeq gsize numTxInTrace initialize
  let initState = initialMockChainState genstate
  traceFromInitState @(MOCKCHAIN era)
    testGlobals
    (fromIntegral (length vs))
    (Gen1 vs genstate)
    (Just (\_ -> pure $ Right initState))

traceProp ::
  forall era prop.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  (MockChainState era -> MockChainState era -> prop) ->
  Gen prop
traceProp numTxInTrace gsize f = do
  trace1 <- genTrace numTxInTrace gsize initStableFields
  pure (f (_traceInitState trace1) (lastState trace1))

forEachEpochTrace ::
  forall era prop.
  ( Testable prop
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  (Trace (MOCKCHAIN era) -> prop) ->
  Gen Property
forEachEpochTrace tracelen genSize f = do
  let newEpoch tr1 tr2 = nesEL (mcsNes tr1) /= nesEL (mcsNes tr2)
  trc <- do
    (vs, genstate) <- genTxSeq genSize tracelen initStableFields
    let initState = initialMockChainState genstate
    traceFromInitState @(MOCKCHAIN era)
      testGlobals
      (fromIntegral (length vs))
      (Gen1 vs genstate)
      (Just (\_ -> pure $ Right initState))
  let propf (subtrace, index) = counterexample ("Subtrace of EpochNo " ++ show index ++ " fails.") (f subtrace)
  -- The very last sub-trace may not be a full epoch, so we throw it away.
  case reverse (splitTrace newEpoch trc) of
    [] -> pure (property True)
    _ : revSplits -> pure (conjoin (propf <$> zip (reverse revSplits) [0 :: Int ..]))

-- | Check a property on the 'sts' traces.
--
-- Takes an optional generator for initial state of the STS.
forAllTraceFromInitState ::
  forall sts traceGenEnv prop.
  ( HasTrace sts traceGenEnv
  , Testable prop
  , Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  -- | Optional generator of STS initial state
  Maybe (IRC sts -> Gen (Either (NonEmpty (STS.PredicateFailure sts)) (State sts))) ->
  (Trace sts -> prop) ->
  Property
forAllTraceFromInitState baseEnv maxTraceLength traceGenEnv genSt0 =
  forAllShrinkBlind
    (traceFromInitState @sts @traceGenEnv baseEnv maxTraceLength traceGenEnv genSt0)
    (const [])

-- =========================================================================
-- Test for just making a trace

-- | We are making 'smoke' tests, testing for smoke rather than fire.
--   Under the assumption that shorter tests have advantages
--   like not getting turned off because the tests take too long. A glaring failure is
--   likely to be caught in 'n' tests, rather than the standard '100'
testPropMax :: Testable prop => Int -> String -> prop -> Spec
testPropMax n name x = prop name (withMaxSuccess n x)

chainTest ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , Eq (StashedAVVMAddresses era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  Spec
chainTest n gsize = testPropMax 30 (eraName @era) action
  where
    action = do
      (vs, genstate) <- genTxSeq @era gsize n initStableFields
      let initState = initialMockChainState genstate
      trace1 <-
        traceFromInitState @(MOCKCHAIN era)
          testGlobals
          (fromIntegral (length vs))
          (Gen1 vs genstate)
          (Just (\_ -> pure $ Right initState))
      -- Here is where we can add some properties for traces:
      pure (_traceInitState trace1 === initState)

testTraces :: Int -> Spec
testTraces n =
  describe "MockChainTrace" $ do
    chainTest @BabbageEra n defaultGenSize
    chainTest @AlonzoEra n defaultGenSize
    chainTest @MaryEra n defaultGenSize
    chainTest @AllegraEra n defaultGenSize
    multiEpochTest @BabbageEra 225 defaultGenSize
    multiEpochTest @ShelleyEra 225 defaultGenSize

-- | Show that Ada is preserved across multiple Epochs
multiEpochTest ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  Spec
multiEpochTest numTx gsize =
  let gensize = gsize {blocksizeMax = 4, slotDelta = (6, 12)}
      getEpoch mockchainstate = nesEL (mcsNes mockchainstate)
      propf firstSt lastSt =
        collect (getEpoch lastSt) (totalAda firstSt === totalAda lastSt)
   in testPropMax
        30
        ("Multi epoch. Ada is preserved. " ++ (eraName @era) ++ ". Trace length = " ++ show numTx)
        (traceProp @era numTx gensize propf)
