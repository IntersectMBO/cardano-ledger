{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
  dsUnifiedL,
  esLStateL,
  lsCertStateL,
  prevPParamsEpochStateL,
  psDepositsL,
  psStakePoolParamsL,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyUtxowPredFailure (ScriptWitnessNotValidatingUTXOW),
 )
import Cardano.Ledger.State (
  ChainAccountState (..),
  EraCertState (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  SnapShots (..),
  UTxO (..),
  calculatePoolDistr,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad (forM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (get, gets)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Control.State.Transition as STS
import Control.State.Transition.Extended (IRC (), STS (..), TRC (..))
import Data.Default (Default (def))
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
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Generic.ApplyTx (applyTx)
import Test.Cardano.Ledger.Generic.Functions (
  adaPots,
  allInputs,
  getBody,
  getScriptWits,
  getWitnesses,
  isValid',
  totalAda,
  txoutFields,
 )
import Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
  GenRS,
  GenSize (..),
  GenState (..),
  getBlocksizeMax,
  getReserves,
  getSlot,
  getSlotDelta,
  getTreasury,
  gsModel,
  initStableFields,
  initialLedgerState,
  modifyModel,
  runGenRS,
 )
import Test.Cardano.Ledger.Generic.MockChain
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, stashedAVVMAddressesZero)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)
import Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (stakeDistr)
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.Control.State.Transition.Trace (Trace (..), lastState, splitTrace)
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..), traceFromInitState)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- ===========================================

-- | Generate a Tx and an internal Model of the state after the tx
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel :: forall era. Reflect era => Proof era -> Int -> SlotNo -> GenRS era (Tx era)
genRsTxAndModel proof n slot = do
  (_, tx) <- genAlonzoTx proof slot
  modifyModel (\model -> applyTx proof n slot model tx)
  pure tx

-- | Generate a Vector of (StrictSeq (Tx era))  representing a (Vector Block)
genRsTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  Int ->
  Int ->
  [(StrictSeq (Tx era), SlotNo)] ->
  SlotNo ->
  GenRS era (Vector (StrictSeq (Tx era), SlotNo))
genRsTxSeq _ this lastN ans _slot | this >= lastN = do
  pure (Vector.fromList (reverse ans))
genRsTxSeq proof this lastN ans slot = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  txs <- forM [0 .. n - 1] (\i -> genRsTxAndModel proof (this + i) slot)
  newSlotRange <- gets getSlotDelta
  nextSlotNo <- lift $ SlotNo . (+ unSlotNo slot) <$> choose newSlotRange
  genRsTxSeq proof (this + n) lastN ((SS.fromList txs, slot) : ans) nextSlotNo

-- | Generate a Vector of Blocks, and an initial LedgerState
genTxSeq ::
  forall era.
  Reflect era =>
  Proof era -> -- Proof of the Era we want to generate the sequence in
  GenSize -> -- Size of things the generated code should adhere to
  Int -> -- The number of Tx in the sequence
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Vector (StrictSeq (Tx era), SlotNo), GenState era)
genTxSeq proof gensize numTx initialize = do
  runGenRS proof gensize (initialize >> genRsTxSeq proof 0 numTx [] (SlotNo $ 1))

runTest :: IO ()
runTest = do
  (v, _) <- generate $ genTxSeq Babbage def 20 (pure ())
  print (Vector.length v)

-- ==================================================================
-- Constructing the "real", initial NewEpochState, from the GenState

initialMockChainState ::
  Reflect era =>
  Proof era ->
  GenState era ->
  MockChainState era
initialMockChainState proof gstate =
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
        , stashedAVVMAddresses = stashedAVVMAddressesZero proof
        }

makeEpochState :: Reflect era => GenState era -> LedgerState era -> EpochState era
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

snaps :: (EraTxOut era, EraCertState era) => LedgerState era -> SnapShots
snaps (LedgerState UTxOState {utxosUtxo = u, utxosFees = f} certState) =
  SnapShots snap (calculatePoolDistr snap) snap snap f
  where
    pstate = certState ^. certPStateL
    dstate = certState ^. certDStateL
    snap = stakeDistr u dstate pstate

-- ==============================================================================

-- | Turn a UTxO into a smaller UTxO, with only entries mentioned in
--   the inputs of 'txs' ,  then pretty print it.
pcSmallUTxO :: EraTx era => Proof era -> MUtxo era -> [Tx era] -> Expr
pcSmallUTxO proof u txs = ppMap toExpr (shortTxOut proof) m
  where
    keys = Set.unions (map f txs)
    f tx = allInputs proof (getBody proof tx)
    m = Map.restrictKeys u keys

raiseMockError ::
  forall era.
  ( Reflect era
  , EraTest era
  , ToExpr (MockChainFailure era)
  ) =>
  Word64 ->
  SlotNo ->
  EpochState era ->
  NonEmpty (MockChainFailure era) ->
  [Tx era] ->
  GenState era ->
  String
raiseMockError slot (SlotNo next) epochstate pdfs txs GenState {..} =
  let utxo = unUTxO $ (utxosUtxo . lsUTxOState . esLState) epochstate
      _ssPoolParams = epochstate ^. esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL
      _pooldeposits = epochstate ^. esLStateL . lsCertStateL . certPStateL . psDepositsL
      _keydeposits = UM.depositMap $ epochstate ^. esLStateL . lsCertStateL . certDStateL . dsUnifiedL
   in show
        [ pcSmallUTxO reify utxo txs
        , ppSet toExpr gsStablePools
        , ppSet toExpr gsStableDelegators
        , -- You never know what is NEEDED to debug a failure, and what is a DISTRACTION
          -- These things certainly fall in that category. I leave them commented out so if
          -- they are not a distraction in the current error, they are easy to turn back on.
          -- ppString "===================================",
          -- ppString "PoolDeposits\n" <> ppMap pcKeyHash pcCoin _pooldeposits,
          -- ppString "===================================",
          -- ppString "KeyDeposits\n" <> ppMap pcCredential pcCoin _keydeposits,
          -- ppString "Model KeyDeposits\n" <> ppMap pcCredential pcCoin (mKeyDeposits gsModel),
          -- ppString "Initial Pool Distr\n" <> ppMap pcKeyHash pcIndividualPoolStake gsInitialPoolDistr,
          -- ppString "===================================",
          -- ppString "Initial Pool Params\n" <> ppMap pcKeyHash pcPoolParams gsInitialPoolParams,
          -- ppString "===================================",
          -- ppString "Initial Rewards\n" <> ppMap pcCredential pcCoin gsInitialRewards,
          showBlock utxo txs
        , toExpr $ adaPots reify epochstate
        , ppList toExpr (Fold.toList pdfs)
        , toExpr slot
        , toExpr next
        , ppMap
            toExpr
            toExpr
            (Map.restrictKeys gsScripts . badScripts reify $ Fold.toList pdfs)
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

showBlock :: forall era. (Reflect era, EraTest era) => MUtxo era -> [Tx era] -> Expr
showBlock u txs = ppList pppair (zip txs [0 ..])
  where
    pppair (tx, n) =
      let body = getBody reify tx
       in toExpr
            [ toExpr @Int n
            , toExpr $ hashAnnotated body
            , smartTxBody u body
            , toExpr (isValid' reify tx)
            , ppMap toExpr toExpr (getScriptWits reify (getWitnesses reify tx))
            ]

shortTxOut :: EraTxOut era => Proof era -> TxOut era -> Expr
shortTxOut proof out = case txoutFields proof out of
  (Addr _ pay _, _, _) -> toExpr (pay, out ^. coinTxOutL)
  _ -> error "Bootstrap Address in shortTxOut"

smartTxBody :: EraTest era => MUtxo era -> TxBody era -> Expr
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

data Gen1 era = Gen1 (Vector (StrictSeq (Tx era), SlotNo)) (GenState era)

instance
  ( STS (MOCKCHAIN era)
  , Reflect era
  , EraTest era
  , ToExpr (MockChainFailure era)
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
                  "sigGen in (HasTrace (MOCKCHAIN era) (Gen1 era)) FAILS" : map show (Fold.toList pdfs)
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
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Trace (MOCKCHAIN era))
genTrace proof numTxInTrace gsize initialize = do
  (vs, genstate) <- genTxSeq proof gsize numTxInTrace initialize
  let initState = initialMockChainState proof genstate
  traceFromInitState @(MOCKCHAIN era)
    testGlobals
    (fromIntegral (length vs))
    (Gen1 vs genstate)
    (Just (\_ -> pure $ Right initState))

traceProp ::
  forall era prop.
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  (MockChainState era -> MockChainState era -> prop) ->
  Gen prop
traceProp proof numTxInTrace gsize f = do
  trace1 <- genTrace proof numTxInTrace gsize initStableFields
  pure (f (_traceInitState trace1) (lastState trace1))

forEachEpochTrace ::
  forall era prop.
  ( Testable prop
  , Reflect era
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  (Trace (MOCKCHAIN era) -> prop) ->
  Gen Property
forEachEpochTrace proof tracelen genSize f = do
  let newEpoch tr1 tr2 = nesEL (mcsNes tr1) /= nesEL (mcsNes tr2)
  trc <- case proof of
    Conway -> genTrace proof tracelen genSize initStableFields
    Babbage -> genTrace proof tracelen genSize initStableFields
    Alonzo -> genTrace proof tracelen genSize initStableFields
    Allegra -> genTrace proof tracelen genSize initStableFields
    Mary -> genTrace proof tracelen genSize initStableFields
    Shelley -> genTrace proof tracelen genSize initStableFields
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
testPropMax :: Testable prop => Int -> String -> prop -> TestTree
testPropMax n name x = testProperty name (withMaxSuccess n x)

chainTest ::
  forall era.
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  , Eq (StashedAVVMAddresses era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
chainTest proof n gsize = testPropMax 30 message action
  where
    message = show proof ++ " era."
    action = do
      (vs, genstate) <- genTxSeq proof gsize n initStableFields
      let initState = initialMockChainState proof genstate
      trace1 <-
        traceFromInitState @(MOCKCHAIN era)
          testGlobals
          (fromIntegral (length vs))
          (Gen1 vs genstate)
          (Just (\_ -> pure $ Right initState))
      -- Here is where we can add some properties for traces:
      pure (_traceInitState trace1 === initState)

testTraces :: Int -> TestTree
testTraces n =
  testGroup
    "MockChainTrace"
    [ chainTest Babbage n def
    , chainTest Alonzo n def
    , chainTest Mary n def
    , chainTest Allegra n def
    , multiEpochTest Babbage 225 def
    , multiEpochTest Shelley 225 def
    ]

-- | Show that Ada is preserved across multiple Epochs
multiEpochTest ::
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
multiEpochTest proof numTx gsize =
  let gensize = gsize {blocksizeMax = 4, slotDelta = (6, 12)}
      getEpoch mockchainstate = nesEL (mcsNes mockchainstate)
      propf firstSt lastSt =
        collect (getEpoch lastSt) (totalAda firstSt === totalAda lastSt)
   in testPropMax
        30
        ("Multi epoch. Ada is preserved. " ++ show proof ++ " era. Trace length = " ++ show numTx)
        (traceProp proof numTx gensize propf)
