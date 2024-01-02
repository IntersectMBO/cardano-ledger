{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Constrained.Trace.TraceMonad
where

import Cardano.Ledger.BaseTypes (Globals, SlotNo (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  DRepPulser (..),
  DRepPulsingState (..),
  PulsingSnapshot (..),
  newEpochStateDRepPulsingStateL,
 )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState (..))
import Cardano.Ledger.Shelley.Rules (runIdentity)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict hiding (State)
import Control.State.Transition.Extended (IRC (..), STS, TRC (..))
import Control.State.Transition.Trace (
  Trace (..),
  TraceOrder (OldestFirst),
  lastState,
  traceInitState,
  traceSignals,
  traceStates,
 )
import Control.State.Transition.Trace.Generator.QuickCheck (
  HasTrace (..),
  shrinkTrace,
  traceFromInitState,
 )
import Data.Default.Class (Default (def))
import Data.Foldable (toList)
import qualified Data.Foldable as Fold (toList)
import qualified Data.HashSet as HashSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as SS (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as Vector (length)
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast (
  Pred (..),
  RootTarget (..),
  Subst (..),
  Term (..),
  emptySubst,
  runTarget,
  runTerm,
  substPredWithVarTest,
  substToEnv,
 )
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap, itemFromSet, suchThatErr)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), runTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (
  dstateStage,
  pstateStage,
  vstateStage,
 )
import Test.Cardano.Ledger.Constrained.Preds.LedgerState (ledgerStateStage)
import Test.Cardano.Ledger.Constrained.Preds.NewEpochState (epochStateStage, newEpochStateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Universes (universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (
  DependGraph (..),
  OrderInfo,
  initialOrder,
  mkDependGraph,
  notBefore,
  rewriteGen,
 )
import Test.Cardano.Ledger.Constrained.Solver (solveOneVar)
import Test.Cardano.Ledger.Constrained.Vars (currentSlot, newEpochStateT)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockBlock (..), MockChainState (..))
import Test.Cardano.Ledger.Generic.PrettyCore (pcSlotNo, pcTx, ppList, ppPair, ppStrictSeq, summaryMapCompact)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Trace (chooseIssuer)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.Tasty.QuickCheck

-- ================================

data TraceState era = TraceState !Int !(Env era)

newtype TraceM era x = TraceM (StateT (TraceState era) (ExceptT [String] Gen) x)
  deriving (Functor, Applicative, Monad)

runTraceM :: Int -> Env era -> TraceM era a -> Gen (a, Int, Env era)
runTraceM n env (TraceM x) = do
  y <- runExceptT (runStateT x (TraceState n env))
  case y of
    Left xs -> error (unlines xs)
    Right (z, TraceState i s) -> pure (z, i, s)

fstTriple :: (a, b, c) -> a
fstTriple = (\(p, _, _) -> p)

toGen :: TraceM era a -> Gen a
toGen ax = fstTriple <$> runTraceM 0 emptyEnv ax

failTrace :: [String] -> TraceM era a
failTrace ss = TraceM (lift (throwE ss))

instance MonadFail (TraceM era) where
  fail err = failTrace [err]

-- ==============================
-- Lift a (Type era a) to (TraceM era a)

liftTyped :: Typed a -> TraceM era a
liftTyped x = do
  case runTyped x of
    Left ss -> failTrace ss
    Right a -> pure a

liftGen :: Gen a -> TraceM era a
liftGen g = TraceM (lift (lift g))

getEnv :: TraceM era (Env era)
getEnv = TraceM (state (\s@(TraceState _ !e) -> (e, s)))

getCount :: TraceM era Int
getCount = TraceM (state (\s@(TraceState !i _) -> (i, s)))

putEnv :: Env era -> TraceM era (Env era)
putEnv env = TraceM (state (\(TraceState i _) -> (env, TraceState i env)))

putCount :: Int -> TraceM era ()
putCount i = TraceM (state (\(TraceState _ !e) -> ((), TraceState i e)))

liftCounter :: ((Int, a) -> Gen (Int, b)) -> a -> TraceM era b
liftCounter f a = do
  !i <- getCount
  (!j, !b) <- liftGen (f (i, a))
  putCount j
  pure b

-- ========================

-- | Lookup the value of a Term in the Env internal to TraceM.
getTerm :: Term era a -> TraceM era a
getTerm term = do
  env <- getEnv
  case runTyped (runTerm env term) of
    Right a -> pure a
    Left ss -> failTrace (ss ++ ["in call to (getTerm " ++ show term ++ ")"])

getTarget :: RootTarget era r a -> TraceM era a
getTarget tar = do
  env <- getEnv
  case runTyped (runTarget env tar) of
    Right a -> pure a
    Left ss -> failTrace (ss ++ ["in call to (getTarget " ++ show tar ++ ")"])

-- | Pick a random (key,value) pair from a Map
fromMapTerm :: Term era (Map k a) -> TraceM era (k, a)
fromMapTerm term = do
  u <- getTerm term
  liftGen $ genFromMap ["fromMapTerm " ++ show term] u

-- | Pick a random (key,value) pair from a Map such that the (key,value) pair meets predicate 'p'
fromMapTermSuchThat :: Term era (Map k a) -> ((k, a) -> Bool) -> TraceM era (k, a)
fromMapTermSuchThat term p = do
  u <- getTerm term
  let n = Map.size u
  liftGen $
    suchThatErr
      ["suchThatErr call (fromMapTermSuchThat " ++ show term ++ " size = " ++ show n]
      (genFromMap ["fromMapTerm " ++ show term] u)
      p

-- | Pick a random element from a Set
fromSetTerm :: Term era (Set b) -> TraceM era b
fromSetTerm term = do
  u <- getTerm term
  liftGen (fst <$> (itemFromSet ["fromSetTerm " ++ show term] u))

-- | Update the Env internal to TraceM.
update :: (Env era -> TraceM era (Env era)) -> TraceM era ()
update f = getEnv >>= f >>= putEnv >> pure ()

-- | Update the value of one variable stored in the Env internal to TraceM.
updateVar :: Term era t -> (t -> t) -> TraceM era ()
updateVar term@(Var v) adjust = TraceM $ do
  TraceState i env <- get
  case runTyped (runTerm env term) of
    Right valA -> put (TraceState i (storeVar v (adjust valA) env))
    Left ss -> lift (throwE (ss ++ ["in call to 'updateVar'"]))
updateVar term _ = failTrace ["Non Var term in call to 'updateVar'", show term]

setVar :: Term era t -> t -> TraceM era ()
setVar (Var v) t = TraceM $ do
  TraceState i env <- get
  put (TraceState i (storeVar v t env))
setVar term _ = failTrace ["Non Var term in call to 'setVar'", show term]

-- ============================================================================
-- A few helper functions that need to do different things in different Eras.

refInputs :: Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
refInputs (Shelley _) _ = Set.empty
refInputs (Allegra _) _ = Set.empty
refInputs (Mary _) _ = Set.empty
refInputs (Alonzo _) _ = Set.empty
refInputs (Babbage _) txb = txb ^. referenceInputsTxBodyL
refInputs (Conway _) txb = txb ^. referenceInputsTxBodyL

reqSig :: Proof era -> TxBody era -> Set (KeyHash 'Witness (EraCrypto era))
reqSig (Shelley _) _ = Set.empty
reqSig (Allegra _) _ = Set.empty
reqSig (Mary _) _ = Set.empty
reqSig (Alonzo _) _ = Set.empty
reqSig (Babbage _) txb = txb ^. reqSignerHashesTxBodyL
reqSig (Conway _) txb = txb ^. reqSignerHashesTxBodyL

-- =======================================================================================
-- Code to run the standard [Pred era] found in the Preds directory in the TraceM monad
-- First code to compile in the TraceM monad, and then use the compiled version to run
-- the solver in the TraceM monad

-- | Compile [Pred era] in the TraceM monad
--   First: Apply the Rewriter
--   Second: appy the Subst
--   Third: Construct the DependGraph
compileTraceWithSubst :: Era era => OrderInfo -> Subst era -> [Pred era] -> TraceM era (DependGraph era)
compileTraceWithSubst info subst0 cs0 = do
  simple <- liftCounter rewriteGen cs0
  graph <- liftTyped $ do
    let instanPreds = fmap (substPredWithVarTest subst0) simple
    orderedNames <- initialOrder info instanPreds
    mkDependGraph (length orderedNames) [] HashSet.empty orderedNames [] (Prelude.filter notBefore instanPreds)
  pure graph

-- | Use the tool chain to generate a Subst from a list of Pred, in the TraceM monad.
toolChainTrace :: Era era => Proof era -> OrderInfo -> [Pred era] -> Subst era -> TraceM era (Subst era)
toolChainTrace _proof order cs subst0 = do
  (DependGraph pairs) <- compileTraceWithSubst order subst0 cs
  Subst subst <- liftGen (foldlM' solveOneVar subst0 pairs)
  let isTempV k = not (elem '.' k)
  pure $ (Subst (Map.filterWithKey (\k _ -> isTempV k) subst))

-- =====================================================================
-- Lift some [Pred era] into TraceM (monadic) (Subst era) transformers
-- Create tool (TraceM era (Subst era)) functions that can be chained together

universeTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
universeTrace proof subst = liftGen (universeStage def proof subst)

pparamsTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
pparamsTrace proof subst = liftGen (pParamsStage proof subst)

pstateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
pstateTrace proof subst = liftGen (pstateStage proof subst)

vstateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
vstateTrace proof subst = liftGen (vstateStage proof subst)

dstateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
dstateTrace proof subst = liftGen (dstateStage proof subst)

ledgerStateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
ledgerStateTrace proof subst = liftGen (ledgerStateStage def proof subst)

epochStateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
epochStateTrace proof subst = liftGen (epochStateStage proof subst)

newEpochStateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
newEpochStateTrace proof subst = liftGen (newEpochStateStage proof subst)

-- ==============================================================
-- Code to make Traces

-- | Iterate a function 'make' to make a trace of length 'n'. Each call to 'make' gets the
--   most recent value of the Env internal to TraceM. The function 'make' is
--   supposed to compute 'a', and (possibly) update the Env internal to TraceM.
makeTrace :: Int -> TraceM era a -> TraceM era [(Env era, a)]
makeTrace 0 _ = pure []
makeTrace n make = do
  env0 <- getEnv
  a <- make
  xs <- makeTrace (n - 1) make
  pure ((env0, a) : xs)

data TraceStep era a = TraceStep {tsBefore :: !(Env era), tsAfter :: !(Env era), tsA :: !a}

beforeAfterTrace :: Int -> (Int -> TraceM era a) -> TraceM era [TraceStep era a]
beforeAfterTrace 0 _ = pure []
beforeAfterTrace !n make = do
  !beforeEnv <- getEnv
  !a <- make n
  !afterEnv <- getEnv
  xs <- beforeAfterTrace (n - 1) make
  let !ans = TraceStep {tsBefore = beforeEnv, tsAfter = afterEnv, tsA = a} : xs
  pure ans

-- =====================================================================

-- | Generate an Env that contains the pieces of the LedgerState
--   by chaining smaller pieces together.
genLedgerStateEnv :: Reflect era => Proof era -> TraceM era (Env era)
genLedgerStateEnv proof = do
  subst <-
    pure emptySubst
      >>= pparamsTrace proof
      >>= universeTrace proof
      >>= vstateTrace proof
      >>= pstateTrace proof
      >>= dstateTrace proof
      >>= ledgerStateTrace proof
  env <- liftTyped (substToEnv subst emptyEnv)
  putEnv env

-- | Generate an Env that contains the pieces of the NewEpochState
--   by chaining smaller pieces together.
genNewEpochStateEnv :: Reflect era => Proof era -> TraceM era (Env era)
genNewEpochStateEnv proof = do
  subst <-
    pure emptySubst
      >>= pparamsTrace proof
      >>= universeTrace proof
      >>= vstateTrace proof
      >>= pstateTrace proof
      >>= dstateTrace proof
      >>= ledgerStateTrace proof
      >>= epochStateTrace proof
      >>= newEpochStateTrace proof
  env <- liftTyped (substToEnv subst emptyEnv)
  putEnv env

-- ======================================================================
-- Using traces by running the MOCKCHAIN STS instance
-- ======================================================================

-- | How we encode a trace, when we run STS of (MOCKCHAIN era)
data PredGen era = PredGen (Vector (StrictSeq (Tx era), SlotNo)) (Env era)

instance Reflect era => Show (PredGen era) where
  show (PredGen xs _) = show (ppList (ppPair (ppStrictSeq (pcTx reify)) (pcSlotNo)) (toList xs))

instance
  ( STS (MOCKCHAIN era)
  , Reflect era
  ) =>
  HasTrace (MOCKCHAIN era) (PredGen era)
  where
  type BaseEnv (MOCKCHAIN era) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals

  envGen _gstate = pure ()

  sigGen (PredGen txss _) () mcs@(MockChainState newepoch _ (SlotNo lastSlot) count) = do
    let NewEpochState epochnum _ _ _epochstate _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer epochnum lastSlot count pooldistr
    let (txs, nextSlotNo) = txss ! count
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal
    case runShelleyBase (applySTSTest (TRC @(MOCKCHAIN era) ((), mcs, mockblock))) of
      Left pdfs ->
        let _txsl = Fold.toList txs
         in error ("FAILS\n" ++ unlines (map show pdfs))
      Right mcs2 -> seq mcs2 (pure mockblock)

  shrinkSignal _ = []

-- ==================================================
-- Now to run a Property test we need two things
-- 1) An initial (MockChainState newepochstate tickNes slot count)
-- 2) A (PredGen blocks env)
-- We need to generate these together so that the trace is valid, i.e. it passes all the STS rules

genTraceParts ::
  Reflect era =>
  Proof era ->
  Int ->
  (Proof era -> TraceM era (Tx era)) ->
  TraceM era ([TraceStep era (Tx era)], Maybe (IRC (MOCKCHAIN era) -> Gen (Either a (MockChainState era))), PredGen era)
genTraceParts proof len genTx = do
  env0 <- genNewEpochStateEnv proof
  newepochstate <- liftTyped $ runTarget env0 (newEpochStateT proof)
  SlotNo currslot <- liftTyped $ runTerm env0 currentSlot
  let initStateFun = Just (\(IRC ()) -> pure (Right (MockChainState newepochstate newepochstate (SlotNo currslot) 0)))
  steps <- beforeAfterTrace len (\_ -> genTx proof)
  let getTx (TraceStep _ _ x) = x
  zs <- traceStepToVector getTx steps (currslot + 1) []
  pure (steps, initStateFun, PredGen zs env0)

traceStepToVector ::
  (step -> Tx era) ->
  [step] ->
  Word64 ->
  [([Tx era], SlotNo)] ->
  TraceM era (Vector (StrictSeq (Tx era), SlotNo))
traceStepToVector _ [] _ zs = pure (fromList (reverse (map (\(x, y) -> (SS.fromList x, y)) zs)))
traceStepToVector getTx steps m prev = do
  n <- liftGen $ choose (1, 2)
  let steplist = take n steps
  nextslot <- liftGen ((+ m) <$> choose (2, 4))
  traceStepToVector getTx (drop n steps) nextslot ((map getTx steplist, SlotNo m) : prev)

-- | Generate a Control.State.Transition.Trace(Trace) from a (TraceM era (Tx era))
newStsTrace ::
  ( Reflect era
  , STS (MOCKCHAIN era)
  ) =>
  Proof era ->
  Int ->
  (Proof era -> TraceM era (Tx era)) ->
  Gen (Trace (MOCKCHAIN era))
newStsTrace proof len genTx = do
  (_, initfun, predgen@(PredGen zs _)) <- toGen $ genTraceParts proof len genTx
  traceFromInitState testGlobals (fromIntegral (Vector.length zs - 1)) predgen initfun

-- | Create a testable Property from two things
--   1) A function that generates Tx
--   2) A function that creates a property from a (Trace (MOCKCHAIN era))
mockChainProp ::
  forall era.
  (Reflect era, STS (MOCKCHAIN era)) =>
  Proof era ->
  Int ->
  (Proof era -> TraceM era (Tx era)) ->
  (Trace (MOCKCHAIN era) -> Property) ->
  Property
mockChainProp proof n genTxAndUpdateEnv propf = do
  let genTrace = newStsTrace proof n genTxAndUpdateEnv
  forAllShrinkBlind
    genTrace
    ( if False
        then shrinkTrace @(MOCKCHAIN era) @(PredGen era) testGlobals
        else (\_ -> [])
    )
    propf

-- ===========================================================================

-- | Given trace [(sig0,state0),(sig1,state1),(sig2,state2),(sig3,state3)]
--   conjoin [p pstate0 sig1 state1, p state0 sig2 state2, p state0 sig3 state3]
-- test that 'p' holds between state0 and stateN for all N
stepProp :: (MockChainState era -> MockBlock era -> MockChainState era -> Property) -> (Trace (MOCKCHAIN era) -> Property)
stepProp p tr = case zipWith (p state0) sigs states of
  [] -> property True
  (_ : more) -> conjoin more
  where
    state0 = tr ^. traceInitState
    states = traceStates OldestFirst tr
    sigs = traceSignals OldestFirst tr

-- | Given trace [(sig0,state0),(sig1,state1),(sig2,state2),(sig3,state3)]
--   conjoin [p pstate0 sig1 state1, p state1 sig2 state2, p state2 sig3 state3]
--   test that 'p' holds bteween (state0 stateN sig(N+1) state(N+1) for all N
deltaProp :: (MockChainState era -> MockBlock era -> MockChainState era -> Property) -> Trace (MOCKCHAIN era) -> Property
deltaProp p tr = case states of
  [] -> property True
  _ : more -> conjoin (zipWith3 p states sigs more)
  where
    states = traceStates OldestFirst tr
    sigs = traceSignals OldestFirst tr

-- | Given trace [(sig0,state0),(sig1,state1),(sig2,state2),(sig3,state3)]
--   test that (p state0 stateN),  where stateN is the last state in the trace
preserveProp :: (MockChainState era -> MockChainState era -> Property) -> (Trace (MOCKCHAIN era) -> Property)
preserveProp p tr = p state0 stateN
  where
    state0 = tr ^. traceInitState
    stateN = lastState tr

-- | Apply '(preserveProp p)' to each full Epoch in a Trace. A partial epoch at the end of the trace is not tested.
epochProp :: ConwayEraGov era => (MockChainState era -> MockChainState era -> Property) -> (Trace (MOCKCHAIN era) -> Property)
epochProp p tr = counterexample message $ conjoin (map (\(epoch, _n) -> (p (head epoch) (last epoch))) epochs)
  where
    states = traceStates OldestFirst tr
    epochs = zip (splitEpochs states) [0 :: Int ..]
    showEpoch (epoch, n) = unlines (("EPOCH " ++ show n) : map showPulserState epoch)
    message = "Trace length " ++ show (length states) ++ unlines (map showEpoch epochs)

splitE :: (a -> a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
splitE _ [] epoch epochs = reverse (reverse epoch : epochs)
splitE _ [x] epoch epochs = reverse (reverse (x : epoch) : epochs)
splitE boundary (x : y : more) epoch epochs =
  if boundary x y
    then splitE boundary (y : more) [] (reverse (x : epoch) : epochs)
    else splitE boundary (y : more) (x : epoch) epochs

splitTest :: [[(Int, Char)]]
splitTest = splitE boundary (zip [1, 1, 1, 2, 2, 1, 3, 3, 3, 3, 4, 4, 4, 5, 6, 6, 6, 8, 8] "abcdefghijklmnopqrstuvwxyz") [] []
  where
    boundary :: (Int, Char) -> (Int, Char) -> Bool
    boundary (n, _) (m, _) = n + 1 <= m

splitEpochs :: [MockChainState era] -> [[MockChainState era]]
splitEpochs xs = init $ splitE boundary xs [] [] -- We only want full traces, so we drop the last potential trace
  where
    boundary mcs1 mcs2 = nesEL (mcsNes mcs1) + 1 <= nesEL (mcsNes mcs2)

-- =====================

showPulserState :: ConwayEraGov era => MockChainState era -> String
showPulserState (MockChainState nes _ slot _) = "Slot = " ++ show slot ++ "   " ++ getPulserInfo (nes ^. newEpochStateDRepPulsingStateL)
  where
    getPulserInfo (DRPulsing x) = "Balance = " ++ show (summaryMapCompact (dpBalance x)) ++ "    DRepDistr = " ++ show (summaryMapCompact (dpDRepDistr x))
    getPulserInfo (DRComplete psnap _) = "Complete DRepDistr = " ++ show (summaryMapCompact (psDRepDistr psnap))
