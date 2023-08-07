{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Cardano.Ledger.Constrained.Trace.TraceMonad
where

import Cardano.Ledger.Babbage.TxBody (referenceInputsTxBodyL, reqSignerHashesTxBodyL)
import Cardano.Ledger.Core (Era (..), TxBody)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Default.Class (Default (def))
import qualified Data.HashSet as HashSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast (
  Pred (..),
  RootTarget (..),
  Subst (..),
  Term (..),
  runTarget,
  runTerm,
  substPredWithVarTest,
 )
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap, itemFromSet, suchThatErr)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), runTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (certStatePreds, pstatePreds, vstatePreds)
import Test.Cardano.Ledger.Constrained.Preds.LedgerState (ledgerStatePreds)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsPreds)
import Test.Cardano.Ledger.Constrained.Preds.Universes (universePreds)
import Test.Cardano.Ledger.Constrained.Rewrite (
  DependGraph (..),
  OrderInfo,
  initialOrder,
  mkDependGraph,
  notBefore,
  rewriteGen,
  standardOrderInfo,
 )
import Test.Cardano.Ledger.Constrained.Solver (solveOneVar)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.QuickCheck (Gen)

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
universeTrace proof subst = toolChainTrace proof standardOrderInfo (universePreds def proof) subst

pparamsTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
pparamsTrace proof subst = toolChainTrace proof standardOrderInfo (pParamsPreds proof) subst

pstateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
pstateTrace proof subst = toolChainTrace proof standardOrderInfo (pstatePreds proof) subst

vstateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
vstateTrace proof subst = toolChainTrace proof standardOrderInfo (vstatePreds proof) subst

certStateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
certStateTrace proof subst = toolChainTrace proof standardOrderInfo (certStatePreds proof) subst

ledgerStateTrace :: Reflect era => Proof era -> Subst era -> TraceM era (Subst era)
ledgerStateTrace proof subst = toolChainTrace proof standardOrderInfo (ledgerStatePreds def proof) subst
