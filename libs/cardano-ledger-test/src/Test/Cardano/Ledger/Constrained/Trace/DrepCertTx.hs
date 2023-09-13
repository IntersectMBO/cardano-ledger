{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate a Simple Tx with 1 inout, 1 output, and 1 DRep related Cert
module Test.Cardano.Ledger.Constrained.Trace.DrepCertTx where

import Cardano.Ledger.BaseTypes (Globals, TxIx)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core (Era (..), EraRule, EraTx (..), EraTxBody (..), EraTxOut (..), Script, Tx, TxBody, TxCert, Value, coinTxOutL)
import Cardano.Ledger.DRepDistr hiding (drepDeposit)
import Cardano.Ledger.Pretty (ppList)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Reader (Reader)
import Control.State.Transition.Extended (BaseM, Environment, PredicateFailure, STS, Signal, State)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast (RootTarget (..), runTarget, runTerm, (^$))
import Test.Cardano.Ledger.Constrained.Classes (
  PParamsF (..),
  ScriptF (..),
  TxBodyF (..),
  TxCertF (..),
  TxF (..),
  TxOutF (..),
  liftUTxO,
  unScriptF,
 )
import Test.Cardano.Ledger.Constrained.Combinators (itemFromSet)
import Test.Cardano.Ledger.Constrained.Env (Env (..), emptyEnv)
import Test.Cardano.Ledger.Constrained.Lenses (updateWithLens)
import Test.Cardano.Ledger.Constrained.Monad (Typed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Trace.Actions (certsAction, feesAction, inputsAction, outputsAction)
import Test.Cardano.Ledger.Constrained.Trace.Pipeline (Rule (..), sts)
import Test.Cardano.Ledger.Constrained.Trace.SimpleTx (completeTxBody, genLedgerStateEnv, getSTSLedgerEnv, plutusFreeCredential, simpleTxBody)
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (
  LedgerSTS,
  TraceM,
  TraceStep (..),
  beforeAfterTrace,
  fromSetTerm,
  getEnv,
  getTarget,
  getTerm,
  liftGen,
  liftTyped,
  reqSig,
  runTraceM,
  setVar,
  toGen,
 )
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), WitnessesField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (pcTx, pcTxCert)
import Test.Cardano.Ledger.Generic.Proof (TxCertWit (..), whichTxCert)
import Test.Cardano.Ledger.Generic.Proof hiding (LEDGER, lift)
import Test.Cardano.Ledger.Generic.Updaters (merge, newTx, newTxBody, newWitnesses)
import Test.QuickCheck (Gen, Property, arbitrary, conjoin, counterexample, generate, oneof, quickCheck, shuffle, whenFail, withMaxSuccess, (===))

-- =========================================================================

-- | Fix the first Outputs field in a [TxBodyField], by applying the delta Coin to the first Output in that Outputs field
--   This is used to compensate for certificates which make deposits, and this Coin must come from somewhere, so it is
--   added (subtracted if the Coin is negative) from the first TxOut.
fixOutput :: EraTxOut era => Coin -> [TxBodyField era] -> [TxBodyField era]
fixOutput _ [] = []
fixOutput delta (Outputs' (txout : more) : others) = (Outputs' (updateWithLens coinTxOutL (<+> delta) txout : more)) : others
fixOutput delta (x : xs) = x : fixOutput delta xs

-- | Compute a valid Cert, and the change in the stored Deposits from that Cert.
drepCert :: Reflect era => Proof era -> TraceM era (Coin, [TxBodyField era])
drepCert proof = case whichTxCert proof of
  TxCertShelleyToBabbage -> pure (mempty, [])
  TxCertConwayToConway -> do
    plutusmap <- getTerm plutusUniv
    drepCreds <- Set.filter (plutusFreeCredential plutusmap) <$> getTerm voteUniv
    (cred, _) <- liftGen (itemFromSet [] drepCreds)
    mdrepstate <- getTarget (Constr "mapMember" (Map.lookup cred) ^$ dreps)
    deposit@(Coin m) <- getTerm (drepDeposit proof)
    case mdrepstate of
      Nothing -> pure (Coin (-m), [Certs' [ConwayTxCertGov $ ConwayRegDRep cred deposit SNothing]])
      Just (DRepState _expiry _manchor _dep) ->
        liftGen $
          oneof
            [ pure (deposit, [Certs' [ConwayTxCertGov $ ConwayUnRegDRep cred deposit]])
            , do
                mAnchor <- arbitrary
                pure (Coin 0, [Certs' [ConwayTxCertGov $ ConwayUpdateDRep cred mAnchor]])
            ]

drepCertTx :: Reflect era => Coin -> Proof era -> TraceM era (TxF era)
drepCertTx maxFeeEstimate proof = do
  simplefields <- simpleTxBody proof maxFeeEstimate
  (deltadeposit, certfields) <- drepCert proof
  let txb = newTxBody proof (fixOutput deltadeposit simplefields ++ certfields)
  tx <- completeTxBody proof maxFeeEstimate txb
  setVar txterm (TxF proof tx)
  pure (TxF proof tx)

-- ============================================

genWithLEDGERinfo ::
  forall era.
  Reflect era =>
  Proof era ->
  (Proof era -> TraceM era (TxF era)) ->
  TraceM era (Env era, LedgerEnv era, LedgerState era, Tx era)
genWithLEDGERinfo proof txgen = do
  _ <- genLedgerStateEnv proof

  -- Compute the TRC before we make the Tx, because that adds things to the Env
  txIx <- liftGen arbitrary
  env0 <- getEnv
  (lenv, ledgerstate) <- liftTyped $ getSTSLedgerEnv proof txIx env0

  -- Now generate a Tx using the parameter function
  TxF _ tx <- txgen proof
  pure (env0, lenv, ledgerstate, tx)

-- \| Update the internal Env by applying the Actions
--   appropriate for a SimpleTx with DRepCerts.
--   Changes to the Env are done by side effects in the TraceM mondad.
applyDRepCertActions :: Reflect era => Proof era -> Tx era -> TraceM era ()
applyDRepCertActions proof tx = do
  let txb = tx ^. bodyTxL
      feeCoin = txb ^. feeTxBodyL
      txbcerts = txb ^. certsTxBodyL
  inputsAction proof (txb ^. inputsTxBodyL)
  outputsAction proof txb (fmap (TxOutF proof) (toList (txb ^. outputsTxBodyL)))
  feesAction feeCoin
  certsAction proof txbcerts

certsOf :: EraTx era => Tx era -> [TxCert era]
certsOf tx = toList (tx ^. (bodyTxL . certsTxBodyL))

showCerts proof certs = show (ppList (pcTxCert proof) certs)

test4 :: Gen Property
test4 = do
  let proof = Conway Standard
  (env0, lenv, lstate, tx) <- toGen $ genWithLEDGERinfo proof (drepCertTx (Coin 100000))
  case sts proof LEDGER lenv lstate tx of
    Left ps ->
      let errsLines = "" : "applySTS fails" : map show ps
       in pure $
            counterexample
              (unlines (errsLines ++ ["Tx =", show (pcTx proof tx)]))
              ( whenFail
                  ( putStrLn (unlines errsLines)
                      >> goRepl proof env0 ""
                      -- >> goRepl proof env0 ""
                  )
                  False
              )
    Right _ -> pure (True === True)

-- ================================================

-- | Generate a Tx that can be made into a Trace, because it applies the
--   necessary Actions to update the Env. The Env must contain the Vars that
--   are updated by 'applyDRepCertActions' . It is best to intialize the whole
--   LedgerState to do this.
drepCertTxForTrace :: Reflect era => Coin -> Proof era -> TraceM era (Tx era)
drepCertTxForTrace maxFeeEstimate proof = do
  (TxF _ tx) <- drepCertTx maxFeeEstimate proof
  applyDRepCertActions proof tx
  pure tx

-- | Make a TraceM computation that generates a Trace of SimpleTx with DRepCerts.
--   Note we run genLedgerStateEnv to get an Env with all the LedgerState Vars.
--   which need to be known at each step in the trace, and are updated at each step.
makeDRepCertTrace :: Reflect era => Int -> Proof era -> TraceM era [TraceStep era (Tx era)]
makeDRepCertTrace len proof = do
  env0 <- genLedgerStateEnv proof
  beforeAfterTrace len (\_ -> drepCertTxForTrace (Coin 100000) proof)

testDRepTrace = do
  let proof = Conway Standard
  trace <- toGen $ makeDRepCertTrace 200 proof
  txid <- arbitrary
  withMaxSuccess 100 <$> (conjoin <$> (monadTyped $ forM trace (passesLedger proof txid)))

go = do
  let proof = Conway Standard
  trace <- generate $ toGen $ makeDRepCertTrace 20 proof
  let f (TraceStep _before _after x) = putStrLn (showCerts proof (certsOf x))
  mapM_ f trace

passesLedger ::
  ( LedgerSTS era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Proof era ->
  TxIx ->
  TraceStep era (Tx era) ->
  Typed Property
passesLedger proof txIx (TraceStep before after tx) = do
  beforeLedgerState <- runTarget before (ledgerStateT proof)
  afterLedgerState <- runTarget after (ledgerStateT proof)
  slot <- runTerm before currentSlot
  (PParamsF _ pp) <- runTerm before (pparams proof)
  accntState <- runTarget before accountStateT
  afterLedgerState <- runTarget after (ledgerStateT proof)
  case sts proof LEDGER (LedgerEnv slot txIx pp accntState) beforeLedgerState tx of
    Right expected -> pure (afterLedgerState === expected)
    Left ps -> error (unlines (map show ps))
