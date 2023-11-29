{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate a Simple Tx with 1 inout, 1 output, and 1 DRep related Cert
module Test.Cardano.Ledger.Constrained.Trace.DrepCertTx where

import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  PulsingSnapshot (..),
  computeDrepDistr,
  finishDRepPulser,
  newEpochStateDRepPulsingStateL,
  utxosGovStateL,
 )
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core (
  Era (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  Tx,
  TxCert,
  coinTxOutL,
 )
import Cardano.Ledger.DRep hiding (drepDeposit)
import Cardano.Ledger.Pretty (ppList)
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  IncrementalStake (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  VState (..),
  allObligations,
 )
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast (RootTarget (..), (^$))
import Test.Cardano.Ledger.Constrained.Classes (TxF (..), TxOutF (..))
import Test.Cardano.Ledger.Constrained.Combinators (itemFromSet)
import Test.Cardano.Ledger.Constrained.Preds.Tx (hashBody)
import Test.Cardano.Ledger.Constrained.Trace.Actions (certsAction, feesAction, inputsAction, outputsAction)
import Test.Cardano.Ledger.Constrained.Trace.SimpleTx (completeTxBody, plutusFreeCredential, simpleTxBody)
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (
  TraceM,
  epochProp,
  getTarget,
  getTerm,
  liftGen,
  mockChainProp,
  setVar,
  showPulserState,
  stepProp,
 )
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..))
import Test.Cardano.Ledger.Generic.MockChain (MockBlock (..), MockChainState (..))
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxCert)
import Test.Cardano.Ledger.Generic.Proof (
  ConwayEra,
  Evidence (..),
  Proof (..),
  Reflect (..),
  StandardCrypto,
  TxCertWit (..),
  whichTxCert,
 )
import Test.Cardano.Ledger.Generic.Updaters (newTxBody)
import Test.Tasty
import Test.Tasty.QuickCheck

import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Hashes (EraIndependentTxBody)

-- =========================================================================

-- | Fix the first Outputs field in a [TxBodyField], by applying the delta Coin to the first Output in that Outputs field
--   This is used to compensate for certificates which make deposits, and this Coin must come from somewhere, so it is
--   added (subtracted if the Coin is negative) from the first TxOut.
fixOutput :: EraTxOut era => Coin -> [TxBodyField era] -> [TxBodyField era]
fixOutput _ [] = []
fixOutput delta (Outputs' (txout : more) : others) =
  (Outputs' ((txout & coinTxOutL <>~ delta) : more)) : others
fixOutput delta (x : xs) = x : fixOutput delta xs

-- | Compute a valid Cert, and the change in the stored Deposits from that Cert.
drepCert :: Reflect era => Proof era -> TraceM era (Coin, [TxBodyField era])
drepCert proof = case whichTxCert proof of
  TxCertShelleyToBabbage -> pure (mempty, [])
  TxCertConwayToConway -> do
    plutusmap <- getTerm plutusUniv
    drepCreds <- Set.filter (plutusFreeCredential plutusmap) <$> getTerm voteUniv
    (cred, _) <- liftGen (itemFromSet [] drepCreds)
    mdrepstate <- getTarget (Constr "mapMember" (Map.lookup cred) ^$ currentDRepState)
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

-- | Update the internal Env by applying the Actions
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

-- ======================================

conway :: Proof (ConwayEra StandardCrypto)
conway = Conway Standard

drepTree :: TestTree
drepTree =
  testGroup
    "DRep property traces"
    [ testProperty
        "Watch pulser on traces of length 120."
        (withMaxSuccess 2 $ mockChainProp (Conway Standard) 120 (drepCertTxForTrace (Coin 100000)) (stepProp watchPulser))
    , testProperty
        "All Tx are valid on traces of length 150."
        (withMaxSuccess 2 $ mockChainProp (Conway Standard) 150 (drepCertTxForTrace (Coin 100000)) (stepProp allValidSignals))
    , testProperty
        "Bruteforce = Pulsed, in every epoch, on traces of length 150"
        (withMaxSuccess 5 $ mockChainProp (Conway Standard) 150 (drepCertTxForTrace (Coin 60000)) (epochProp pulserWorks))
    ]

main :: IO ()
main = defaultMain drepTree

-- =================================================
-- Example functions that can be lifted to
-- Trace (MOCKCHAIN era) -> Property
-- using 'stepProp', 'deltaProp', 'preserveProp', and 'epochProp'
-- this lifted value is then passed to 'mockChainProp' to make a Property

allValidSignals :: MockChainState era -> MockBlock era -> MockChainState era -> Property
allValidSignals _state0 (MockBlock _ _ _txs) _stateN = (property True)

watchPulser :: ConwayEraGov era => MockChainState era -> MockBlock era -> MockChainState era -> Property
watchPulser _state0 (MockBlock _ _ _txs) stateN = trace (showPulserState stateN) (property True)

pulserWorks :: ConwayEraGov era => MockChainState era -> MockChainState era -> Property
pulserWorks mcsfirst mcslast =
  counterexample
    ( "\nFirst "
        ++ showPulserState mcsfirst
        ++ "\nLast "
        ++ showPulserState mcslast
    )
    (bruteForceDRepDistr (mcsTickNes mcsfirst) === extractPulsingDRepDistr (mcsNes mcslast))

bruteForceDRepDistr :: NewEpochState era -> Map.Map (DRep (EraCrypto era)) (CompactForm Coin)
bruteForceDRepDistr nes = computeDrepDistr umap dreps incstk
  where
    ls = (esLState . nesEs) nes
    cs = lsCertState ls
    IStake incstk _ = (utxosStakeDistr . lsUTxOState) ls
    umap = dsUnified (certDState cs)
    dreps = vsDReps (certVState cs)

extractPulsingDRepDistr :: ConwayEraGov era => NewEpochState era -> Map.Map (DRep (EraCrypto era)) (CompactForm Coin)
extractPulsingDRepDistr nes = (psDRepDistr . fst . finishDRepPulser) (nes ^. newEpochStateDRepPulsingStateL)

-- ===============================================
-- helper functions

showCerts :: Proof era -> [TxCert era] -> String
showCerts proof cs = show (ppList (pcTxCert proof) cs)

certsOf :: EraTx era => Tx era -> [TxCert era]
certsOf tx = toList (tx ^. (bodyTxL . certsTxBodyL))

hashTx :: EraTx era => Proof era -> Tx era -> Hash (HASH (EraCrypto era)) EraIndependentTxBody
hashTx proof tx = hashBody proof (tx ^. bodyTxL)

showMap :: (Show k, Show v) => String -> Map.Map k v -> String
showMap msg m = unlines (msg : map show (Map.toList m))

traceMap :: (Show k, Show v) => String -> Map.Map k v -> a -> a
traceMap s m x = trace (showMap s m) x

showPotObl :: ConwayEraGov era => NewEpochState era -> String
showPotObl nes = "\nPOT " ++ show (utxosDeposited us) ++ "\nAll " ++ show (allObligations certSt (us ^. utxosGovStateL))
  where
    es = nesEs nes
    ls = esLState es
    us = lsUTxOState ls
    certSt = lsCertState ls
