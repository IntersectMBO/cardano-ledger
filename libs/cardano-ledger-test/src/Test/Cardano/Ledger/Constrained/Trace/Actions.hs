{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Trace.Actions where

import Cardano.Ledger.BaseTypes (addEpochInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core
import Cardano.Ledger.DRep (DRepState (DRepState))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val (Val (..))
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (TraceM, getTerm, updateVar)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Generic.Proof

-- ====================================================================
-- Some experiments with updating the state (Stored in the Env)
-- Used as a means to track what applySTS does.

inputsAction :: (EraTest era, Reflect era) => Proof era -> Set TxIn -> TraceM era ()
inputsAction proof is = updateVar (utxo proof) (\u -> Map.withoutKeys u is)

outputsAction ::
  (EraTest era, Reflect era) => Proof era -> TxBody era -> [TxOutF era] -> TraceM era ()
outputsAction proof txb outs = updateVar (utxo proof) (\u -> Map.union u (makemap outs))
  where
    makemap outPuts = Map.fromList (fmap (\(out, n) -> (mkTxInPartial txid n, out)) (zip outPuts [0 ..]))
    txid = TxId (hashAnnotated txb)

feesAction :: (EraTest era, Reflect era) => Coin -> TraceM era ()
feesAction feeCoin = updateVar fees (<+> feeCoin)

certAction :: Era era => Proof era -> TxCert era -> TraceM era ()
certAction p@Conway cert =
  case cert of
    ConwayTxCertGov (ConwayRegDRep cred _ manchor) -> do
      epoch <- getTerm currentEpoch
      activity <- getTerm (drepActivity p)
      dep <- getTerm (drepDeposit p)
      updateVar
        currentDRepState
        (Map.insert cred (DRepState (addEpochInterval epoch activity) manchor dep mempty))
    ConwayTxCertGov (ConwayUnRegDRep cred dep) -> do
      updateVar currentDRepState (Map.delete cred)
      updateVar deposits (<-> dep)
    ConwayTxCertGov (ConwayUpdateDRep cred mAnchor) -> do
      epoch <- getTerm currentEpoch
      activity <- getTerm (drepActivity p)
      updateVar
        currentDRepState
        ( Map.adjust
            ( \(DRepState _ _ deposit delegs) -> DRepState (addEpochInterval epoch activity) mAnchor deposit delegs
            )
            cred
        )
    _ -> pure ()
certAction _ _ = pure ()

certsAction :: (Foldable t, Era era) => Proof era -> t (TxCert era) -> TraceM era ()
certsAction p xs = forM_ xs (certAction p)
