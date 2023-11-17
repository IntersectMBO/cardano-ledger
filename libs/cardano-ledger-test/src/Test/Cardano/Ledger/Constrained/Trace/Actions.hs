{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Trace.Actions where

import Cardano.Ledger.BaseTypes (addEpochInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core (Era (..), TxBody, TxCert)
import Cardano.Ledger.DRep (DRepState (DRepState))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val (Val (..))
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.Classes (TxOutF (..))
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (TraceM, getTerm, updateVar)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Proof hiding (lift)

-- ====================================================================
-- Some experiments with updating the state (Stored in the Env)
-- Used as a means to track what applySTS does.

inputsAction :: Era era => Proof era -> Set (TxIn (EraCrypto era)) -> TraceM era ()
inputsAction proof is = updateVar (utxo proof) (\u -> Map.withoutKeys u is)

outputsAction :: Reflect era => Proof era -> TxBody era -> [TxOutF era] -> TraceM era ()
outputsAction proof txb outs = updateVar (utxo proof) (\u -> Map.union u (makemap outs))
  where
    makemap outPuts = Map.fromList (fmap (\(out, n) -> (mkTxInPartial txid n, out)) (zip outPuts [0 ..]))
    txid = TxId (hashAnnotated txb)

feesAction :: Era era => Coin -> TraceM era ()
feesAction feeCoin = updateVar fees (<+> feeCoin)

certAction :: Era era => Proof era -> TxCert era -> TraceM era ()
certAction p@(Conway _) cert =
  case cert of
    ConwayTxCertGov (ConwayRegDRep cred _ manchor) -> do
      epoch <- getTerm currentEpoch
      activity <- getTerm (drepActivity p)
      dep <- getTerm (drepDeposit p)
      updateVar currentDRepState (Map.insert cred (DRepState (addEpochInterval epoch activity) manchor dep))
    ConwayTxCertGov (ConwayUnRegDRep cred dep) -> do
      updateVar currentDRepState (Map.delete cred)
      updateVar deposits (<-> dep)
    ConwayTxCertGov (ConwayUpdateDRep cred mAnchor) -> do
      epoch <- getTerm currentEpoch
      activity <- getTerm (drepActivity p)
      updateVar currentDRepState (Map.adjust (\(DRepState _ _ deposit) -> DRepState (addEpochInterval epoch activity) mAnchor deposit) cred)
    _ -> pure ()
certAction _ _ = pure ()

certsAction :: (Foldable t, Era era) => Proof era -> t (TxCert era) -> TraceM era ()
certsAction p xs = forM_ xs (certAction p)
