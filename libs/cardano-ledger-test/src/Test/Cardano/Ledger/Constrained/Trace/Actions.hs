module Test.Cardano.Ledger.Constrained.Trace.Actions where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (..), TxBody)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.Classes (TxOutF (..))
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (TraceM, updateVar)
import Test.Cardano.Ledger.Constrained.Vars (fees, utxo)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)

-- ====================================================================
-- Some experiments with updating the state (Stored in the Env)
-- Used as a means to track what applySTS does.

inputsAction :: Proof era -> Set (TxIn (EraCrypto era)) -> TraceM era ()
inputsAction proof is = updateVar (utxo proof) (\u -> Map.withoutKeys u is)

outputsAction :: Reflect era => Proof era -> TxBody era -> [TxOutF era] -> TraceM era ()
outputsAction proof txb outs = updateVar (utxo proof) (\u -> Map.union u (makemap outs))
  where
    makemap outPuts = Map.fromList (fmap (\(out, n) -> (mkTxInPartial txid n, out)) (zip outPuts [0 ..]))
    txid = TxId (hashAnnotated txb)

feesAction :: Era era => Coin -> TraceM era ()
feesAction feeCoin = updateVar fees (<+> feeCoin)
