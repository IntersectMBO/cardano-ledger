module HSLedgerTest (module HSLedgerTest, module MAlonzo.Code.Ledger.Foreign.LedgerTypes, module MAlonzo.Code.Ledger.Foreign.HSLedger) where

import MAlonzo.Code.Ledger.Foreign.HSLedger
import MAlonzo.Code.Ledger.Foreign.LedgerTypes

initParams :: PParams
initParams = MkPParams
  {a = 1, b = 5, maxBlockSize = 1000, maxTxSize = 100, maxHeaderSize = 100, poolDeposit = 10, emax = 10, pv = (1, 0)}

initEnv :: UTxOEnv
initEnv = MkUTxOEnv {slot = 0, pparams = initParams}

a0 :: Addr
a0 = 0

a1 :: Addr
a1 = 1

a2 :: Addr
a2 = 2

initUTxO :: UTxO
initUTxO = [((0, 0), (a0, 1000))]

initState :: UTxOState
initState = MkUTxOState {utxo = initUTxO, fees = 0}

data SimpleTxBody = MkSimpleTxBody
  { stxins  :: [TxIn]
  , stxouts :: [(Ix, TxOut)]
  , stxvldt :: (Maybe Integer, Maybe Integer)
  , stxid   :: TxId }

bodyFromSimple :: PParams -> SimpleTxBody -> TxBody
bodyFromSimple pp stxb = let s = 5 in MkTxBody
  { txins  = stxins stxb
  , txouts = stxouts stxb
  , txfee  = (a pp) * s + (b pp)
  , txvldt = stxvldt stxb
  , txsize = s
  , txid   = stxid stxb }

testTxBody1 :: TxBody
testTxBody1 = bodyFromSimple initParams $ MkSimpleTxBody
  { stxins = [(0, 0)], stxouts = [(0, (a0, 890)), (1, (a1, 100))], stxvldt = (Nothing, Just 10), stxid = 1 }

testTx1 :: Tx
testTx1 = MkTx { body = testTxBody1, wits = MkTxWitnesses { vkSigs = [(0, 1)], scripts = [] }, txAD = Nothing }

testTxBody2 :: TxBody
testTxBody2 = bodyFromSimple initParams $ MkSimpleTxBody
  { stxins = [(1, 1)], stxouts = [(0, (a2, 10)), (1, (a1, 80))], stxvldt = (Nothing, Just 10), stxid = 2 }

testTx2 :: Tx
testTx2 = MkTx { body = testTxBody2, wits = MkTxWitnesses { vkSigs = [(1, 3)], scripts = [] }, txAD = Nothing }

runSteps :: (e -> s -> sig -> Maybe s) -> e -> s -> [sig] -> Maybe s
runSteps f e s []       = Just s
runSteps f e s (t : ts) = do
  s' <- f e s t
  runSteps f e s' ts

utxowSteps :: UTxOEnv -> UTxOState -> [Tx] -> Maybe UTxOState
utxowSteps = runSteps utxowStep

main :: IO ()
main = print $ utxowSteps initEnv initState [testTx1, testTx2]
