module Mutator
    (
      mutateId
    , mutateNat
    , mutateNat'
    , mutateCerts
    , mutateCoin
    , mutateCoin'
    , mutateLedgerEntry
    , mutateTx
    ) where

import Data.Set                  as Set
import Numeric.Natural

import Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range

import Coin
import LedgerState (LedgerEntry(..))
import UTxO        (Tx(..), TxWits(..), TxIn(..), TxOut(..))

mutateId :: a -> Gen a
mutateId = pure

mutateNatRange :: Natural -> Natural -> Natural -> Gen Natural
mutateNatRange lower upper _ = Gen.integral $ Range.linear lower upper

mutateNatSmall :: Natural -> Gen Natural
mutateNatSmall n = do
  b <- Gen.enumBounded :: Gen Bool
  pure $ if b then n + 1 else (if n > 0 then n - 1 else 0)

mutateNat' :: Natural -> Natural -> Natural -> Gen Natural
mutateNat' lower upper n = do
  n' <- Gen.choice [mutateNatRange lower upper n, mutateNatSmall n]
  if n == n' then Gen.discard else pure n'

mutateNat :: Natural -> Natural -> Natural -> Gen Natural
mutateNat lower upper n =
  Gen.choice [mutateId n, mutateNatRange lower upper n, mutateNatSmall n]

mutateCoin :: Natural -> Natural -> Coin -> Gen Coin
mutateCoin lower upper (Coin val) = Coin <$> mutateNat lower upper val

mutateCoin' :: Natural -> Natural -> Coin -> Gen Coin
mutateCoin' lower upper (Coin val) = Coin <$> mutateNat' lower upper val

mutateLedgerEntry :: LedgerEntry -> Gen LedgerEntry
mutateLedgerEntry d@(DelegationData _) = mutateId d
mutateLedgerEntry (TransactionData txwits) = do
  body' <- mutateTx $ body txwits
  pure $ TransactionData $ TxWits body' (witnessSet txwits)

mutateTx :: Tx -> Gen Tx
mutateTx tx = do
  inputs'  <- mutateInputs $ Set.toList (inputs tx)
  outputs' <- mutateOutputs $ outputs tx
  certs'   <- mutateCerts   $ certs tx
  pure $ Tx (Set.fromList inputs')
            outputs'
            certs'

mutateInputs :: [TxIn] -> Gen [TxIn]
mutateInputs = minp

minp :: [TxIn] -> Gen [TxIn]
minp [] = pure []
minp (txin:txins) = do
  mtxin      <- mutateInput txin
  mtxins     <- minp txins
  dropTxin   <- Gen.enumBounded
  pure $ if dropTxin then mtxins else mtxin:mtxins

mutateInput :: TxIn -> Gen TxIn
mutateInput (TxIn idx index) = do
  index' <- mutateNat 0 100 index
  pure $ TxIn idx index'

mutateOutputs :: [TxOut] -> Gen [TxOut]
mutateOutputs = mout

mout :: [TxOut] -> Gen [TxOut]
mout [] = pure []
mout (txout:txouts) = do
  mtxout    <- mutateOutput txout
  mtxouts   <- mout txouts
  dropTxOut <- Gen.enumBounded
  pure $ if dropTxOut then mtxouts else mtxout:mtxouts

mutateOutput :: TxOut -> Gen TxOut
mutateOutput (TxOut addr c) = do
  c' <- mutateCoin 0 100 c
  pure $ TxOut addr c'

mutateCerts = mutateId
