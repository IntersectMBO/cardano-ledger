{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module
-- Description : Generators for mutating data.
--
-- This module implements a mutation based approach for generating invalid
-- data. Input values are mutated depending on their value.
module Test.Shelley.Spec.Ledger.NonTraceProperties.Mutator
  ( mutateNat,
    mutateCoin,
    mutateTx,
    mutateTxBody,
    mutateDCert,
    getAnyStakeKey,
  )
where

import qualified Data.List as List (map)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.Keys (KeyRole (..), coerceKeyRole, hashKey, vKey)
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
  ( _body,
    _certs,
    _inputs,
    _outputs,
    _ttl,
    _txfee,
    _wdrls,
    _witnessMSigMap,
    _witnessVKeySet,
    pattern Tx,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.TxData
  ( PoolParams (..),
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertMir,
    pattern DCertPool,
    pattern Delegation,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes

-- | Identity mutator that does not change the input value.
mutateId :: a -> Gen a
mutateId = pure

-- | Mutator that ignores the input value and returns any value in the range
-- 'lower'..'upper'.
mutateNatRange :: Natural -> Natural -> Natural -> Gen Natural
mutateNatRange lower upper _ = Gen.integral $ Range.linear lower upper

-- | Mutator that adds or subtracts 1 from a given input value. In the case of
-- underflow it returns 0.
mutateNatSmall :: Natural -> Gen Natural
mutateNatSmall n = do
  b <- Gen.enumBounded :: Gen Bool
  pure $ if b then n + 1 else (if n > 0 then n - 1 else 0)

-- | Mutator that combines the identity, range selector and small change mutator
-- in a random choice.
mutateNat :: Natural -> Natural -> Natural -> Gen Natural
mutateNat lower upper n =
  Gen.choice [mutateId n, mutateNatRange lower upper n, mutateNatSmall n]

-- | Mutator for 'Coin' values, based on mutation of the contained value field.
mutateCoin :: Natural -> Natural -> Coin -> Gen Coin
mutateCoin lower upper (Coin val) =
  Coin . fromIntegral <$> mutateNat lower upper (fromIntegral val)

-- | Mutator of 'Tx' which mutates the contained transaction
mutateTx :: Tx -> Gen Tx
mutateTx txwits = do
  body' <- mutateTxBody $ _body txwits
  pure $ Tx body' (_witnessVKeySet txwits) (_witnessMSigMap txwits) SNothing

-- | Mutator for Transaction which mutates the set of inputs and the set of
-- unspent outputs.
mutateTxBody :: TxBody -> Gen TxBody
mutateTxBody tx = do
  inputs' <- mutateInputs $ Set.toList (_inputs tx)
  outputs' <- mutateOutputs $ _outputs tx
  pure $
    TxBody
      (Set.fromList inputs')
      outputs'
      (_certs tx)
      (_wdrls tx)
      (_txfee tx)
      (_ttl tx)
      SNothing
      SNothing

-- | Mutator for a list of 'TxIn'.
mutateInputs :: [TxIn] -> Gen [TxIn]
mutateInputs [] = pure []
mutateInputs (txin : txins) = do
  mtxin <- mutateInput txin
  mtxins <- mutateInputs txins
  dropTxin <- Gen.enumBounded
  pure $ if dropTxin then mtxins else mtxin : mtxins

-- | Mutator for a single 'TxIn', which mutates the index of the output to
-- spend.
mutateInput :: TxIn -> Gen TxIn
mutateInput (TxIn idx index) = do
  index' <- mutateNat 0 100 index
  pure $ TxIn idx index'

-- | Mutator for a list of 'TxOut'.
mutateOutputs :: StrictSeq TxOut -> Gen (StrictSeq TxOut)
mutateOutputs StrictSeq.Empty = pure StrictSeq.Empty
mutateOutputs (txout :<| txouts) = do
  mtxout <- mutateOutput txout
  mtxouts <- mutateOutputs txouts
  dropTxOut <- Gen.enumBounded
  pure $ if dropTxOut then mtxouts else mtxout :<| mtxouts

-- | Mutator for a single 'TxOut' which mutates the associated 'Coin' value of
-- the output.
mutateOutput :: TxOut -> Gen TxOut
mutateOutput (TxOut addr c) = do
  c' <- mutateCoin 0 100 c
  pure $ TxOut addr c'

-- Mutators for 'DelegationData'

-- Mutators that change the type of certificate must be implemented in
-- 'Generator.hs' in order to prevent cyclic imports.

-- | Select one random verification staking key from list of pairs of KeyPair.
getAnyStakeKey :: KeyPairs -> Gen (VKey 'Staking)
getAnyStakeKey keys = vKey . snd <$> Gen.element keys

-- | Mutate 'Epoch' analogously to 'Coin' data.
mutateEpoch :: Natural -> Natural -> EpochNo -> Gen EpochNo
mutateEpoch lower upper (EpochNo val) =
  EpochNo . fromIntegral
    <$> mutateNat lower upper (fromIntegral val)

-- | Mutator for delegation certificates.
-- A 'RegKey' and 'DeRegKey' select randomly a key fomr the supplied list of
-- keypairs.
-- A 'RetirePool' certificate mutates the epoch and the key of the certificate.
-- A 'RegPool' certificate mutates the staking key, the pool's cost and margin.
-- A 'Delegate' certificates selects randomly keys for delegator and delegatee
-- from the supplied list of keypairs.
mutateDCert :: KeyPairs -> DPState -> DCert -> Gen DCert
mutateDCert keys _ (DCertDeleg (RegKey _)) =
  DCertDeleg . RegKey . KeyHashObj . hashKey . vKey . snd <$> Gen.element keys
mutateDCert keys _ (DCertDeleg (DeRegKey _)) =
  DCertDeleg . DeRegKey . KeyHashObj . hashKey . vKey . snd <$> Gen.element keys
mutateDCert keys _ (DCertPool (RetirePool _ epoch@(EpochNo e))) = do
  epoch' <- mutateEpoch 0 (fromIntegral e) epoch
  key' <- getAnyStakeKey keys
  pure $ DCertPool (RetirePool (coerceKeyRole $ hashKey key') epoch')
mutateDCert
  keys
  _
  ( DCertPool
      ( RegPool
          (PoolParams _ vrfHk pledge cost margin rwdacnt owners relays poolMD)
        )
    ) = do
    key' <- getAnyStakeKey keys
    cost' <- mutateCoin 0 100 cost
    p' <- mutateNat 0 100 (fromIntegral $ numerator $ intervalValue margin)
    let interval = fromMaybe interval0 (mkUnitInterval $ fromIntegral p' % 100)
    pure $
      (DCertPool . RegPool)
        (PoolParams (coerceKeyRole $ hashKey key') vrfHk pledge cost' interval rwdacnt owners relays poolMD)
mutateDCert keys _ (DCertDeleg (Delegate (Delegation _ _))) = do
  delegator' <- getAnyStakeKey keys
  delegatee' <- getAnyStakeKey keys
  pure $ DCertDeleg $ Delegate $ Delegation (KeyHashObj $ hashKey delegator') (coerceKeyRole $ hashKey delegatee')
mutateDCert keys _ (DCertGenesis (GenesisDelegCert gk _ vrfKH)) = do
  _delegatee <- getAnyStakeKey keys
  pure $ DCertGenesis $ GenesisDelegCert gk (coerceKeyRole $ hashKey _delegatee) vrfKH
mutateDCert _ _ (DCertMir (MIRCert pot credCoinMap)) = do
  let credCoinList = Map.toList credCoinMap
      coins = List.map snd credCoinList
  coins' <- mapM (mutateCoin 1 100) coins
  pure $ DCertMir $ MIRCert pot $ Map.fromList $ zip (List.map fst credCoinList) coins'
