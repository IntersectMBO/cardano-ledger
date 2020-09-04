{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestUtxow
  ( preserveBalance,
    preserveBalanceRestricted,
    preserveOutputsTx,
    eliminateTxInputs,
    newEntriesAndUniqueTxIns,
    noDoubleSpend,
    requiredMSigSignaturesSubset,
  )
where

import Control.Iterate.SetAlgebra (dom, domain, eval, (<|), (∩), (⊆))
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (isSubmapOf)
import qualified Data.Set as Set (fromList, intersection, isSubsetOf, map, null)
import Shelley.Spec.Ledger.API
  ( UTXO,
    UTXOW,
  )
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyRole (..),
  )
import Shelley.Spec.Ledger.LedgerState (keyRefunds, pattern UTxOState)
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.Tx
  ( Tx,
    addrWits,
    getKeyCombinations,
    msigWits,
    _body,
    _witnessSet,
  )
import Shelley.Spec.Ledger.TxBody
  ( PoolParams (..),
    TxIn (..),
    witKeyHash,
    _certs,
    _inputs,
    _txfee,
  )
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits, txins, txouts, pattern UTxO)
import qualified Cardano.Ledger.Val as Val
import Test.QuickCheck (Property, conjoin, (===))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  PParams ->
  [ ( Map (KeyHash 'StakePool C) (PoolParams C),
      SourceSignalTarget (UTXOW C)
    )
  ] ->
  Property
preserveBalance pp tr =
  conjoin $
    map createdIsConsumed tr
  where
    createdIsConsumed
      ( stp,
        SourceSignalTarget
          { source = UTxOState u _ _ _,
            signal = tx,
            target = UTxOState u' _ _ _
          }
        ) =
        created u' stp tx == consumed u tx
    created u stp_ tx =
      balance u
        <> _txfee (_body tx)
        <> totalDeposits pp stp_ (toList $ _certs $ _body tx)
    consumed u tx =
      balance u
        <> keyRefunds pp (_body tx)

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  PParams ->
  [ ( Map (KeyHash 'StakePool C) (PoolParams C),
      SourceSignalTarget (UTXOW C)
    )
  ] ->
  Property
preserveBalanceRestricted pp tr =
  conjoin $
    map createdIsConsumed tr
  where
    createdIsConsumed
      ( stp,
        SourceSignalTarget
          { source = UTxOState u _ _ _,
            signal = tx,
            target = UTxOState _ _ _ _
          }
        ) =
        inps u tx == outs stp (_body tx)
    inps u tx = balance $ eval ((_inputs $ _body tx) <| u)
    outs stp_ tx =
      balance (txouts tx)
        <> _txfee tx
        <> depositChange stp_ (toList $ _certs tx) tx
    depositChange stp_ certs txb =
      totalDeposits pp stp_ certs
        Val.~~ (keyRefunds pp txb)

-- | Preserve outputs of Txs
preserveOutputsTx ::
  [SourceSignalTarget (UTXO C)] ->
  Property
preserveOutputsTx tr =
  conjoin $
    map outputPreserved tr
  where
    outputPreserved
      SourceSignalTarget
        { signal = tx,
          target = UTxOState (UTxO utxo') _ _ _
        } =
        let UTxO outs = txouts (_body tx)
         in outs `Map.isSubmapOf` utxo'

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  [SourceSignalTarget (UTXO C)] ->
  Property
eliminateTxInputs tr =
  conjoin $
    map inputsEliminated tr
  where
    inputsEliminated
      SourceSignalTarget
        { signal = tx,
          target = UTxOState (UTxO utxo') _ _ _
        } =
        Set.null $ eval (txins (_body tx) ∩ dom utxo')

-- | Check that all new entries of a Tx are included in the new UTxO and that
-- all TxIds are new.
newEntriesAndUniqueTxIns ::
  [SourceSignalTarget (UTXO C)] ->
  Property
newEntriesAndUniqueTxIns tr =
  conjoin $
    map newEntryPresent tr
  where
    newEntryPresent
      SourceSignalTarget
        { source = (UTxOState (UTxO utxo) _ _ _),
          signal = tx,
          target = (UTxOState (UTxO utxo') _ _ _)
        } =
        let UTxO outs = txouts (_body tx)
            outIds = Set.map (\(TxIn _id _) -> _id) (domain outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (domain utxo)
         in null (outIds `Set.intersection` oldIds)
              && eval ((dom outs) ⊆ (dom utxo'))

-- | Check for absence of double spend
noDoubleSpend ::
  [SourceSignalTarget (UTXO C)] ->
  Property
noDoubleSpend tr =
  [] === getDoubleInputs (map sig tr)
  where
    sig (SourceSignalTarget _ _ s) = s
    getDoubleInputs :: [Tx C] -> [(Tx C, [Tx C])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx C -> [Tx C] -> [(Tx C, [Tx C])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
      where
        doubles =
          filter
            ( \tx_i ->
                (not . Set.null)
                  (inps_j `Set.intersection` _inputs (_body tx_i))
            )
            ts
        inps_j = _inputs $ _body tx_j

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
--
-- TODO @mgudemann
-- This property is currenty disabled du to time-out problems with getting all
-- possible combinations for multi-sig.
requiredMSigSignaturesSubset :: [SourceSignalTarget (UTXOW C)] -> Property
requiredMSigSignaturesSubset tr =
  conjoin $
    map signaturesSubset tr
  where
    signaturesSubset sst =
      let khs = keyHashSet sst
       in all (existsReqKeyComb khs) (msigWits . _witnessSet $ signal sst)
    existsReqKeyComb keyHashes msig =
      any (\kl -> (Set.fromList kl) `Set.isSubsetOf` keyHashes) (getKeyCombinations msig)
    keyHashSet sst =
      Set.map witKeyHash (addrWits . _witnessSet $ signal sst)
