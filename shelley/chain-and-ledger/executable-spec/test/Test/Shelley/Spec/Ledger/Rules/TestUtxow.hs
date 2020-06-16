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

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map (isSubmapOf)
import qualified Data.Set as Set (fromList, intersection, isSubsetOf, map, null)
import Shelley.Spec.Ledger.Core ((<|), dom)
import Shelley.Spec.Ledger.LedgerState (keyRefunds, pattern UTxOState)
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.Tx
  ( _body,
    _witnessSet,
    addrWits,
    getKeyCombinations,
    msigWits,
  )
import Shelley.Spec.Ledger.TxData (_certs, _inputs, _txfee, witKeyHash, pattern TxIn)
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits, txins, txouts, pattern UTxO)
import Test.QuickCheck ((===), Property, conjoin)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( StakePools,
    Tx,
    UTXO,
    UTXOW,
  )

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  PParams ->
  [(StakePools ShortHash, SourceSignalTarget (UTXOW ShortHash))] ->
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
        + _txfee (_body tx)
        + totalDeposits pp stp_ (toList $ _certs $ _body tx)
    consumed u tx =
      balance u
        + keyRefunds pp (_body tx)

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  PParams ->
  [(StakePools ShortHash, SourceSignalTarget (UTXOW ShortHash))] ->
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
    inps u tx = balance $ (_inputs $ _body tx) <| u
    outs stp_ tx =
      balance (txouts tx)
        + _txfee tx
        + depositChange stp_ (toList $ _certs tx) tx
    depositChange stp_ certs txb =
      totalDeposits pp stp_ certs
        - (keyRefunds pp txb)

-- | Preserve outputs of Txs
preserveOutputsTx ::
  [SourceSignalTarget (UTXO ShortHash)] ->
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
  [SourceSignalTarget (UTXO ShortHash)] ->
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
        Set.null $ txins (_body tx) `Set.intersection` dom utxo'

-- | Check that all new entries of a Tx are included in the new UTxO and that
-- all TxIds are new.
newEntriesAndUniqueTxIns ::
  [SourceSignalTarget (UTXO ShortHash)] ->
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
            outIds = Set.map (\(TxIn _id _) -> _id) (dom outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (dom utxo)
         in null (outIds `Set.intersection` oldIds)
              && (dom outs) `Set.isSubsetOf` (dom utxo')

-- | Check for absence of double spend
noDoubleSpend ::
  [SourceSignalTarget (UTXO ShortHash)] ->
  Property
noDoubleSpend tr =
  [] === getDoubleInputs (map sig tr)
  where
    sig (SourceSignalTarget _ _ s) = s
    getDoubleInputs :: [Tx ShortHash] -> [(Tx ShortHash, [Tx ShortHash])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx ShortHash -> [Tx ShortHash] -> [(Tx ShortHash, [Tx ShortHash])]
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
requiredMSigSignaturesSubset :: [SourceSignalTarget (UTXOW ShortHash)] -> Property
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
