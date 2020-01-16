{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestUtxow
  ( preserveBalance
  , preserveBalanceRestricted
  , preserveOutputsTx
  , eliminateTxInputs
  , newEntriesAndUniqueTxIns
  , noDoubleSpend
  , requiredMSigSignaturesSubset)
where

import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map (isSubmapOf)
import qualified Data.Set as Set (fromList, intersection, isSubsetOf, map, null)

import           Test.QuickCheck (Property, conjoin, (===))

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, target)

import           ConcreteCryptoTypes (StakeCreds, StakePools, Tx, UTXO, UTXOW)
import           Keys (hashAnyKey)
import           Ledger.Core (dom, (<|))
import           LedgerState (pattern UTxOState, keyRefunds)
import           PParams (PParams)
import           Tx (getKeyCombinations)
import           TxData (pattern TxIn, pattern WitGVKey, pattern WitVKey, _body, _certs, _inputs,
                     _txfee, _witnessMSigMap, _witnessVKeySet)
import           UTxO (pattern UTxO, balance, totalDeposits, txins, txouts)

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance
  :: PParams
  -> [(StakeCreds, StakePools, SourceSignalTarget UTXOW)]
  -> Property
preserveBalance pp tr =
  conjoin $
    map createdIsConsumed tr
  where
    createdIsConsumed (stk, stp, SourceSignalTarget
                                   { source = UTxOState u _ _ _
                                   , signal = tx
                                   , target = UTxOState u' _ _ _}) =
      created u' stp tx == consumed u stk tx

    created u stp_ tx =
        balance u
      + _txfee (_body tx)
      + totalDeposits pp stp_ (toList $ _certs $ _body tx)

    consumed u stk_ tx =
        balance u
      + keyRefunds pp stk_ (_body tx)

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted
  :: PParams
  -> [(StakeCreds, StakePools, SourceSignalTarget UTXOW)]
  -> Property
preserveBalanceRestricted pp tr =
  conjoin $
    map createdIsConsumed tr
  where
    createdIsConsumed (stk, stp, SourceSignalTarget
                                   { source = UTxOState u _ _ _
                                   , signal = tx
                                   , target = UTxOState _ _ _ _}) =
      inps u tx == outs stk stp (_body tx)

    inps u tx = balance $ (_inputs $ _body tx) <| u

    outs stk_ stp_ tx =
        balance (txouts tx)
      + _txfee tx
      + depositChange stk_ stp_ (toList $ _certs tx) tx

    depositChange stk_ stp_ certs txb =
        totalDeposits pp stp_ certs
      - (keyRefunds pp stk_ txb)

-- | Preserve outputs of Txs
preserveOutputsTx
  :: [SourceSignalTarget UTXO]
  -> Property
preserveOutputsTx tr =
  conjoin $
    map outputPreserved tr
  where
    outputPreserved SourceSignalTarget
                      { signal = tx
                      , target = UTxOState (UTxO utxo') _ _ _} =
      let UTxO outs = txouts (_body tx)
      in outs `Map.isSubmapOf` utxo'

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs
  :: [SourceSignalTarget UTXO]
  -> Property
eliminateTxInputs tr =
  conjoin $
    map inputsEliminated tr
  where
    inputsEliminated SourceSignalTarget
                       { signal = tx
                       , target = UTxOState (UTxO utxo') _ _ _} =
      Set.null $ txins (_body tx) `Set.intersection` dom utxo'

-- | Check that all new entries of a Tx are included in the new UTxO and that
-- all TxIds are new.
newEntriesAndUniqueTxIns
  :: [SourceSignalTarget UTXO]
  -> Property
newEntriesAndUniqueTxIns tr =
  conjoin $
    map newEntryPresent tr
  where
    newEntryPresent SourceSignalTarget
                      { source = (UTxOState (UTxO utxo) _ _ _)
                      , signal = tx
                      , target = (UTxOState (UTxO utxo') _ _ _)} =
      let UTxO outs = txouts (_body tx)
          outIds = Set.map (\(TxIn _id _) -> _id) (dom outs)
          oldIds = Set.map (\(TxIn _id _) -> _id) (dom utxo)
      in
           null (outIds `Set.intersection` oldIds)
        && (dom outs) `Set.isSubsetOf` (dom utxo')

-- | Check for absence of double spend
noDoubleSpend
  :: [SourceSignalTarget UTXO]
  -> Property
noDoubleSpend tr =
  [] === getDoubleInputs (map sig tr)
  where
    sig (SourceSignalTarget _ _ s) = s

    getDoubleInputs :: [Tx] -> [(Tx, [Tx])]
    getDoubleInputs [] = []
    getDoubleInputs (t:ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts

    lookForDoubleSpends :: Tx -> [Tx] -> [(Tx, [Tx])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
      where doubles =
              filter (\tx_i -> (not . Set.null)
                       (inps_j `Set.intersection` _inputs (_body tx_i))) ts
            inps_j = _inputs $ _body tx_j

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
--
-- TODO @mgudemann
-- This property is currenty disabled du to time-out problems with getting all
-- possible combinations for multi-sig.
requiredMSigSignaturesSubset :: [SourceSignalTarget UTXOW] -> Property
requiredMSigSignaturesSubset tr =
  conjoin $
  map signaturesSubset tr
  where
    signaturesSubset sst =
      let khs = keyHashSet sst in
      all (existsReqKeyComb khs) (_witnessMSigMap $ signal sst)
    existsReqKeyComb keyHashes msig  =
      any (\kl -> (Set.fromList kl) `Set.isSubsetOf` keyHashes) (getKeyCombinations msig)
    keyHashSet sst =
      Set.map (\case
                  WitVKey vk _   -> hashAnyKey vk
                  WitGVKey vkg _ -> hashAnyKey vkg) (_witnessVKeySet $ signal sst)
