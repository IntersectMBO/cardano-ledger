{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestUtxow where

import           Control.Monad (when)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map (isSubmapOf)
import qualified Data.Set as Set (intersection, isSubsetOf, map, null)
import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests, (===))

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern SourceSignalTarget, TraceOrder (..),
                     signal, source, sourceSignalTargets, target, traceLength, traceSignals,
                     _traceEnv)

import           LedgerState (pattern UTxOState, decayedTx, keyRefunds)
import           MockTypes (Tx, UTXOW)
import           STS.Utxo (UtxoEnv (..))
import           TxData (pattern TxIn, _body, _certs, _inputs, _txfee)
import           UTxO (pattern UTxO, balance, deposits, txins, txouts)

import           Ledger.Core (dom, (<|))
import           Test.Utils (assertAll, testGlobals, runShelleyBase)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance :: Property
preserveBalance = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals traceLen `ofLengthAtLeast` 1)
  let
    tr = sourceSignalTargets t
    UtxoEnv _ pp stk stp _ = _traceEnv t

  assertAll (createdIsConsumed pp stk stp) tr

  where createdIsConsumed pp stk stp (SourceSignalTarget
                                       { source = UTxOState u _ _ _
                                       , signal = tx
                                       , target = UTxOState u' _ _ _}) =
          created u' tx pp stp == consumed u tx pp stk
        created u tx pp stp =
            balance u
          + _txfee (_body tx)
          + (deposits pp stp (toList $ _certs $ _body tx))
        consumed u tx pp stk =
            balance u
          + keyRefunds pp stk (_body tx)

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted :: Property
preserveBalanceRestricted = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals traceLen `ofLengthAtLeast` 1)
  let
    tr = sourceSignalTargets t
    UtxoEnv _ pp stk stp _ = _traceEnv t

  assertAll (createdIsConsumed pp stk stp) tr

  where createdIsConsumed pp stk stp (SourceSignalTarget
                                       { source = UTxOState u _ _ _
                                       , signal = tx
                                       , target = UTxOState _ _ _ _}) =
          inps u tx == outs (_body tx) pp stk stp
        inps u tx = balance $ (_inputs $ _body tx) <| u
        outs tx pp stk stp =
            balance (txouts tx)
          + _txfee tx
          + depositChange pp stk stp (toList $ _certs tx) tx

        depositChange pp stk stp certs txb =
            deposits pp stp certs
          - (refunded pp stk txb + decayed pp stk txb)
        refunded pp stk txb = keyRefunds pp stk txb
        decayed pp stk txb  = runShelleyBase $ decayedTx pp stk txb

-- | Preserve outputs of Txs
preserveOutputsTx :: Property
preserveOutputsTx = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    assertAll outputPreserved tr

  where outputPreserved (SourceSignalTarget
                         { signal = tx
                         , target = UTxOState (UTxO utxo') _ _ _}) =
          let UTxO outs = txouts (_body tx) in
          outs `Map.isSubmapOf` utxo'

-- | Check that consumed inputs are elimiated from the resulting UTxO
eliminateTxInputs :: Property
eliminateTxInputs = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    assertAll inputsEliminated tr

  where inputsEliminated (SourceSignalTarget
                           { signal = tx
                           , target = UTxOState (UTxO utxo') _ _ _}) =
          Set.null $ txins (_body tx) `Set.intersection` dom utxo'

-- | Check that all new entries of a Tx are included in the new UTxO and that
-- all TxIds are new.
newEntriesAndUniqueTxIns :: Property
newEntriesAndUniqueTxIns = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    assertAll newEntryPresent tr

  where newEntryPresent (SourceSignalTarget
                          { source = (UTxOState (UTxO utxo) _ _ _)
                          , signal = tx
                          , target = (UTxOState (UTxO utxo') _ _ _)}) =
          let UTxO outs = txouts (_body tx)
              outIds = Set.map (\(TxIn _id _) -> _id) (dom outs)
              oldIds = Set.map (\(TxIn _id _) -> _id) (dom utxo)
          in
               null (outIds `Set.intersection` oldIds)
            && (dom outs) `Set.isSubsetOf` (dom utxo')

-- | Check for absence of double spend
noDoubleSpend :: Property
noDoubleSpend = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @UTXOW testGlobals $ fromIntegral traceLen)
  let
    sigs = traceSignals OldestFirst t

  [] === getDoubleInputs sigs

  where
    getDoubleInputs :: [Tx] -> [(Tx, [Tx])]
    getDoubleInputs [] = []
    getDoubleInputs (t:ts) = (lookForDoubleSpends t ts) ++ (getDoubleInputs ts)

    lookForDoubleSpends :: Tx -> [Tx] -> [(Tx, [Tx])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
      where doubles =
              filter (\tx_i -> (not . Set.null)
                       (inps_j `Set.intersection` (_inputs $ _body tx_i))) ts
            inps_j = _inputs $ _body tx_j
