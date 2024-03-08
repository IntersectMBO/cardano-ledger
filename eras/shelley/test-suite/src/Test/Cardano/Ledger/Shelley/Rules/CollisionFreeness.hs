{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness (
  tests,
) where

import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness), witVKeyHash)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..), txins, txouts)
import Control.SetAlgebra (eval, (∩))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro hiding (ix)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombinations)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  TestingLedger,
  forAllChainTrace,
  ledgerTraceFromBlock,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  sourceSignalTargets,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
tests ::
  forall era ledger.
  ( EraGen era
  , ChainProperty era
  , TestingLedger era ledger
  , QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  TestTree
tests =
  testProperty "inputs are eliminated, outputs added to utxo and TxIds are unique" $
    forAllChainTrace @era traceLen defaultConstants $ \tr -> do
      let ssts = sourceSignalTargets tr
      conjoin . concat $
        [ -- collision freeness
          map (eliminateTxInputs @era @ledger) ssts
        , map (newEntriesAndUniqueTxIns @era @ledger) ssts
        , -- no double spend
          map noDoubleSpend ssts
        , -- tx signatures
          map (requiredMSigSignaturesSubset @era @ledger) ssts
        ]

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "eliminateTxInputs" $
    conjoin $
      map inputsEliminated $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    inputsEliminated
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = (UTxO u')}) _
        , signal = tx
        } =
        property $
          hasFailedScripts tx
            || Set.null (eval (txins @era (tx ^. bodyTxL) ∩ Map.keysSet u'))

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
newEntriesAndUniqueTxIns SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "newEntriesAndUniqueTxIns" $
    conjoin $
      map newEntryPresent $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    newEntryPresent
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = UTxO u}) _
        , signal = tx
        , target = LedgerState (UTxOState {utxosUtxo = UTxO u'}) _
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
            outIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet u)
         in property $
              hasFailedScripts tx
                || ((outIds `Set.disjoint` oldIds) && (outs `Map.isSubmapOf` u'))

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
requiredMSigSignaturesSubset ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
requiredMSigSignaturesSubset SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "requiredMSigSignaturesSubset" $
    conjoin $
      map signaturesSubset $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    signaturesSubset :: SourceSignalTarget ledger -> Property
    signaturesSubset SourceSignalTarget {signal = tx} =
      let khs = keyHashSet tx
       in property $
            all (existsReqKeyComb khs) (tx ^. witsTxL . scriptTxWitsL)

    existsReqKeyComb keyHashes msig =
      any (\kl -> Set.fromList kl `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
    keyHashSet :: Tx era -> Set (KeyHash 'Witness (EraCrypto era))
    keyHashSet tx_ =
      Set.map witVKeyHash (tx_ ^. witsTxL . addrTxWitsL)

--- | Check for absence of double spend in a block
noDoubleSpend ::
  forall era.
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
noDoubleSpend SourceSignalTarget {signal} =
  counterexample "noDoubleSpend" $
    [] === getDoubleInputs txs
  where
    txs = toList $ (fromTxSeq @era . bbody) signal

    getDoubleInputs :: [Tx era] -> [(Tx era, [Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx era -> [Tx era] -> [(Tx era, [Tx era])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      [(tx_j, doubles) | not (null doubles)]
      where
        doubles =
          if hasFailedScripts tx_j
            then []
            else
              filter
                ( \tx_i ->
                    not
                      ( hasFailedScripts tx_i
                          || Set.disjoint inps_j (tx_i ^. bodyTxL . inputsTxBodyL)
                      )
                )
                ts
        inps_j = tx_j ^. bodyTxL . inputsTxBodyL
