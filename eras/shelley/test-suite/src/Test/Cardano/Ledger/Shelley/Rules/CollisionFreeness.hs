{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness (
  tests,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (witVKeyHash)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.TxIn (TxIn (..))
import Control.SetAlgebra (eval, (∩))
import Control.State.Transition.Extended (BaseM, Environment, STS, Signal, State)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro hiding (ix)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombinations)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  forAllChainTrace,
  ledgerTraceFromBlock,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (ChainProperty)
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
  forall era.
  ( EraGen era
  , EraStake era
  , ChainProperty era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , STS (EraRule "LEDGER" era)
  ) =>
  TestTree
tests =
  testProperty "inputs are eliminated, outputs added to utxo and TxIds are unique" $
    forAllChainTrace @era traceLen defaultConstants $ \tr -> do
      let ssts = sourceSignalTargets tr
      conjoin . concat $
        [ -- collision freeness
          map (eliminateTxInputs @era) ssts
        , map (newEntriesAndUniqueTxIns @era) ssts
        , -- no double spend
          map noDoubleSpend ssts
        , -- tx signatures
          map (requiredMSigSignaturesSubset @era) ssts
        ]

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , STS (EraRule "LEDGER" era)
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "eliminateTxInputs" $
    conjoin $
      map inputsEliminated $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
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
  forall era.
  ( ChainProperty era
  , EraGen era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , STS (EraRule "LEDGER" era)
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
newEntriesAndUniqueTxIns SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "newEntriesAndUniqueTxIns" $
    conjoin $
      map newEntryPresent $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
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
  forall era.
  ( ChainProperty era
  , EraGen era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , STS (EraRule "LEDGER" era)
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
requiredMSigSignaturesSubset SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "requiredMSigSignaturesSubset" $
    conjoin $
      map signaturesSubset $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    signaturesSubset :: SourceSignalTarget (EraRule "LEDGER" era) -> Property
    signaturesSubset SourceSignalTarget {signal = tx} =
      let khs = keyHashSet tx
       in property $
            all (existsReqKeyComb khs) (tx ^. witsTxL . scriptTxWitsL)

    existsReqKeyComb keyHashes msig =
      any (\kl -> Set.fromList kl `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
    keyHashSet :: Tx era -> Set (KeyHash 'Witness)
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
    txs = toList $ bbody signal ^. txSeqBlockBodyL

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
