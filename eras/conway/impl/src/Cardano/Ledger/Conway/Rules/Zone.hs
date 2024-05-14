{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Zone where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Conway.Core (
  ConwayEraTxBody (fulfillsTxBodyL),
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  EraTxBody (TxBody, inputsTxBodyL),
  txIdTx,
 )
import Cardano.Ledger.Conway.Era (ConwayZONE)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
 )
import Cardano.Ledger.Shelley.API (
  LedgerState (LedgerState),
  ShelleyLedgersEnv (LedgersEnv),
  TxIn (TxIn),
 )
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Void (Void)
import Lens.Micro ((^.))
import Lens.Micro.Type (Lens')

instance
  ( EraCrypto era ~ era
  , ConwayEraPParams era
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , EraTx era
  , ConwayEraTxBody era
  ) =>
  STS (ConwayZONE era)
  where
  type Environment (ConwayZONE era) = ShelleyLedgersEnv era
  type PredicateFailure (ConwayZONE era) = Void
  type Signal (ConwayZONE era) = Seq (Tx era)
  type State (ConwayZONE era) = LedgerState era
  type BaseM (ConwayZONE era) = ShelleyBase

  initialRules = []
  transitionRules = [zoneTransition]

-- We'll build the zones in the ZONES rule (turn the [Tx era] into [[Tx era]])?
zoneTransition ::
  forall era.
  ( --   EraCrypto era ~ era
    -- , EraTx era
    -- , ConwayEraTxBody era
    -- , ConwayEraPParams era
    -- ,
    Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  ) =>
  TransitionRule (ConwayZONE era)
zoneTransition =
  judgmentContext
    -- I guess we want UTxOStateTemp here?
    >>= \(TRC (LedgersEnv slotNo pParams accountState, LedgerState utxoState certState, txs :: Seq (Tx era))) -> do
      if chkLinear txs
        && all (chkRqTx txs) txs
        && all chkIsValid txs
        then -- ZONE-V

          trans @(EraRule "LEDGERS" era) $
            TRC (LedgersEnv slotNo pParams accountState, LedgerState utxoState certState, txs)
        else -- ZONE-N
          undefined
  where
    chkLinear :: Seq (Tx era) -> Bool
    chkLinear = undefined
    -- chkRqTx tb tx = ∀[ txrid ∈ tx .Tx.body .TxBody.requiredTxs ] Any (txrid ≡_) ( getIDs tb )
    chkRqTx :: Seq (Tx era) -> Tx era -> Bool
    chkRqTx _tb _tx = undefined
    -- chkIsValid tx = tx .Tx.isValid ≡ true
    chkIsValid :: Tx era -> Bool
    chkIsValid _tx = undefined

-- This should be moved to LEDGERS rule at the point where you figure out how to do it
-- isThisUseful :: UTxO era -> Seq (Tx era) -> (UTxO era, UTxO era)
-- isThisUseful utxo txs =
--   over both UTxO $
--     Map.partitionWithKey (\k _ -> k `notElem` (requiredTxs =<< toList txs)) (unUTxO utxo)

mkEdges ::
  EraTx era =>
  Lens' (TxBody era) (Set (TxIn (EraCrypto era))) ->
  Tx era ->
  [Tx era] ->
  [(Tx era, Tx era)]
mkEdges l = go
  where
    go _ [] = []
    go tx (h : txs) =
      if txIdTx tx `elem` fmap (\(TxIn x _) -> x) (toList (h ^. bodyTxL . l))
        then (tx, h) : go tx txs
        else go tx txs

-- make edges for a given transaction
mkIOEdges :: EraTx era => Tx era -> [Tx era] -> [(Tx era, Tx era)]
mkIOEdges = mkEdges inputsTxBodyL

-- make FR edges for a given transaction
mkFREdges :: (EraTx era, ConwayEraTxBody era) => Tx era -> [Tx era] -> [(Tx era, Tx era)]
mkFREdges = mkEdges fulfillsTxBodyL

-- make all edges for all transactions
mkAllEdges :: (EraTx era, ConwayEraTxBody era) => [Tx era] -> [Tx era] -> [(Tx era, Tx era)]
mkAllEdges [] _ = []
mkAllEdges (h : txs) ls = mkIOEdges h ls ++ mkFREdges h ls ++ mkAllEdges txs ls

-- -- get a set of TxIds containing all IDs of transaction in given list tb
-- getIDs : List Tx → ℙ TxId
-- getIDs tb = foldr (λ { tx ls → ls ∪ (singleton (tx .body .txid)) }) ∅ tb

-- -- make edges for a given transaction
-- mkEdges : Tx → List Tx → List (Tx × Tx)
-- mkEdges _ [] = []
-- mkEdges tx (h ∷ tb) with ( ( tx .body .txid ) ∈? (fromList (map proj₁ (setToList (h .body .txins)))) )
-- ... | yes p = (tx , h) ∷ mkEdges tx tb
-- ... | no ¬p = mkEdges tx tb

-- -- make FR edges for a given transaction
-- mkFREdges : Tx → List Tx → List (Tx × Tx)
-- mkFREdges _ [] = []
-- mkFREdges tx (h ∷ tb) with ( ( tx .body .txid ) ∈? (fromList (map proj₁ (setToList (h .body .fulfills)))) )
-- ... | yes p = (h , tx) ∷ mkFREdges tx tb
-- ... | no ¬p = mkFREdges tx tb

-- -- make all edges for all transactions
-- mkAllEdges : List Tx → List Tx → List (Tx × Tx)
-- mkAllEdges [] ls = []
-- mkAllEdges (h ∷ tb) ls = mkEdges h ls ++ mkFREdges h ls ++ mkAllEdges tb ls

-- -- for a given tx, and set of edges,
-- -- returns a list of transactions ls such that for each e in ls is such that e -> tx is a dependency
-- -- i.e. returns all ends of incoming edges
-- hasIncEdges : Tx → List (Tx × Tx) → List Tx
-- hasIncEdges tx [] = []
-- hasIncEdges tx ((e , tx') ∷ edges) with (tx .body .txid ≡ᵇ tx' .body .txid)
-- ... | true = e ∷ (hasIncEdges tx edges)
-- ... | false = (hasIncEdges tx edges)

-- -- filters a list of transactions such that only ones with no incoming edges remain
-- nodesWithNoIncomingEdge : List Tx → List (Tx × Tx) → List Tx
-- nodesWithNoIncomingEdge [] edges = []
-- nodesWithNoIncomingEdge (tx ∷ txs) edges with (hasIncEdges tx edges)
-- ... | e ∷ dges = nodesWithNoIncomingEdge txs edges
-- ... | [] = tx ∷ nodesWithNoIncomingEdge txs edges

-- -- remove the first instance of a transaction in a list
-- removeTx : Tx → List Tx → List Tx
-- removeTx tx [] = []
-- removeTx tx (n ∷ ne) with (tx .body .txid ≡ᵇ n .body .txid)
-- ... | true = ne
-- ... | false = [ n ] ++ (removeTx tx ne)

-- -- remove a transaction from a list if it has no incoming edges
-- ifNoEdgeRemove : Tx → List (Tx × Tx) → List Tx → List Tx
-- ifNoEdgeRemove tx edges s with (hasIncEdges tx edges)
-- ... | [] = removeTx tx s
-- ... | e ∷ dges = s

-- -- given tx1, for all tx such that (tx1 , tx) in edges,
-- --             remove (tx1 , tx) from the graph
-- --             if tx has no other incoming edges then
-- --               insert tx into S
-- updateRES : Tx → List (Tx × Tx) → List Tx → ((List (Tx × Tx)) × (List Tx))
-- updateRES tx1 [] s = ([] , s)
-- updateRES tx1 ((tx , tx') ∷ em) s with (tx .body .txid ≡ᵇ tx1 .body .txid)
-- ... | true = (proj₁ (updateRES tx1 em (ifNoEdgeRemove tx em s)) , (ifNoEdgeRemove tx em s))
-- ... | false = ((tx , tx') ∷ proj₁ (updateRES tx1 em s) , s)

-- -- topologically sorts a tx list
-- -- arguments : tracking edges for agda termination check, remaining edges, remaining txs with no incoming edge (S), current sorted list (L)
-- -- returns nothing if there are remaining edges the graph, but S is empty
-- topSortTxs : List (Tx × Tx) → List (Tx × Tx) → List Tx → List Tx → Maybe (List Tx)
-- topSortTxs e [] s srtd = just srtd
-- topSortTxs [] (r ∷ em) s srtd = nothing
-- topSortTxs e (r ∷ em) [] srtd = nothing
-- topSortTxs ((tx1 , tx2) ∷ dges) (r ∷ em) (tx ∷ rls) srtd =
--   topSortTxs dges (proj₁ updRES) (proj₂ updRES) (srtd ++ [ tx1 ])
--   where updRES = updateRES tx1 (r ∷ em) (removeTx tx1 (tx ∷ rls))

-- -- TOP SORT original
-- -- L ← Empty list that will contain the sorted elements
-- -- S ← Set of all nodes with no incoming edge
-- --
-- -- while S is not empty do
-- --     remove a node n from S
-- --     add n to L
-- --     for each node m with an edge e from n to m do
-- --         remove edge e from the graph
-- --         if m has no other incoming edges then
-- --             insert m into S
-- --
-- -- if graph has edges then
-- --     return error   (graph has at least one cycle)
-- -- else
-- --     return L   (a topologically sorted order)

-- -- TOP SORT implemented
-- -- L ← Empty list that will contain the sorted elements
-- -- S ← Set of all nodes with no incoming edge
-- -- --
-- -- for each (tx1 , tx2) in edges do
-- --           remove tx1 from S
-- --           add tx1 to L
-- --           for all tx such that (tx1 , tx) in edges,
-- --             remove (tx1 , tx) from the graph
-- --             if tx has no other incoming edges then
-- --               insert tx into S
-- --
-- -- if graph has edges then
-- --     return error   (graph has at least one cycle)
-- -- else
-- --     return L   (a topologically sorted order)