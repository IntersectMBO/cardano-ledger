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
import Data.Sequence (Seq)
import Data.Set (Set, toList)
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
      if txIdTx tx `elem` fmap (\(TxIn x _) -> x) (toList $ h ^. bodyTxL . l)
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

-- -- for a given tx, and set of edges,
-- -- returns a list of transactions ls such that for each e in ls is such that e -> tx is a dependency
-- -- i.e. returns all ends of incoming edges
hasIncEdges :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era]
hasIncEdges _ [] = []
hasIncEdges tx ((e, tx') : edges) =
  if txIdTx tx == txIdTx tx'
    then e : hasIncEdges tx edges
    else hasIncEdges tx edges

-- -- filters a list of transactions such that only ones with no incoming edges remain
nodesWithNoIncomingEdge :: EraTx era => [Tx era] -> [(Tx era, Tx era)] -> [Tx era]
nodesWithNoIncomingEdge [] _ = []
nodesWithNoIncomingEdge (tx : txs) edges = case hasIncEdges tx edges of
  [] -> tx : nodesWithNoIncomingEdge txs edges
  _ -> nodesWithNoIncomingEdge txs edges

-- -- remove the first instance of a transaction in a list
removeTx :: EraTx era => Tx era -> [Tx era] -> [Tx era]
removeTx _ [] = []
removeTx tx (n : ne) =
  if txIdTx tx == txIdTx n
    then ne
    else n : removeTx tx ne

-- -- remove a transaction from a list if it has no incoming edges
ifNoEdgeRemove :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era] -> [Tx era]
ifNoEdgeRemove tx edges s = case hasIncEdges tx edges of
  [] -> removeTx tx s
  _ -> s

-- given tx1, for all tx such that (tx1 , tx) in edges,
--             remove (tx1 , tx) from the graph
--             if tx has no other incoming edges then
--               insert tx into S
updateRES :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era] -> ([(Tx era, Tx era)], [Tx era])
updateRES _ [] s = ([], s)
updateRES tx1 ((tx, tx') : em) s =
  if txIdTx tx == txIdTx tx1
    then (fst (updateRES tx1 em (ifNoEdgeRemove tx em s)), ifNoEdgeRemove tx em s)
    else ((tx, tx') : fst (updateRES tx1 em s), s)

-- -- topologically sorts a tx list
-- -- arguments : tracking edges for agda termination check, remaining edges, remaining txs with no incoming edge (S), current sorted list (L)
-- -- returns nothing if there are remaining edges the graph, but S is empty
topSortTxs ::
  EraTx era => [(Tx era, Tx era)] -> [(Tx era, Tx era)] -> [Tx era] -> [Tx era] -> Maybe [Tx era]
topSortTxs _ [] _ srtd = Just srtd
topSortTxs [] _ _ _ = Nothing
topSortTxs _ _ [] _ = Nothing
topSortTxs ((tx1, _) : dges) (r : em) (tx : rls) srtd =
  topSortTxs dges (fst updRES) (snd updRES) (srtd ++ [tx1])
  where
    updRES = updateRES tx1 (r : em) (removeTx tx1 (tx : rls))

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