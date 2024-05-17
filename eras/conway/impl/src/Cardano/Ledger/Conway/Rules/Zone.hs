{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Zone where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx)
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid))
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Conway.Core (
  ConwayEraTxBody (fulfillsTxBodyL, requiredTxsTxBodyL),
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  EraTxBody (TxBody, inputsTxBodyL),
  InjectRuleFailure (..),
  isValidTxL,
 )
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayZONE)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
 )
import Cardano.Ledger.Core (txIdTx)
import Cardano.Ledger.Shelley.API (
  LedgerState (LedgerState),
  TxIn (TxIn),
 )
import Cardano.Ledger.TxIn (TxId)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import Data.Set (Set, toList)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.Type (Lens')

import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers (ConwayLedgersEnv (ConwayLedgersEnv))
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core (EraRuleEvent, EraRuleFailure)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
 )
import NoThunks.Class (NoThunks)

data ConwayZonePredFailure era
  = LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
  | -- | ShelleyInConwayPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
    ShelleyInConwayPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
  deriving (Generic)

newtype ConwayZoneEvent era
  = ShelleyInConwayEvent (ShelleyLedgersEvent era)

type instance EraRuleFailure "ZONE" (ConwayEra c) = ConwayZonePredFailure (ConwayEra c)

instance InjectRuleFailure "ZONE" ConwayZonePredFailure (ConwayEra c)

type instance EraRuleEvent "ZONE" (ConwayEra c) = ConwayZoneEvent (ConwayEra c)

instance InjectRuleFailure "ZONE" ShelleyLedgersPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure

instance InjectRuleFailure "ZONE" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure :: ConwayLedgerPredFailure (ConwayEra c) -> ConwayZonePredFailure (ConwayEra c)
  injectFailure = undefined -- LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = undefined -- ShelleyInConwayPredFailure . Shelley.LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" BabbageUtxowPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" AlonzoUtxowPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ShelleyUtxowPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayUtxoPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" BabbageUtxoPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" AlonzoUtxoPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" AlonzoUtxosPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayUtxosPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ShelleyUtxoPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" AllegraUtxoPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayCertsPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayCertPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayDelegPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ShelleyPoolPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayGovCertPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "ZONE" ConwayGovPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (ConwayZonePredFailure era)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (ConwayZonePredFailure era)

deriving anyclass instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (ConwayZonePredFailure era)

instance
  ( EraCrypto era ~ era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ConwayEraPParams era
  , Environment (EraRule "LEDGERS" era) ~ ConwayLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , EraTx era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  ) =>
  STS (ConwayZONE era)
  where
  type Environment (ConwayZONE era) = ConwayLedgersEnv era
  type PredicateFailure (ConwayZONE era) = ConwayZonePredFailure era
  type Signal (ConwayZONE era) = Seq (Tx era)
  type State (ConwayZONE era) = LedgerState era
  type BaseM (ConwayZONE era) = ShelleyBase

  initialRules = []
  transitionRules = [zoneTransition]

-- We'll build the zones in the ZONES rule (turn the [Tx era] into [[Tx era]])?
zoneTransition ::
  forall era.
  ( Environment (EraRule "LEDGERS" era) ~ ConwayLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , EraCrypto era ~ era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  TransitionRule (ConwayZONE era)
zoneTransition =
  judgmentContext
    -- I guess we want UTxOStateTemp here?
    >>= \( TRC
            s@( ConwayLedgersEnv slotNo ixRange pParams accountState
                , LedgerState utxoState certState
                , txs :: Seq (Tx era)
                )
          ) -> do
        if chkLinear (Foldable.toList txs)
          && all (chkRqTx txs) txs
          && all chkIsValid txs
          then -- ZONE-V

            trans @(EraRule "LEDGERS" era) $
              TRC (ConwayLedgersEnv slotNo ixRange pParams accountState, LedgerState utxoState certState, txs)
          else -- ZONE-N
          -- TODO
            trans @(ConwayZONE era) $ TRC s
  where
    chkLinear :: [Tx era] -> Bool
    chkLinear txs =
      topSortTxs
        (mkAllEdges txs txs)
        (mkAllEdges txs txs)
        (nodesWithNoIncomingEdge txs (mkAllEdges txs txs))
        []
        == Just []
    -- chkRqTx txs tx = ∀[ txrid ∈ tx .Tx.body .TxBody.requiredTxs ] Any (txrid ≡_) ( getIDs txs )
    chkRqTx :: Seq (Tx era) -> Tx era -> Bool
    chkRqTx txs tx = all chk txrids
      where
        chk txrid = txrid `elem` ids
        txrids = fmap txInTxId $ toList $ tx ^. bodyTxL . requiredTxsTxBodyL
        ids :: Set (TxId era)
        ids = getIDs $ Foldable.toList txs
    -- chkIsValid tx = tx .Tx.isValid ≡ true
    chkIsValid :: Tx era -> Bool
    chkIsValid tx = tx ^. isValidTxL == IsValid True

-- This should be moved to LEDGERS rule at the point where you figure out how to do it
-- isThisUseful :: UTxO era -> Seq (Tx era) -> (UTxO era, UTxO era)
-- isThisUseful utxo txs =
--   over both UTxO $
--     Map.partitionWithKey (\k _ -> k `notElem` (requiredTxs =<< toList txs)) (unUTxO utxo)

txInTxId :: TxIn era -> TxId era
txInTxId (TxIn x _) = x

-- get a set of TxIds containing all IDs of transaction in given list tb
getIDs :: (EraCrypto era ~ era, EraTx era) => [Tx era] -> Set (TxId (EraCrypto era))
getIDs = foldr (\tx ls -> ls `Set.union` Set.singleton (txIdTx tx)) mempty

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

-- remove a transaction from a list if it has no incoming edges
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