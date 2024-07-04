{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Zone where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx)
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), totExUnits)
import Cardano.Ledger.Babel.Core (
  BabelEraTxBody (fulfillsTxBodyL),
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  EraTxBody (TxBody, inputsTxBodyL),
  InjectRuleFailure (..),
  collateralInputsTxBodyL,
  isValidTxL,
  requiredTxsTxBodyL,
 )
import Cardano.Ledger.Babel.Era (BabelEra, BabelZONE)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  epochInfo,
  systemStart,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
 )
import Cardano.Ledger.Core (
  EraRuleEvent,
  EraRuleFailure,
  txIdTx,
 )
import Cardano.Ledger.Shelley.API (
  LedgerState (LedgerState),
  TxIn (TxIn),
  UTxO (..),
  UTxOState (..),
 )
import Cardano.Ledger.TxIn (TxId)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  tellEvent,
  trans,
  whenFailureFree,
 )
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import Data.Set (Set, toList)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.Type (Lens')

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext, evalPlutusScripts)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (ValidationTagMismatch),
  TagMismatchDescription (PassedUnexpectedly),
  invalidBegin,
  invalidEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babel.Core (ppMaxTxExUnitsL)
import Cardano.Ledger.Babel.LedgerState.Types (
  LedgerStateTemp,
  fromLedgerState,
  toLedgerState,
 )
import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Ledgers (BabelLEDGERS, BabelLedgersEnv (BabelLedgersEnv))
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure (..))
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Plutus (
  PlutusWithContext,
  ScriptFailure (scriptFailurePlutus),
  ScriptResult (..),
 )
import Cardano.Ledger.Plutus.ExUnits (pointWiseExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (updateStakeDistribution)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
 )
import Cardano.Ledger.UTxO (EraUTxO (ScriptsNeeded))
import Control.Monad.RWS (asks)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import NoThunks.Class (NoThunks)
import Validation (failureUnless)

data BabelZonePredFailure era
  = LedgersFailure (PredicateFailure (BabelLEDGERS era)) -- Subtransition Failures
  | -- | ShelleyInBabelPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
    ShelleyInBabelPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
  deriving (Generic)

data BabelZoneEvent era
  = ShelleyInBabelEvent (ShelleyLedgersEvent era)
  | ZoneFailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | ZoneSuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))

type instance EraRuleFailure "ZONE" (BabelEra c) = BabelZonePredFailure (BabelEra c)

instance InjectRuleFailure "ZONE" BabelZonePredFailure (BabelEra c)

type instance EraRuleEvent "ZONE" (BabelEra c) = BabelZoneEvent (BabelEra c)

instance InjectRuleFailure "ZONE" ShelleyLedgersPredFailure (BabelEra c) where
  injectFailure = LedgersFailure

instance InjectRuleFailure "ZONE" BabelLedgerPredFailure (BabelEra c) where
  injectFailure :: BabelLedgerPredFailure (BabelEra c) -> BabelZonePredFailure (BabelEra c)
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (BabelZonePredFailure era)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (BabelZonePredFailure era)

deriving anyclass instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (BabelZonePredFailure era)

instance
  ( EraRule "ZONE" era ~ BabelZONE era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ConwayEraPParams era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerStateTemp era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , EraTx era
  , BabelEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  ) =>
  STS (BabelZONE era)
  where
  type Environment (BabelZONE era) = BabelLedgersEnv era
  type PredicateFailure (BabelZONE era) = BabelZonePredFailure era
  type Signal (BabelZONE era) = Seq (Tx era)
  type State (BabelZONE era) = LedgerState era
  type BaseM (BabelZONE era) = ShelleyBase
  type Event (BabelZONE era) = BabelZoneEvent era

  initialRules = []
  transitionRules = [zoneTransition]

{- txsize tx ≤ maxTxSize pp -}
-- We've moved this to the ZONE rule. See https://github.com/IntersectMBO/formal-ledger-specifications/commit/c3e18ac1d3da92dd4894bbc32057a143f9720f52#diff-5f67369ed62c0dab01e13a73f072b664ada237d094bbea4582365264dd163bf9
-- ((totSizeZone ltx) ≤ᵇ (Γ .LEnv.pparams .PParams.maxTxSize)) ≡ true
-- runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

zoneTransition ::
  forall era.
  ( EraRule "ZONE" era ~ BabelZONE era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerStateTemp era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , BabelEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  ) =>
  TransitionRule (BabelZONE era)
zoneTransition =
  judgmentContext
    -- I guess we want UTxOStateTemp here?
    >>= \( TRC
            ( BabelLedgersEnv slotNo ixRange pParams accountState
              , LedgerState utxoState certState
              , txs :: Seq (Tx era)
              )
          ) -> do
        {- ((totSizeZone ltx) ≤ᵇ (Γ .LEnv.pparams .PParams.maxTxSize)) ≡ true -}
        runTestOnSignal $ validateMaxTxSizeUTxO pParams (Foldable.toList txs)
        if all chkIsValid txs -- ZONE-V
          then do
            -- TODO WG: make sure `runTestOnSignal` is correct rather than `runTest`
            {- All (chkRqTx ltx) (fromList ltx) -}
            runTestOnSignal $ failureUnless (all (chkRqTx txs) txs) CheckRqTxFailure
            {- noCycles ltx -}
            runTestOnSignal $ failureUnless (chkLinear (Foldable.toList txs)) CheckLinearFailure
            {- totExunits tx ≤ maxTxExUnits pp -}
            runTestOnSignal $ validateExUnitsTooBigUTxO pParams (Foldable.toList txs)
            lsTemp <- -- TODO WG: Should we be checking FRxO is empty before converting?
              trans @(EraRule "LEDGERS" era) $
                TRC
                  ( BabelLedgersEnv slotNo ixRange pParams accountState
                  , fromLedgerState $ LedgerState utxoState certState
                  , txs
                  )
            pure $ toLedgerState lsTemp
          else -- ZONE-N
          do
            -- Check that only the last transaction is invalid
            runTestOnSignal $
              failureUnless (chkExactlyLastInvalid (Foldable.toList txs)) MoreThanOneInvalidTransaction
            babelEvalScriptsTxInvalid @era
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
        -- asd = tx ^. requiredTxsTxL
        txrids = fmap txInTxId $ toList $ tx ^. bodyTxL . requiredTxsTxBodyL
        ids :: Set (TxId (EraCrypto era))
        ids = getIDs $ Foldable.toList txs
    -- chkIsValid tx = tx .Tx.isValid ≡ true
    chkIsValid :: Tx era -> Bool
    chkIsValid tx = tx ^. isValidTxL == IsValid True
    sizeTx :: Tx era -> Integer
    sizeTx t = t ^. sizeTxF
    totSizeZone :: [Tx era] -> Integer
    totSizeZone z = sum (map sizeTx z)
    validateMaxTxSizeUTxO ::
      PParams era ->
      [Tx era] ->
      Test (BabelUtxoPredFailure era)
    validateMaxTxSizeUTxO pp z =
      failureUnless (zoneSize <= maxTxSize) $ MaxTxSizeUTxO zoneSize maxTxSize
      where
        maxTxSize = toInteger (pp ^. ppMaxTxSizeL)
        zoneSize = totSizeZone z
    validateExUnitsTooBigUTxO ::
      PParams era ->
      [Tx era] ->
      Test (BabelUtxoPredFailure era)
    validateExUnitsTooBigUTxO pp txs =
      failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
        ExUnitsTooBigUTxO maxTxExUnits totalExUnits
      where
        maxTxExUnits = pp ^. ppMaxTxExUnitsL
        -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the zone:
        totalExUnits = Foldable.foldl' (<>) mempty $ fmap totExUnits txs
    -- TODO WG: This can probably be rolled in with the main check in the if expression
    chkExactlyLastInvalid :: [Tx era] -> Bool
    chkExactlyLastInvalid txs = case reverse txs of
      (l : txs') -> (l ^. isValidTxL == IsValid False) && all ((== IsValid True) . (^. isValidTxL)) txs'
      [] -> True

-- data BabelUtxosEvent era
--   = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
--   | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
--   | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
--   | -- | The UTxOs consumed and created by a signal tx
--     TxUTxODiff
--       -- | UTxO consumed
--       (UTxO era)
--       -- | UTxO created
--       (UTxO era)
--   deriving (Generic)

babelEvalScriptsTxInvalid ::
  forall era.
  ( EraRule "ZONE" era ~ BabelZONE era
  , BabelEraTxBody era
  , AlonzoEraTx era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerStateTemp era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraRuleFailure "ZONE" era ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  ) =>
  TransitionRule (BabelZONE era)
babelEvalScriptsTxInvalid =
  do
    TRC
      ( BabelLedgersEnv _slotNo _ixRange pp _accountState
        , LedgerState us@(UTxOState utxo _ fees _ _ _) certState
        , txs :: Seq (Tx era)
        ) <-
      judgmentContext
    -- TODO WG: Is the list last first or last...last (Probably last last)
    let tx = last (Foldable.toList txs) -- TODO WG use safe head
        txBody = tx ^. bodyTxL

    -- TRC (UtxoEnv _ pp _, us@(UTxOState utxo _ _ fees _ _ _), tx) <- judgmentContext
    -- {- txb := txbody tx -}
    sysSt <- liftSTS $ asks systemStart
    ei <- liftSTS $ asks epochInfo

    () <- pure $! traceEvent invalidBegin ()

    {- TODO WG:
      I think you actually need a different function that collects Plutus scripts from
      ALL transactions, but just using the collateral for the last one? Or evals scripts from ALL txs? Or something like that?
      Basically, yes, the last TX is the one that failed, but we need to collect collat for all the other ones, too. -}
    case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
      Right sLst ->
        {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
        {- isValid tx = evalScripts tx sLst = False -}
        whenFailureFree $
          when2Phase $ case evalPlutusScripts tx sLst of
            Passes _ ->
              failBecause $
                injectFailure @"ZONE" $
                  ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
            Fails ps fs -> do
              mapM_ (tellEvent . ZoneSuccessfulPlutusScriptsEvent @era) (nonEmpty ps)
              tellEvent (ZoneFailedPlutusScriptsEvent @era (scriptFailurePlutus <$> fs))
      Left info -> failBecause (injectFailure $ CollectErrors info)
    () <- pure $! traceEvent invalidEnd ()

    {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
    {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
    let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
        UTxO collouts = collOuts txBody
        DeltaCoin collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
    pure $!
      LedgerState
        us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
          { utxosUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
          {- fees + collateralFees -}
          , utxosFees = fees <> Coin collateralFees -- NEW to Babbage
          , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
          }
        certState

txInTxId :: TxIn era -> TxId era
txInTxId (TxIn x _) = x

-- get a set of TxIds containing all IDs of transaction in given list tb
getIDs :: EraTx era => [Tx era] -> Set (TxId (EraCrypto era))
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
mkFREdges :: (EraTx era, BabelEraTxBody era) => Tx era -> [Tx era] -> [(Tx era, Tx era)]
mkFREdges = mkEdges fulfillsTxBodyL

-- make all edges for all transactions
mkAllEdges :: (EraTx era, BabelEraTxBody era) => [Tx era] -> [Tx era] -> [(Tx era, Tx era)]
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
  uncurry (topSortTxs dges) updRES (srtd ++ [tx1])
  where
    updRES = updateRES tx1 (r : em) (removeTx tx1 (tx : rls))

instance
  ( Era era
  , STS (BabelLEDGERS era)
  , PredicateFailure (EraRule "LEDGERS" era) ~ ShelleyLedgersPredFailure era
  ) =>
  Embed (BabelLEDGERS era) (BabelZONE era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInBabelEvent