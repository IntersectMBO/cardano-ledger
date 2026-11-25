{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Re-construction of an announced `EndorserBlock`, for Leios committee members that
-- need to decide whether to vote for it.
--
-- The committee member knows the `EbReferences` from the announcement, but not the
-- transactions themselves: those are fetched one by one.
-- This interface accepts them as they are resolved and reproduces, step by step, the verdict
-- that the EB rule will later reach on the same endorser block.
module Cardano.Ledger.Dijkstra.Block.Endorser.ReBuilder (
  EbReBuilderState,
  EbReBuilderInitError (..),
  EbReBuilderError (..),
  EbReBuilderResult (..),
  EbReBuilderTxError (..),
  EbReBuilderFinalizerError (..),
  EraEndorserBlockReBuilder (..),
  FullEbReBuilderError (..),
  reBuildFullEndorserBlock,
  -- remove after placeholders are removed
  EraEndorserBlockBuilder (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra ()
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams, ppMaxEndorserBlockTxsSizeL)
import Cardano.Ledger.Dijkstra.Rules.Eb (
  EbAlreadyApplied (..),
  EbAnnouncementHashMismatch (..),
  EbAnnouncementSizeMismatch (..),
  EbAnnouncementSlotMismatch (..),
  EbReferencesSizeTooBig (..),
  EbWasNotAnnounced (..),
  validateEbAnnouncementHash,
  validateEbAnnouncementSize,
  validateEbAnnouncementSlot,
  validateEbNotApplied,
  validateEbReferencesSize,
 )
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Plutus.ExUnits (OrdExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.API (ApplyTx (..), ValidatedTx)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State
import Control.State.Transition (PredicateFailure)
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.SimpleOMap.Strict as OMap
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import Validation (Validation (..), failureUnless, validationToEither)

-- | Running state for the re-construction of an announced `EndorserBlock`.
data EbReBuilderState era = EbReBuilderState
  { ebrbsEbReferences :: !(EbReferences era)
  -- ^ The announced references, as supplied at initialisation.
  , ebrbsTxHashes :: !(S.Vector (SafeHash EraIndependentTx))
  -- ^ The referenced transaction hashes, in the same order as in `EbReferences`
  , ebrbsTxSizes :: !(P.Vector Word32)
  -- ^ The declared transaction sizes, in the same order as `ebrbsTxHashes`.
  , ebrbsStAnnTxs :: !(Map (SafeHash EraIndependentTx) (StAnnTx TopTx era))
  -- ^ The transactions resolved so far.
  , ebrbsBlockBodyLimits :: !(BlockBodyLimits era)
  -- ^ Totals accumulated across the transactions applied so far.
  , ebrbsNewEpochState :: !(NewEpochState era)
  -- ^ The state the transactions applied so far have produced.
  }
  deriving (Generic)

ebrbsStAnnTxsL :: Lens' (EbReBuilderState era) (Map (SafeHash EraIndependentTx) (StAnnTx TopTx era))
ebrbsStAnnTxsL = lens ebrbsStAnnTxs $ \x y -> x {ebrbsStAnnTxs = y}

ebrbsBlockBodyLimitsL :: Lens' (EbReBuilderState era) (BlockBodyLimits era)
ebrbsBlockBodyLimitsL = lens ebrbsBlockBodyLimits $ \x y -> x {ebrbsBlockBodyLimits = y}

ebrbsNewEpochStateL :: Lens' (EbReBuilderState era) (NewEpochState era)
ebrbsNewEpochStateL = lens ebrbsNewEpochState $ \x y -> x {ebrbsNewEpochState = y}

data EbReBuilderInitError era
  = EbReBuilderInitNoAnnouncement !(EbWasNotAnnounced era)
  | EbReBuilderInitEbReferencesSizeTooBig !(EbReferencesSizeTooBig era)
  | EbReBuilderInitEbAnnouncementHashMismatch !(EbAnnouncementHashMismatch era)
  | EbReBuilderInitEbAnnouncementSizeMismatch !(EbAnnouncementSizeMismatch era)
  | EbReBuilderInitEbAnnouncementSlotMismatch !(EbAnnouncementSlotMismatch era)
  | EbReBuilderInitEbAlreadyApplied !(EbAlreadyApplied era)
  | -- | The declared transaction sizes add up to more than `ppMaxEndorserBlockTxsSizeL`
    EbReBuilderInitEbTxsSizeTooBig !(EbTxsSizeTooBig era)
  deriving (Eq, Show, Generic)

-- | An inconsistency between the endorser block being re-constructed and the references
-- that were announced for it, or a limit that the transactions applied so far exceed.
data EbReBuilderError era
  = -- | The supplied transaction is not the one the references name at this position.
    EbReBuilderTxHashMismatch !TxIx !(Mismatch RelEQ (SafeHash EraIndependentTx))
  | -- | The supplied transaction is not the size the references declare for it.
    EbReBuilderTxSizeMismatch !TxIx !(Mismatch RelEQ Word32)
  | EbReBuilderTxsSizeTooBig !(EbTxsSizeTooBig era)
  | EbReBuilderExUnitsTooBig !(EbExUnitsTooBig era)
  | EbReBuilderRefScriptsSizeTooBig !(EbRefScriptsSizeTooBig era)
  deriving (Eq, Show, Generic)

data EbReBuilderResult era
  = EbReBuilderSuccess !(EbReBuilderState era) !(ValidatedTx era)
  | -- | The endorser block is inconsistent with its announced references.
    EbReBuilderError !(EbReBuilderError era) !(ValidatedTx era)
  | -- | The transaction itself did not validate.
    EbReBuilderTxError !TxIx !(EbReBuilderTxError era)
  deriving (Generic)

-- | Produced when finalisation is initiated, but transactions are still expected.
newtype EbReBuilderFinalizerError era
  = -- Q: do we need to keep the order here?
    EbReBuilderMissingTxsError (NonEmpty (SafeHash EraIndependentTx))
  deriving (Eq, Show, Generic)

class EraEndorserBlockBuilder era => EraEndorserBlockReBuilder era where
  data EbReBuilderTxError era

  -- | Derives the initial state from the ledger state and the announced references.
  mkInitEbReBuilderState ::
    NewEpochState era ->
    EbReferences era ->
    Either (EbReBuilderInitError era) (EbReBuilderState era)

  getEbReBuilderStateNewEpochState :: EbReBuilderState era -> NewEpochState era

  -- | Adjusts the `UTxO` that the re-builder validates against.
  modifyEbReBuilderStateUTxO :: EbReBuilderState era -> (UTxO era -> UTxO era) -> EbReBuilderState era

  -- | Resolves the next transaction, checking it against its reference and applying it via the LEDGER rule
  snocTxEbReBuilder ::
    Globals ->
    EbReBuilderState era ->
    Either (ValidatedTx era) (Tx TopTx era) ->
    EbReBuilderResult era

  -- | Assembles the `EndorserBlock` and returns the state its transactions produced.
  finalizeEbReBuilder ::
    EbReBuilderState era ->
    Either (EbReBuilderFinalizerError era) (EndorserBlock era, NewEpochState era)

instance EraEndorserBlockReBuilder DijkstraEra where
  newtype EbReBuilderTxError DijkstraEra
    = DijkstraEbReBuilderTxError (NonEmpty (DijkstraLedgerPredFailure DijkstraEra))

  mkInitEbReBuilderState = mkInitEbReBuilderStateDijkstra

  getEbReBuilderStateNewEpochState EbReBuilderState {ebrbsNewEpochState} = ebrbsNewEpochState

  modifyEbReBuilderStateUTxO ebReBuilderState@EbReBuilderState {ebrbsNewEpochState} f =
    ebReBuilderState {ebrbsNewEpochState = ebrbsNewEpochState & utxoL %~ f}

  snocTxEbReBuilder = error "snocTxEbReBuilder: not implemented yet"

  finalizeEbReBuilder = finalizeEbReBuilderDijkstra

mkInitEbReBuilderStateDijkstra ::
  forall era.
  ( DijkstraEraPParams era
  , EraBlockBody era
  , EraEndorserBlock era
  , EraGov era
  ) =>
  NewEpochState era ->
  EbReferences era ->
  Either (EbReBuilderInitError era) (EbReBuilderState era)
mkInitEbReBuilderStateDijkstra nes ebRefs =
  case nes ^. nesEsL . esLStateL . lsLastEbAnnouncementL of
    SNothing ->
      Left $ EbReBuilderInitNoAnnouncement $ EbWasNotAnnounced $ hashAnnotated ebRefs
    SJust ebA -> do
      let announcement = ebA ^. ebAnnouncementL
      runEbTest EbReBuilderInitEbReferencesSizeTooBig $
        validateEbReferencesSize pp ebRefs
      runEbTest EbReBuilderInitEbAnnouncementHashMismatch $
        validateEbAnnouncementHash announcement ebRefs
      runEbTest EbReBuilderInitEbAnnouncementSizeMismatch $
        validateEbAnnouncementSize announcement ebRefs
      runEbTest EbReBuilderInitEbAlreadyApplied $
        validateEbNotApplied ebA
      runEbTest EbReBuilderInitEbAnnouncementSlotMismatch $
        validateEbAnnouncementSlot ebA lastTickSlotNo
      runEbTest EbReBuilderInitEbTxsSizeTooBig $ validateEbTxsSize pp totalTxsSize
      pure
        EbReBuilderState
          { ebrbsEbReferences = ebRefs
          , ebrbsTxHashes = txHashes
          , ebrbsTxSizes = txSizes
          , ebrbsStAnnTxs = Map.empty
          , ebrbsBlockBodyLimits = mempty
          , ebrbsNewEpochState = nes
          }
  where
    pp = nes ^. nesEsL . curPParamsEpochStateL
    lastTickSlotNo = nes ^. nesLastTickSlotNoL
    ebRefsCount = OMap.size (ebRefs ^. txRefsEbReferencesG)
    txSizes = P.fromListN ebRefsCount $ Foldable.toList $ OMap.toStrictSeq (ebRefs ^. txRefsEbReferencesG)
    txHashes = S.fromListN ebRefsCount $ Foldable.toList $ OMap.toStrictSeqOKeys (ebRefs ^. txRefsEbReferencesG)
    totalTxsSize = P.foldl' (+) 0 txSizes
    runEbTest toInitError = first (toInitError . NE.head) . validationToEither

finalizeEbReBuilderDijkstra ::
  forall era.
  EraEndorserBlock era =>
  EbReBuilderState era ->
  Either (EbReBuilderFinalizerError era) (EndorserBlock era, NewEpochState era)
finalizeEbReBuilderDijkstra EbReBuilderState {ebrbsEbReferences, ebrbsStAnnTxs, ebrbsNewEpochState} =
  case validationToEither resolvedTxs of
    Left missingTxs -> Left $ EbReBuilderMissingTxsError missingTxs
    Right txs -> Right (endorserBlock txs, newEpochState)
  where
    endorserBlock txs =
      mkBasicEndorserBlock
        & referencesEndorserBlockL .~ ebrbsEbReferences
        & transactionsEndorserBlockL .~ EbTransactions txs
    newEpochState =
      ebrbsNewEpochState & nesEsL . esLStateL . lsLastEbAnnouncementL %~ fmap (ebIsAppliedL .~ True)
    resolvedTxs =
      traverse resolveTx $ OMap.toStrictSeqOKeys (ebrbsEbReferences ^. txRefsEbReferencesG)
    resolveTx ::
      SafeHash EraIndependentTx -> Validation (NonEmpty (SafeHash EraIndependentTx)) (Tx TopTx era)
    resolveTx txHash = case Map.lookup txHash ebrbsStAnnTxs of
      Nothing -> Failure $ pure txHash
      Just stAnnTx -> Success $ stAnnTx ^. txStAnnTxG

data FullEbReBuilderError era
  = FullEbInitError (EbReBuilderInitError era)
  | FullEbReBuilderError (EbReBuilderError era)
  | FullEbFinalizerError (EbReBuilderFinalizerError era)

reBuildFullEndorserBlock ::
  Globals ->
  NewEpochState era ->
  EbReferences era ->
  Map (SafeHash EraIndependentTx) (Either (ValidatedTx era) (Tx TopTx era)) ->
  Either (FullEbReBuilderError era) (EndorserBlock era, NewEpochState era)
reBuildFullEndorserBlock = error "reBuildFullEndorserBlock: not implemented yet"

--------------------------------------------------------------------------------------
-- Placeholders, remove with https://github.com/IntersectMBO/cardano-ledger/pull/6270
-------------------------------------------------------------------------------------

newtype EbTxsSizeTooBig era = EbTxsSizeTooBig (Mismatch RelLTEQ Word32)
  deriving (Eq, Show, Generic)

newtype EbExUnitsTooBig era = EbExUnitsTooBig (Mismatch RelLTEQ OrdExUnits)
  deriving (Eq, Show, Generic)

newtype EbRefScriptsSizeTooBig era = EbRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  deriving (Eq, Show, Generic)

validateEbTxsSize ::
  DijkstraEraPParams era => PParams era -> Word32 -> Test (EbTxsSizeTooBig era)
validateEbTxsSize pp txsSize =
  let maxTxsSize = pp ^. ppMaxEndorserBlockTxsSizeL
   in failureUnless (txsSize <= maxTxsSize) $
        EbTxsSizeTooBig
          Mismatch
            { mismatchSupplied = txsSize
            , mismatchExpected = maxTxsSize
            }

--------------------------------------------------------------------------------------
-- Placeholders, remove with https://github.com/IntersectMBO/cardano-ledger/pull/6284
-------------------------------------------------------------------------------------

data EbBuilderState era = EbBuilderState
  { ebbsStAnnTxs :: !(StrictSeq (StAnnTx TopTx era))
  , ebbsReferencesEntriesSize :: !Int
  , ebbsBlockBodyLimits :: !(BlockBodyLimits era)
  , ebbsNewEpochState :: !(NewEpochState era)
  }
  deriving (Generic)

data EbBuilderResult era
  = EbBuilderSuccess !(EbBuilderState era)
  | EbBuilderLimitExceeded !(NonEmpty (PredicateFailure (EraRule "EB" era)))
  | EbBuilderTxError !(ApplyTxError era)
  deriving (Generic)

class (ApplyTx era, EraEndorserBlock era) => EraEndorserBlockBuilder era where
  mkInitEbBuilderState :: NewEpochState era -> EbBuilderState era
  snocTxEbBuilderValidated :: Globals -> EbBuilderState era -> ValidatedTx era -> EbBuilderResult era
  finalizeEbBuilder :: EbBuilderState era -> Maybe (EndorserBlock era)

instance EraEndorserBlockBuilder DijkstraEra where
  mkInitEbBuilderState = error "placeholder"
  snocTxEbBuilderValidated = error "placeholder"
  finalizeEbBuilder = error "placeholder"
