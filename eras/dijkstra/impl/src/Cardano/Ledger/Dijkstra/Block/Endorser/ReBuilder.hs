{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Cardano.Ledger.Conway.BlockBody
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra ()
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
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
import Cardano.Ledger.Dijkstra.Rules.EbLedgers (
  EbExUnitsTooBig,
  EbRefScriptsSizeTooBig,
  EbTxsSizeTooBig,
  validateEbExUnits,
  validateEbRefScriptsSize,
  validateEbTxsSize,
 )
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Rules.ValidationMode
import Cardano.Ledger.Shelley.API (
  ApplyTx (..),
  LedgerEnv (..),
  ValidatedTx,
  extractValidatedTx,
  getValidatedTxProtocolVersion,
  getValidatedTxStAnnTx,
  unsafeMakeValidatedTx,
 )
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Control.Monad (unless)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition
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
import Validation (Validation (..), validationToEither)

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
  | -- | No reference at this position: more transactions were supplied than the
    -- announced references name.
    EbReBuilderNoSuchReference !Int
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

  mkEbReBuilderTxError :: NonEmpty (PredicateFailure (EraRule "LEDGER" era)) -> EbReBuilderTxError era

instance EraEndorserBlockReBuilder DijkstraEra where
  newtype EbReBuilderTxError DijkstraEra
    = DijkstraEbReBuilderTxError (NonEmpty (DijkstraLedgerPredFailure DijkstraEra))

  mkInitEbReBuilderState = mkInitEbReBuilderStateDijkstra

  getEbReBuilderStateNewEpochState EbReBuilderState {ebrbsNewEpochState} = ebrbsNewEpochState

  modifyEbReBuilderStateUTxO ebReBuilderState@EbReBuilderState {ebrbsNewEpochState} f =
    ebReBuilderState {ebrbsNewEpochState = ebrbsNewEpochState & utxoL %~ f}

  snocTxEbReBuilder = snocTxEbReBuilderDijkstra

  finalizeEbReBuilder = finalizeEbReBuilderDijkstra
  mkEbReBuilderTxError = DijkstraEbReBuilderTxError

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
      runTestEither EbReBuilderInitEbReferencesSizeTooBig $
        validateEbReferencesSize pp ebRefs
      runTestEither EbReBuilderInitEbAnnouncementHashMismatch $
        validateEbAnnouncementHash announcement ebRefs
      runTestEither EbReBuilderInitEbAnnouncementSizeMismatch $
        validateEbAnnouncementSize announcement ebRefs
      runTestEither EbReBuilderInitEbAlreadyApplied $
        validateEbNotApplied ebA
      runTestEither EbReBuilderInitEbAnnouncementSlotMismatch $
        validateEbAnnouncementSlot ebA lastTickSlotNo
      runTestEither EbReBuilderInitEbTxsSizeTooBig $ validateEbTxsSize pp totalTxsSize
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

snocTxEbReBuilderDijkstra ::
  forall era.
  ( EraGov era
  , EraEndorserBlockReBuilder era
  , DijkstraEraPParams era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  , STS (EraRule "LEDGER" era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , BlockBodyLimits era ~ ConwayBlockBodyLimits era
  ) =>
  Globals ->
  EbReBuilderState era ->
  Either (ValidatedTx era) (Tx TopTx era) ->
  EbReBuilderResult era
snocTxEbReBuilderDijkstra
  globals
  ebrbState@EbReBuilderState
    { ebrbsTxHashes
    , ebrbsTxSizes
    , ebrbsStAnnTxs
    , ebrbsBlockBodyLimits
    , ebrbsNewEpochState
    }
  validatedOrTx =
    either id id $ do
      (txIx, claimedTxHash, claimedTxSize) <- validateIxAgainstEbReferences
      validateClaimedSizeAndHash txIx claimedTxHash claimedTxSize
      LedgerStateWithLimits newLedgerState newLimits <- runLedgerRule txIx
      validateBlockLimits newLimits
      pure $
        EbReBuilderSuccess
          ( ebrbState
              & ebrbsStAnnTxsL %~ Map.insert txHash stAnnTx
              & ebrbsBlockBodyLimitsL .~ newLimits
              & ebrbsNewEpochStateL . nesEsL . esLStateL .~ newLedgerState
          )
          validatedTx
    where
      validateIxAgainstEbReferences ::
        Either (EbReBuilderResult era) (TxIx, SafeHash EraIndependentTx, Word32)
      validateIxAgainstEbReferences = do
        let expectedTxIx = Map.size ebrbsStAnnTxs
        maybe (Left (failWith (EbReBuilderNoSuchReference expectedTxIx))) Right $ do
          hash <- ebrbsTxHashes S.!? expectedTxIx
          size <- ebrbsTxSizes P.!? expectedTxIx
          index <- txIxFromIntegral @Int @Maybe expectedTxIx
          pure (index, hash, size)
      runLedgerRule :: TxIx -> Either (EbReBuilderResult era) (LedgerStateWithLimits era)
      runLedgerRule txIx = do
        let stsOpts =
              ApplySTSOpts
                { asoAssertions = globalAssertionPolicy
                , asoValidation = validationPolicy
                , asoEvents = EPDiscard
                }
            runLedger =
              flip runReader globals $
                applySTSOptsEither @(EraRule "LEDGER" era)
                  stsOpts
                  (TRC (ledgerEnv, LedgerStateWithLimits ledgerState ebrbsBlockBodyLimits, stAnnTx))
        case runLedger of
          Left predFailures ->
            Left (EbReBuilderTxError txIx (mkEbReBuilderTxError predFailures))
          Right lswl -> Right lswl
      validateBlockLimits :: ConwayBlockBodyLimits era -> Either (EbReBuilderResult era) ()
      validateBlockLimits ConwayBlockBodyLimits {cbblTxsSize, cbblExUnits, cbblRefScriptsSize} = do
        runTestEither (failWith . EbReBuilderTxsSizeTooBig) $ validateEbTxsSize pp cbblTxsSize
        runTestEither (failWith . EbReBuilderExUnitsTooBig) $ validateEbExUnits pp cbblExUnits
        runTestEither (failWith . EbReBuilderRefScriptsSizeTooBig) $
          validateEbRefScriptsSize pp cbblRefScriptsSize
      validateClaimedSizeAndHash ::
        TxIx -> SafeHash EraIndependentTx -> Word32 -> Either (EbReBuilderResult era) ()
      validateClaimedSizeAndHash txIx claimedTxHash claimedTxSize = do
        unless (txHash == claimedTxHash) $
          Left $
            failWith $
              EbReBuilderTxHashMismatch
                txIx
                Mismatch {mismatchSupplied = txHash, mismatchExpected = claimedTxHash}
        let txSize = originalBytesSize32 tx
        unless (txSize == claimedTxSize) $
          Left $
            failWith $
              EbReBuilderTxSizeMismatch
                txIx
                Mismatch {mismatchSupplied = txSize, mismatchExpected = claimedTxSize}
      pp = ebrbsNewEpochState ^. nesEsL . curPParamsEpochStateL
      ledgerEnv =
        Shelley.LedgerEnv
          { ledgerSlotNo = ebrbsNewEpochState ^. nesLastTickSlotNoL
          , ledgerEpochNo = Nothing
          , ledgerIx = minBound -- this is not actually used in LEDGER rule starting with conway
          , ledgerPp = pp
          , ledgerAccount = ebrbsNewEpochState ^. chainAccountStateG
          }
      ledgerState = ebrbsNewEpochState ^. nesEsL . esLStateL
      (validationPolicy, validatedTx) = case validatedOrTx of
        Left vTx
          | pvMajor (getValidatedTxProtocolVersion vTx) == pvMajor (pp ^. ppProtocolVersionL) ->
              (ValidateSuchThat (notElem lblStatic), vTx)
        Left vTx ->
          (ValidateAll, mkValidatedTx (extractValidatedTx vTx))
        Right topTx ->
          (ValidateAll, mkValidatedTx topTx)
      mkValidatedTx = unsafeMakeValidatedTx globals ledgerEnv ledgerState
      stAnnTx = getValidatedTxStAnnTx validatedTx
      tx = stAnnTx ^. txStAnnTxG
      txHash = hashAnnotated tx
      failWith e = EbReBuilderError e validatedTx

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

runTestEither :: (failure -> e) -> Test failure -> Either e ()
runTestEither toError = first (toError . NE.head) . validationToEither

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
