{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Building the `EndorserBlock` that a ranking block announces, one validated transaction at a
-- time.
module Cardano.Ledger.Dijkstra.Block.Endorser.Builder (
  EraEndorserBlockBuilder (..),
  EbBuilderState,
  EbBuilderResult (..),
  mkInitEbBuilderStateDijkstra,
  snocTxEbBuilderValidatedDijkstra,
  finalizeEbBuilderDijkstra,
) where

import Cardano.Ledger.BaseTypes (Globals (..), Inject (..), ProtVer (..), ShelleyBase)
import Cardano.Ledger.Binary (EncCBOR (..), encodeMapLen, serialize')
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
  pattern EbReferences,
 )
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraBlockBodyLimits)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
import Cardano.Ledger.Dijkstra.Rules.Eb (EbPredFailure (..), validateEbReferencesSize)
import Cardano.Ledger.Dijkstra.Rules.EbLedgers (EbLedgersPredFailure, validateEbLimits)
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley.API.Mempool (
  ApplyTx (..),
  ValidatedTx,
  extractValidatedTx,
  getValidatedTxProtocolVersion,
  getValidatedTxStAnnTx,
  mkMempoolEnv,
 )
import Cardano.Ledger.Shelley.LedgerState (
  LedgerStateWithLimits (..),
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  nesEsL,
  nesLastTickSlotNoL,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.State (EraGov, utxoG)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  STS (..),
  SingEP (..),
  TRC (..),
  ValidationPolicy (..),
  applySTSOptsEither,
  globalAssertionPolicy,
 )
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Foldable (sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.SimpleOMap.Strict as SimpleOMap
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import Validation (Validation (..))

-- | The transactions added to an `EndorserBlock` so far, together with what is needed to check
-- the next one.
data EbBuilderState era = EbBuilderState
  { ebbsStAnnTxs :: !(StrictSeq (StAnnTx TopTx era))
  , ebbsReferencesEntriesSize :: !Int
  -- ^ Size of the encoded references of the transactions, without the map header
  , ebbsBlockBodyLimits :: !(BlockBodyLimits era)
  -- ^ Totals accumulated by @LEDGER@ over the transactions
  , ebbsNewEpochState :: !(NewEpochState era)
  -- ^ State after applying the transactions
  }
  deriving (Generic)

instance
  ( NFData (StAnnTx TopTx era)
  , NFData (BlockBodyLimits era)
  , NFData (NewEpochState era)
  ) =>
  NFData (EbBuilderState era)

data EbBuilderResult era
  = -- | The transaction was added to the `EndorserBlock`
    EbBuilderSuccess !(EbBuilderState era)
  | -- | Adding the transaction would exceed a limit of the `EndorserBlock`, which is reported as
    -- the failures that applying it would produce
    EbBuilderLimitExceeded !(NonEmpty (PredicateFailure (EraRule "EB" era)))
  | -- | The transaction is no longer valid on top of the transactions added before it
    EbBuilderTxError !(ApplyTxError era)
  deriving (Generic)

class (ApplyTx era, EraEndorserBlock era) => EraEndorserBlockBuilder era where
  -- | Start building an `EndorserBlock` on top of the state that it will be applied to.
  mkInitEbBuilderState :: NewEpochState era -> EbBuilderState era

  -- | Add a transaction validated by the mempool to the `EndorserBlock`. On failure the
  -- previous state can still be finalized.
  snocTxEbBuilderValidated ::
    Globals -> EbBuilderState era -> ValidatedTx era -> EbBuilderResult era

  -- | The `EndorserBlock` with the transactions added so far, or `Nothing` when none were.
  finalizeEbBuilder :: EbBuilderState era -> Maybe (EndorserBlock era)

mkInitEbBuilderStateDijkstra :: EraBlockBody era => NewEpochState era -> EbBuilderState era
mkInitEbBuilderStateDijkstra nes =
  EbBuilderState
    { ebbsStAnnTxs = mempty
    , ebbsReferencesEntriesSize = 0
    , ebbsBlockBodyLimits = mempty
    , ebbsNewEpochState = nes
    }

-- | Apply the transaction with the @MEMPOOL@ rule, reusing its state-derived annotations and
-- skipping its static checks unless the major protocol version changed since it was validated,
-- as `Cardano.Ledger.Shelley.API.Mempool.reapplyValidatedTx` does. Then check the limits of the
-- `EndorserBlock` in the order in which the @EB@ rule checks them.
snocTxEbBuilderValidatedDijkstra ::
  forall era.
  ( ApplyTx era
  , EraGov era
  , DijkstraEraPParams era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  , BlockBodyLimits era ~ DijkstraBlockBodyLimits era
  , STS (EraRule "MEMPOOL" era)
  , BaseM (EraRule "MEMPOOL" era) ~ ShelleyBase
  , Environment (EraRule "MEMPOOL" era) ~ LedgerEnv era
  , State (EraRule "MEMPOOL" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "MEMPOOL" era) ~ StAnnTx TopTx era
  , Inject (NonEmpty (PredicateFailure (EraRule "MEMPOOL" era))) (ApplyTxError era)
  , PredicateFailure (EraRule "EB" era) ~ EbPredFailure era
  , PredicateFailure (EraRule "EBLEDGERS" era) ~ EbLedgersPredFailure era
  ) =>
  Globals -> EbBuilderState era -> ValidatedTx era -> EbBuilderResult era
snocTxEbBuilderValidatedDijkstra globals EbBuilderState {..} validatedTx =
  case runReader (applySTSOptsEither @(EraRule "MEMPOOL" era) opts trc) globals of
    Left failures -> EbBuilderTxError $ inject failures
    Right (LedgerStateWithLimits newLedgerState newBlockBodyLimits) ->
      let tx = stAnnTx ^. txStAnnTxG
          referencesEntriesSize = ebbsReferencesEntriesSize + encodedSize (referenceEntry tx)
          referencesSize =
            encodedSize (encodeMapLen . fromIntegral $ length ebbsStAnnTxs + 1)
              + referencesEntriesSize
       in case sequenceA_
            [ first (fmap EbReferencesSizeTooBigInEB) $
                validateEbReferencesSize pp referencesSize
            , first (fmap EbLedgersFailure) $ validateEbLimits pp newBlockBodyLimits
            ] of
            Failure failures -> EbBuilderLimitExceeded failures
            Success () ->
              EbBuilderSuccess
                EbBuilderState
                  { ebbsStAnnTxs = ebbsStAnnTxs |> stAnnTx
                  , ebbsReferencesEntriesSize = referencesEntriesSize
                  , ebbsBlockBodyLimits = newBlockBodyLimits
                  , ebbsNewEpochState = ebbsNewEpochState & nesEsL . esLStateL .~ newLedgerState
                  }
  where
    pp = ebbsNewEpochState ^. nesEsL . curPParamsEpochStateL
    ledgerState = ebbsNewEpochState ^. nesEsL . esLStateL
    (validationPolicy, stAnnTx)
      | pvMajor (getValidatedTxProtocolVersion validatedTx) == pvMajor (pp ^. ppProtocolVersionL) =
          (ValidateSuchThat (notElem lblStatic), getValidatedTxStAnnTx validatedTx)
      | otherwise =
          ( ValidateAll
          , mkStAnnTx
              (epochInfo globals)
              (systemStart globals)
              pp
              (ledgerState ^. utxoG)
              mempty
              (extractValidatedTx validatedTx)
          )
    opts =
      ApplySTSOpts
        { asoAssertions = globalAssertionPolicy
        , asoValidation = validationPolicy
        , asoEvents = EPDiscard
        }
    trc =
      TRC
        ( mkMempoolEnv ebbsNewEpochState $ ebbsNewEpochState ^. nesLastTickSlotNoL
        , LedgerStateWithLimits ledgerState ebbsBlockBodyLimits
        , stAnnTx
        )
    encodedSize :: EncCBOR a => a -> Int
    encodedSize = BS.length . serialize' (eraProtVerLow @era)
    referenceEntry tx = encCBOR (hashAnnotated tx) <> encCBOR (tx ^. sizeTxF)

finalizeEbBuilderDijkstra ::
  ( EraEndorserBlock era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  ) =>
  EbBuilderState era -> Maybe (EndorserBlock era)
finalizeEbBuilderDijkstra EbBuilderState {ebbsStAnnTxs} = do
  guard . not $ null ebbsStAnnTxs
  let txs = (^. txStAnnTxG) <$> ebbsStAnnTxs
  pure $
    mkBasicEndorserBlock
      & transactionsEndorserBlockL .~ EbTransactions txs
      & referencesEndorserBlockL
        .~ EbReferences (SimpleOMap.fromFoldable $ (\tx -> (hashAnnotated tx, tx ^. sizeTxF)) <$> txs)
