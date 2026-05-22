{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interface to the Shelley ledger for the purposes of managing a Shelley
-- mempool.
module Cardano.Ledger.Shelley.API.Mempool (
  applyTx,
  applyTxWithFullValidation,
  reapplyValidatedTx,
  reapplyTx,
  ApplyTx (..),
  ApplyTxError (..),
  Validated,
  ValidatedTx,
  getValidatedTxStAnnTx,
  getValidatedTxProtocolVersion,
  getValidatedTxSlotNo,
  extractTx,
  extractValidatedTx,
  coerceValidated,
  translateValidated,
  ruleApplyTxValidation,
  defaultApplyTxWithValidation,
  defaultReapplyValidatedTx,

  -- * Exports for testing
  MempoolEnv,
  MempoolState,
  unsafeMakeValidated,
  unsafeMakeValidatedTx,

  -- * Exports for compatibility
  mkMempoolEnv,
  mkMempoolState,
  overNewEpochState,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules.Ledger (
  LedgerEnv (..),
  ShelleyLedgerPredFailure,
  ledgerPpL,
  ledgerSlotNoL,
 )
import Cardano.Ledger.State
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (Coercible, coerce)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | A transaction that has been validated against some chain state.
data ValidatedTx era = ValidatedTx
  { vtStAnnTx :: !(StAnnTx TopTx era)
  -- ^ The transaction paired with the state-derived annotation produced during validation
  , vtProtocolVersion :: !ProtVer
  -- ^ Protocol version under which the validation took place
  , vtSlotNo :: !SlotNo
  -- ^ Slot number under which the validation took place
  }
  deriving (Generic)

deriving instance Eq (StAnnTx TopTx era) => Eq (ValidatedTx era)

deriving instance Show (StAnnTx TopTx era) => Show (ValidatedTx era)

instance NFData (StAnnTx TopTx era) => NFData (ValidatedTx era)

instance NoThunks (StAnnTx TopTx era) => NoThunks (ValidatedTx era)

getValidatedTxStAnnTx :: ValidatedTx era -> StAnnTx TopTx era
getValidatedTxStAnnTx = vtStAnnTx

getValidatedTxProtocolVersion :: ValidatedTx era -> ProtVer
getValidatedTxProtocolVersion = vtProtocolVersion

getValidatedTxSlotNo :: ValidatedTx era -> SlotNo
getValidatedTxSlotNo = vtSlotNo

extractValidatedTx :: EraTx era => ValidatedTx era -> Tx TopTx era
extractValidatedTx validatedTx = getValidatedTxStAnnTx validatedTx ^. txStAnnTxG

-- | A newtype which indicates that a transaction has been validated against
-- some chain state.
newtype Validated tx = Validated tx
  deriving (Eq, NoThunks, Show, NFData)
{-# DEPRECATED Validated "Use 'ValidatedTx' instead." #-}

-- | Extract the underlying unvalidated Tx.
extractTx :: Validated tx -> tx
extractTx (Validated tx) = tx
{-# DEPRECATED extractTx "Use 'extractValidatedTx'" #-}

coerceValidated :: Coercible a b => Validated a -> Validated b
coerceValidated (Validated a) = Validated $ coerce a
{-# DEPRECATED coerceValidated "'Validated' is deprecated; switch to 'ValidatedTx'." #-}

-- Don't use this except in Testing to make Arbitrary instances, etc.
unsafeMakeValidated :: tx -> Validated tx
unsafeMakeValidated = Validated
{-# DEPRECATED unsafeMakeValidated "Use 'unsafeMakeValidatedTx' instead." #-}

-- | Build a 'ValidatedTx' without running the LEDGER rule - should only be used for testing.
unsafeMakeValidatedTx ::
  ApplyTx era =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Tx TopTx era ->
  ValidatedTx era
unsafeMakeValidatedTx globals env state tx =
  ValidatedTx
    { vtStAnnTx =
        mkStAnnTx
          (epochInfo globals)
          (systemStart globals)
          (env ^. ledgerPpL)
          (state ^. utxoG)
          tx
    , vtProtocolVersion = env ^. ledgerPpL . ppProtocolVersionL
    , vtSlotNo = env ^. ledgerSlotNoL
    }

-- | Translate a validated transaction across eras.
--
-- This is not a `TranslateEra` instance since `Validated` is not itself
-- era-parametrised.
translateValidated ::
  forall era f.
  TranslateEra era f =>
  TranslationContext era ->
  Validated (f (PreviousEra era)) ->
  Except (TranslationError era f) (Validated (f era))
translateValidated ctx (Validated tx) = Validated <$> translateEra @era ctx tx
{-# DEPRECATED
  translateValidated
  "Translation of `Validated` does not make sense. It must be fully re-validated in a new era"
  #-}

class
  ( EraTx era
  , Eq (ApplyTxError era)
  , Show (ApplyTxError era)
  , Typeable (ApplyTxError era)
  , Semigroup (ApplyTxError era)
  , EncCBOR (ApplyTxError era)
  , DecCBOR (ApplyTxError era)
  ) =>
  ApplyTx era
  where
  data ApplyTxError era

  mkStAnnTx ::
    EpochInfo (Either Text) ->
    SystemStart ->
    PParams era ->
    UTxO era ->
    Tx TopTx era ->
    StAnnTx TopTx era

  -- | Validate a transaction against a mempool state for given STS options.
  --
  -- /Warning/ - This function is only safe when `ValidateAll` policy is supplied,
  -- otherwise invariant for `ValidatedTx` could be violated.
  -- It will always be safer to use `applyTxWithFullValidation` instead.
  internalApplyTxWithValidation ::
    ValidationPolicy ->
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    Tx TopTx era ->
    Either (ApplyTxError era) (MempoolState era, ValidatedTx era)

  -- | Reapply a previously validated transaction.
  --
  -- /Warning/ - Should not be used directly. `reapplyValidatedTx` should be used instead.
  internalReapplyValidatedTx ::
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    ValidatedTx era ->
    Either (ApplyTxError era) (MempoolState era)

  -- | Validate a transaction against a mempool state for given STS
  -- options, and return the new mempool state, a "validated" 'TxInBlock
  applyTxValidation ::
    ValidationPolicy ->
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    StAnnTx TopTx era ->
    Either (ApplyTxError era) (MempoolState era, Validated (Tx TopTx era))
  applyTxValidation policy globals env state stAnnTx =
    fmap (\vtx -> Validated (vtStAnnTx vtx ^. txStAnnTxG))
      <$> internalApplyTxWithValidation policy globals env state (stAnnTx ^. txStAnnTxG)

{-# DEPRECATED applyTxValidation "Use 'internalApplyTxWithValidation' instead." #-}

ruleApplyTxValidation ::
  forall rule era.
  ( EraTx era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , Environment (EraRule rule era) ~ LedgerEnv era
  , State (EraRule rule era) ~ MempoolState era
  , Signal (EraRule rule era) ~ StAnnTx TopTx era
  ) =>
  ValidationPolicy ->
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  StAnnTx TopTx era ->
  Either (NonEmpty (PredicateFailure (EraRule rule era))) (MempoolState era, ValidatedTx era)
ruleApplyTxValidation validationPolicy globals env state stAnnTx =
  let opts =
        ApplySTSOpts
          { asoAssertions = globalAssertionPolicy
          , asoValidation = validationPolicy
          , asoEvents = EPDiscard
          }
      result =
        flip runReader globals
          . applySTSOptsEither @(EraRule rule era) opts
          $ TRC (env, state, stAnnTx)
      validatedTx =
        ValidatedTx
          { vtStAnnTx = stAnnTx
          , vtProtocolVersion = env ^. ledgerPpL . ppProtocolVersionL
          , vtSlotNo = env ^. ledgerSlotNoL
          }
   in fmap (,validatedTx) result

-- | A default implementation for 'internalApplyTxWithValidation', parameterised by the
-- STS rule to run and a wrapper that lifts predicate failures into the era's
-- 'ApplyTxError'.
defaultApplyTxWithValidation ::
  forall rule era.
  ( ApplyTx era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , Environment (EraRule rule era) ~ LedgerEnv era
  , State (EraRule rule era) ~ MempoolState era
  , Signal (EraRule rule era) ~ StAnnTx TopTx era
  ) =>
  (NonEmpty (PredicateFailure (EraRule rule era)) -> ApplyTxError era) ->
  ValidationPolicy ->
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Tx TopTx era ->
  Either (ApplyTxError era) (MempoolState era, ValidatedTx era)
defaultApplyTxWithValidation wrap validationPolicy globals env state tx =
  let stAnnTx =
        mkStAnnTx
          (epochInfo globals)
          (systemStart globals)
          (env ^. ledgerPpL)
          (state ^. utxoG)
          tx
   in first wrap $
        ruleApplyTxValidation @rule validationPolicy globals env state stAnnTx

-- | A default implementation for 'internalReapplyValidatedTx', parameterised by the
-- STS rule to run and a wrapper that lifts predicate failures into the era's
-- 'ApplyTxError'.
defaultReapplyValidatedTx ::
  forall rule era.
  ( ApplyTx era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , Environment (EraRule rule era) ~ LedgerEnv era
  , State (EraRule rule era) ~ MempoolState era
  , Signal (EraRule rule era) ~ StAnnTx TopTx era
  ) =>
  (NonEmpty (PredicateFailure (EraRule rule era)) -> ApplyTxError era) ->
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  ValidatedTx era ->
  Either (ApplyTxError era) (MempoolState era)
defaultReapplyValidatedTx wrap globals env state vtx =
  fst
    <$> first
      wrap
      ( ruleApplyTxValidation @rule
          (ValidateSuchThat (notElem lblStatic))
          globals
          env
          state
          (vtStAnnTx vtx)
      )

instance ApplyTx ShelleyEra where
  newtype ApplyTxError ShelleyEra = ShelleyApplyTxError (NonEmpty (ShelleyLedgerPredFailure ShelleyEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx _ _ _ _ = id

  internalApplyTxWithValidation validationPolicy globals env state tx =
    let stAnnTx =
          mkStAnnTx
            (epochInfo globals)
            (systemStart globals)
            (env ^. ledgerPpL)
            (state ^. utxoG)
            tx
     in first ShelleyApplyTxError $
          ruleApplyTxValidation @"LEDGER" validationPolicy globals env state stAnnTx

  internalReapplyValidatedTx = defaultReapplyValidatedTx @"LEDGER" ShelleyApplyTxError

type MempoolEnv era = LedgerEnv era

type MempoolState era = LedgerState era

-- | Construct the environment used to validate transactions from the full
-- ledger state.
--
-- Note that this function also takes a slot. During slot validation, the slot
-- given here is the slot of the block containing the transactions. This slot is
-- used for quite a number of things, but in general these do not determine the
-- validity of the transaction. There are two exceptions:
--
-- - Each transaction has a ttl (time-to-live) value. If the slot is beyond this
--   value, then the transaction is invalid.
-- - If the transaction contains a protocol update proposal, then it may only be
--   included until a certain number of slots before the end of the epoch. A
--   protocol update proposal submitted after this is considered invalid.
mkMempoolEnv ::
  EraGov era =>
  NewEpochState era ->
  SlotNo ->
  MempoolEnv era
mkMempoolEnv
  NewEpochState {nesEs}
  slot =
    LedgerEnv
      { ledgerSlotNo = slot
      , ledgerEpochNo = Nothing
      , ledgerIx = minBound
      , ledgerPp = nesEs ^. curPParamsEpochStateL
      , ledgerAccount = esChainAccountState nesEs
      }

-- | Construct a mempool state from the wider ledger state.
--
--   The given mempool state may then be evolved using 'applyTxs', but should be
--   regenerated when the ledger state gets updated (e.g. through application of
--   a new block).
mkMempoolState :: NewEpochState era -> MempoolState era
mkMempoolState NewEpochState {nesEs} = esLState nesEs

-- | Transform a function over mempool states to one over the full
-- 'NewEpochState'.
overNewEpochState ::
  Functor f =>
  (MempoolState era -> f (MempoolState era)) ->
  NewEpochState era ->
  f (NewEpochState era)
overNewEpochState f st = do
  f (mkMempoolState st)
    <&> \ls ->
      st
        { nesEs =
            (nesEs st)
              { esLState = ls
              }
        }

-- | Validate a transaction against a mempool state and return the new
-- mempool state together with a 'ValidatedTx'
applyTxWithFullValidation ::
  ApplyTx era =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Tx TopTx era ->
  Either (ApplyTxError era) (MempoolState era, ValidatedTx era)
applyTxWithFullValidation = internalApplyTxWithValidation ValidateAll

-- | Reapply a previously validated transaction, skipping static checks.
-- Use the state-derived annotations in `StAnnTx` if the current protocol version
-- matches the one in `ValidatedTx`, otherwise reconstruct `StAnnTx.
-- If major protocol version has changed from when `ValidatedTx`
-- was constructed, then full validation is triggered again.
reapplyValidatedTx ::
  (ApplyTx era, EraGov era) =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  ValidatedTx era ->
  Either (ApplyTxError era) (MempoolState era)
reapplyValidatedTx globals env ledgerState vtx
  | pvMajor (vtProtocolVersion vtx) == pvMajor currentPv =
      internalReapplyValidatedTx globals env ledgerState vtx
  | otherwise =
      fst
        <$> internalApplyTxWithValidation
          ValidateAll
          globals
          env
          ledgerState
          (vtStAnnTx vtx ^. txStAnnTxG)
  where
    currentPv = ledgerState ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppProtocolVersionL

-- | Validate a transaction against a mempool state using default STS options
-- and return both the new mempool state and a "validated" 'TxInBlock'.
--
-- The meaning of being "validated" depends on the era. In general, a
-- 'TxInBlock' has had all checks run, and can now only fail due to checks
-- which depend on the state; most notably, that UTxO inputs disappear.
applyTx ::
  ApplyTx era =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Tx TopTx era ->
  Either (ApplyTxError era) (MempoolState era, Validated (Tx TopTx era))
applyTx globals env state tx = do
  (mempoolState, vtx) <- internalApplyTxWithValidation ValidateAll globals env state tx
  pure (mempoolState, Validated (vtStAnnTx vtx ^. txStAnnTxG))
{-# DEPRECATED applyTx "Use 'applyTxWithFullValidation' instead." #-}

-- | Reapply a previously validated 'Tx'.
--
--   This applies the (validated) transaction to a new mempool state. It may
--   fail due to the mempool state changing (for example, a needed output
--   having already been spent). It does not fail due to any static check
--   (such as cryptographic checks).
reapplyTx ::
  ApplyTx era =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Validated (Tx TopTx era) ->
  Either (ApplyTxError era) (MempoolState era)
reapplyTx globals env state (Validated tx) =
  fst <$> internalApplyTxWithValidation (ValidateSuchThat (notElem lblStatic)) globals env state tx
{-# DEPRECATED reapplyTx "Use 'reapplyValidatedTx' instead." #-}
