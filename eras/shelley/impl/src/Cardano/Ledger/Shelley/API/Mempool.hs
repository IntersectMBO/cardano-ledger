{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
  reapplyTx,
  ApplyTx (..),
  ApplyTxError (..),
  Validated,
  extractTx,
  coerceValidated,
  translateValidated,
  ruleApplyTxValidation,

  -- * Exports for testing
  MempoolEnv,
  MempoolState,
  unsafeMakeValidated,

  -- * Exports for compatibility
  mkMempoolEnv,
  mkMempoolState,
  overNewEpochState,
) where

import Cardano.Ledger.BaseTypes (Globals, ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, curPParamsEpochStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv, ShelleyLedgerPredFailure)
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import Cardano.Ledger.Shelley.State ()
import Cardano.Ledger.Slot (SlotNo)
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (Coercible, coerce)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | A newtype which indicates that a transaction has been validated against
-- some chain state.
newtype Validated tx = Validated tx
  deriving (Eq, NoThunks, Show, NFData)

-- | Extract the underlying unvalidated Tx.
extractTx :: Validated tx -> tx
extractTx (Validated tx) = tx

coerceValidated :: Coercible a b => Validated a -> Validated b
coerceValidated (Validated a) = Validated $ coerce a

-- Don't use this except in Testing to make Arbitrary instances, etc.
unsafeMakeValidated :: tx -> Validated tx
unsafeMakeValidated = Validated

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

class
  ( EraTx era
  , Eq (ApplyTxError era)
  , Show (ApplyTxError era)
  , Typeable (ApplyTxError era)
  ) =>
  ApplyTx era
  where
  data ApplyTxError era

  -- | Validate a transaction against a mempool state and for given STS options,
  -- and return the new mempool state, a "validated" 'TxInBlock' and,
  -- depending on the passed options, the emitted events.
  applyTxValidation ::
    ValidationPolicy ->
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    Tx TopTx era ->
    Either (ApplyTxError era) (MempoolState era, Validated (Tx TopTx era))

ruleApplyTxValidation ::
  forall rule era.
  ( STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , Environment (EraRule rule era) ~ LedgerEnv era
  , State (EraRule rule era) ~ MempoolState era
  , Signal (EraRule rule era) ~ Tx TopTx era
  ) =>
  ValidationPolicy ->
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Tx TopTx era ->
  Either (NonEmpty (PredicateFailure (EraRule rule era))) (MempoolState era, Validated (Tx TopTx era))
ruleApplyTxValidation validationPolicy globals env state tx =
  let opts =
        ApplySTSOpts
          { asoAssertions = globalAssertionPolicy
          , asoValidation = validationPolicy
          , asoEvents = EPDiscard
          }
      result =
        flip runReader globals
          . applySTSOptsEither @(EraRule rule era) opts
          $ TRC (env, state, tx)
   in fmap (,Validated tx) result

instance ApplyTx ShelleyEra where
  newtype ApplyTxError ShelleyEra = ShelleyApplyTxError (NonEmpty (ShelleyLedgerPredFailure ShelleyEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR)
  applyTxValidation validationPolicy globals env state tx =
    first ShelleyApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

type MempoolEnv era = Ledger.LedgerEnv era

type MempoolState era = LedgerState.LedgerState era

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
  LedgerState.NewEpochState
    { LedgerState.nesEs
    }
  slot =
    Ledger.LedgerEnv
      { Ledger.ledgerSlotNo = slot
      , Ledger.ledgerEpochNo = Nothing
      , Ledger.ledgerIx = minBound
      , Ledger.ledgerPp = nesEs ^. curPParamsEpochStateL
      , Ledger.ledgerAccount = LedgerState.esChainAccountState nesEs
      }

-- | Construct a mempool state from the wider ledger state.
--
--   The given mempool state may then be evolved using 'applyTxs', but should be
--   regenerated when the ledger state gets updated (e.g. through application of
--   a new block).
mkMempoolState :: NewEpochState era -> MempoolState era
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} = LedgerState.esLState nesEs

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
        { LedgerState.nesEs =
            (LedgerState.nesEs st)
              { LedgerState.esLState = ls
              }
        }

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
applyTx = applyTxValidation ValidateAll

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
  fst <$> applyTxValidation (ValidateSuchThat (notElem lblStatic)) globals env state tx
