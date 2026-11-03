{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Block (
  DijkstraApplyBlock (..),
  applyEndorserBlockEither,
  applyEndorserBlockNoValidation,
  EndorserBlockTransitionError (EndorserBlockTransitionError),
) where

import Cardano.Ledger.BaseTypes (Globals, ShelleyBase)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core (EraBlockBody, EraRule, eraProtVerLow)
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.BlockBody.Internal ()
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Shelley.API (ApplyBlock)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Control.DeepSeq (NFData, deepseq)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  STS (..),
  STSResult (..),
  SingEP (..),
  TRC (..),
  ValidationPolicy (..),
  applySTSOptsResult,
  globalAssertionPolicy,
 )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunk (..), NoThunks (..))

class
  ( ApplyBlock h era
  , EraEndorserBlock era
  , NFData (PredicateFailure (EraRule "EB" era))
  ) =>
  DijkstraApplyBlock h era
  where
  -- Supplied `NewEpochState` must be the one left by the block that announced
  -- the `EndorserBlock`, without ticking it any further, because the
  -- `EndorserBlock` is applied at the slot of that block.
  applyEndorserBlock ::
    SingEP ep ->
    ValidationPolicy ->
    Globals ->
    NewEpochState era ->
    EndorserBlock era ->
    (NewEpochState era, [PredicateFailure (EraRule "EB" era)], [Event (EraRule "EB" era)])
  default applyEndorserBlock ::
    ( STS (EraRule "EB" era)
    , BaseM (EraRule "EB" era) ~ ShelleyBase
    , Environment (EraRule "EB" era) ~ ()
    , State (EraRule "EB" era) ~ NewEpochState era
    , Signal (EraRule "EB" era) ~ EndorserBlock era
    ) =>
    SingEP ep ->
    ValidationPolicy ->
    Globals ->
    NewEpochState era ->
    EndorserBlock era ->
    (NewEpochState era, [PredicateFailure (EraRule "EB" era)], [Event (EraRule "EB" era)])
  applyEndorserBlock eventsPolicy validationPolicy globals newEpochState endorserBlock =
    (stsResultState, stsResultFailures, stsResultEvents)
    where
      opts =
        ApplySTSOpts
          { asoAssertions = globalAssertionPolicy
          , asoValidation = validationPolicy
          , asoEvents = eventsPolicy
          }
      STSResult {stsResultState, stsResultFailures, stsResultEvents} =
        flip runReader globals . applySTSOptsResult @(EraRule "EB" era) opts $
          TRC ((), newEpochState, endorserBlock)

applyEndorserBlockEither ::
  forall h era ep.
  DijkstraApplyBlock h era =>
  SingEP ep ->
  ValidationPolicy ->
  Globals ->
  NewEpochState era ->
  EndorserBlock era ->
  Either (EndorserBlockTransitionError era) (NewEpochState era, [Event (EraRule "EB" era)])
applyEndorserBlockEither eventsPolicy validationPolicy globals newEpochState endorserBlock =
  case failures of
    [] -> Right (newEpochStateResult, events)
    f : fs -> Left $ EndorserBlockTransitionError $ f :| fs
  where
    (newEpochStateResult, failures, events) =
      applyEndorserBlock @h eventsPolicy validationPolicy globals newEpochState endorserBlock

-- | Apply an `EndorserBlock` without any validation.
--
-- The caller implicitly guarantees that the `EndorserBlock` is valid for the supplied
-- `NewEpochState`, which is the case once a certificate for it is included in a ranking block.
applyEndorserBlockNoValidation ::
  forall h era.
  DijkstraApplyBlock h era =>
  Globals ->
  NewEpochState era ->
  EndorserBlock era ->
  NewEpochState era
applyEndorserBlockNoValidation globals newEpochState endorserBlock = newEpochStateResult
  where
    (newEpochStateResult, _failures, _events) =
      applyEndorserBlock @h EPDiscard ValidateNone globals newEpochState endorserBlock

newtype EndorserBlockTransitionError era
  = EndorserBlockTransitionErrorInternal (NonEmpty (PredicateFailure (EraRule "EB" era)))
  deriving (Generic)

pattern EndorserBlockTransitionError ::
  NFData (PredicateFailure (EraRule "EB" era)) =>
  NonEmpty (PredicateFailure (EraRule "EB" era)) ->
  EndorserBlockTransitionError era
pattern EndorserBlockTransitionError failures <-
  EndorserBlockTransitionErrorInternal failures
  where
    EndorserBlockTransitionError failures =
      failures `deepseq` EndorserBlockTransitionErrorInternal failures

{-# COMPLETE EndorserBlockTransitionError #-}

deriving stock instance
  Eq (PredicateFailure (EraRule "EB" era)) =>
  Eq (EndorserBlockTransitionError era)

deriving stock instance
  Show (PredicateFailure (EraRule "EB" era)) =>
  Show (EndorserBlockTransitionError era)

deriving via
  AllowThunk (EndorserBlockTransitionError era)
  instance
    NoThunks (EndorserBlockTransitionError era)

instance EncCBOR h => EncCBOR (Block h DijkstraEra) where
  encCBOR (Block h body) =
    encodeListLen 2 <> encCBOR h <> encCBOR body

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h DijkstraEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @DijkstraEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h, EraBlockBody DijkstraEra) =>
  DecCBOR (Annotator (Block h DijkstraEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 2) $ do
      header <- decCBOR
      body <- decCBOR
      pure $ Block <$> header <*> body
