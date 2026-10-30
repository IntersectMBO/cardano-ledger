{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Eb (EB, EbPredFailure (..)) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, EB, EBLEDGERS)
import Cardano.Ledger.Dijkstra.Rules.EbLedgers (EbLedgersPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent)
import Cardano.Ledger.Dijkstra.State (EbAnnouncement (..))
import Cardano.Ledger.Shelley.LedgerState (
  EraCertState,
  LedgerState,
  NewEpochState,
  esLStateL,
  lsLastEbAnnouncementL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEnv (..))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  failBecause,
  judgmentContext,
  trans,
  (?!),
 )
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))

data EbPredFailure era
  = EbAnnouncementHashMismatch (Mismatch RelEQ (SafeHash EraIndependentEbReferences))
  | EbLedgersFailure (PredicateFailure (EraRule "EBLEDGERS" era))
  | EbWasNotAnnounced (SafeHash EraIndependentEbReferences)
  deriving (Generic)

deriving instance Eq (PredicateFailure (EraRule "EBLEDGERS" era)) => Eq (EbPredFailure era)

deriving instance Ord (PredicateFailure (EraRule "EBLEDGERS" era)) => Ord (EbPredFailure era)

deriving instance Show (PredicateFailure (EraRule "EBLEDGERS" era)) => Show (EbPredFailure era)

instance NFData (PredicateFailure (EraRule "EBLEDGERS" era)) => NFData (EbPredFailure era)

instance
  EncCBOR (PredicateFailure (EraRule "EBLEDGERS" era)) =>
  EncCBOR (EbPredFailure era)
  where
  encCBOR =
    encode . \case
      EbLedgersFailure f -> Sum (EbLedgersFailure @era) 0 !> To f
      EbAnnouncementHashMismatch mm -> Sum EbAnnouncementHashMismatch 1 !> To mm
      EbWasNotAnnounced h -> Sum EbWasNotAnnounced 2 !> To h

instance (Era era, DecCBOR (PredicateFailure (EraRule "EBLEDGERS" era))) => DecCBOR (EbPredFailure era) where
  decCBOR = decode $ Summands "EbPredFailure" $ \case
    0 -> SumD EbLedgersFailure <! From
    1 -> SumD EbAnnouncementHashMismatch <! From
    2 -> SumD EbWasNotAnnounced <! From
    k -> Invalid k

instance
  ( EraCertState era
  , EraEndorserBlock era
  , Eq (PredicateFailure (EraRule "EBLEDGERS" era))
  , Ord (PredicateFailure (EraRule "EBLEDGERS" era))
  , Show (PredicateFailure (EraRule "EBLEDGERS" era))
  , Embed (EraRule "EBLEDGERS" era) (EB era)
  , Environment (EraRule "EBLEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "EBLEDGERS" era) ~ LedgerState era
  , Signal (EraRule "EBLEDGERS" era) ~ EndorserBlock era
  , Monoid (BlockBodyLimits era)
  ) =>
  STS (EB era)
  where
  type State (EB era) = NewEpochState era
  type Signal (EB era) = EndorserBlock era
  type Environment (EB era) = ShelleyLedgersEnv era
  type Event (EB era) = DijkstraLedgerEvent era

  type PredicateFailure (EB era) = EbPredFailure era
  type BaseM (EB era) = ShelleyBase

  initialRules = []
  transitionRules =
    [ do
        TRC (env, state, eb) <- judgmentContext
        let ebHash = hashAnnotated (eb ^. referencesEndorserBlockL)
            ledgerState = state ^. nesEsL . esLStateL
        case ledgerState ^. lsLastEbAnnouncementL of
          SJust EbAnnouncement {ebAnnouncement} -> do
            let expectedHash = ebReferencesAnnouncementHash ebAnnouncement
            ebHash
              == expectedHash
                ?! EbAnnouncementHashMismatch
                  ( Mismatch
                      { mismatchSupplied = ebHash
                      , mismatchExpected = expectedHash
                      }
                  )
          SNothing -> failBecause $ EbWasNotAnnounced ebHash
        newLedgerState <-
          trans @(EraRule "EBLEDGERS" era) $ TRC (env, ledgerState, eb)
        pure $ state & nesEsL . esLStateL .~ newLedgerState
    ]

type instance EraRuleFailure "EB" DijkstraEra = EbPredFailure DijkstraEra

instance InjectRuleFailure "EB" EbPredFailure DijkstraEra

instance
  ( Era era
  , STS (EBLEDGERS era)
  , PredicateFailure (EraRule "EBLEDGERS" era) ~ EbLedgersPredFailure era
  ) =>
  Embed (EBLEDGERS era) (EB era)
  where
  wrapFailed = EbLedgersFailure
