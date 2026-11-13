{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Eb (
  EB,
  EbPredFailure (..),
  EbWasNotAnnounced (..),
  EbReferencesSizeTooBig (..),
  EbAnnouncementSlotMismatch (..),
  EbAnnouncementHashMismatch (..),
  EbAnnouncementSizeMismatch (..),
  EbAlreadyApplied (..),
  validateEbReferences,
  validateEbAnnouncementSlot,
) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (
  EbReferencesAnnouncement,
  ebReferencesAnnouncementHashL,
  ebReferencesAnnouncementSizeL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, EB, EBLEDGERS)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams, ppMaxEndorserBlockReferencesSizeL)
import Cardano.Ledger.Dijkstra.Rules.EbLedgers (EbLedgersPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent)
import Cardano.Ledger.Dijkstra.State (ebAnnouncementL, ebIsAppliedL)
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.LedgerState (
  EraCertState,
  LedgerState,
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  lsLastEbAnnouncementL,
  nesEsL,
  nesLastTickSlotNoL,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEnv (..))
import Cardano.Ledger.Slot (SlotNo, epochFromSlot)
import Cardano.Ledger.State (EbAnnouncement, EraGov, chainAccountStateG, ebAnnouncementSlotNoL)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  trans,
  validate,
  validateTrans,
  whenFailureFreeDefault,
 )
import Data.Bifunctor (first)
import Data.Foldable (sequenceA_)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))
import Validation (failureIf, failureUnless)

newtype EbWasNotAnnounced era = EbWasNotAnnounced (SafeHash EraIndependentEbReferences)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbReferencesSizeTooBig era = EbReferencesSizeTooBig (Mismatch RelLTEQ Int)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbAnnouncementSlotMismatch era = EbAnnouncementSlotMismatch (Mismatch RelEQ SlotNo)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbAnnouncementHashMismatch era
  = EbAnnouncementHashMismatch (Mismatch RelEQ (SafeHash EraIndependentEbReferences))
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbAnnouncementSizeMismatch era = EbAnnouncementSizeMismatch (Mismatch RelEQ Int)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbAlreadyApplied era = EbAlreadyApplied (SafeHash EraIndependentEbReferences)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

data EbPredFailure era
  = EbLedgersFailure (PredicateFailure (EraRule "EBLEDGERS" era))
  | EbWasNotAnnouncedInEB (EbWasNotAnnounced era)
  | EbReferencesSizeTooBigInEB (EbReferencesSizeTooBig era)
  | EbAnnouncementSlotMismatchInEB (EbAnnouncementSlotMismatch era)
  | EbAnnouncementHashMismatchInEB (EbAnnouncementHashMismatch era)
  | EbAnnouncementSizeMismatchInEB (EbAnnouncementSizeMismatch era)
  | EbAlreadyAppliedInEB (EbAlreadyApplied era)
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
      EbWasNotAnnouncedInEB f -> Sum EbWasNotAnnouncedInEB 1 !> To f
      EbReferencesSizeTooBigInEB f -> Sum EbReferencesSizeTooBigInEB 2 !> To f
      EbAnnouncementSlotMismatchInEB f -> Sum EbAnnouncementSlotMismatchInEB 3 !> To f
      EbAnnouncementHashMismatchInEB f -> Sum EbAnnouncementHashMismatchInEB 4 !> To f
      EbAnnouncementSizeMismatchInEB f -> Sum EbAnnouncementSizeMismatchInEB 5 !> To f
      EbAlreadyAppliedInEB f -> Sum EbAlreadyAppliedInEB 6 !> To f

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "EBLEDGERS" era))
  ) =>
  DecCBOR (EbPredFailure era)
  where
  decCBOR = decode $ Summands "EbPredFailure" $ \case
    0 -> SumD EbLedgersFailure <! From
    1 -> SumD EbWasNotAnnouncedInEB <! From
    2 -> SumD EbReferencesSizeTooBigInEB <! From
    3 -> SumD EbAnnouncementSlotMismatchInEB <! From
    4 -> SumD EbAnnouncementHashMismatchInEB <! From
    5 -> SumD EbAnnouncementSizeMismatchInEB <! From
    6 -> SumD EbAlreadyAppliedInEB <! From
    k -> Invalid k

instance
  ( EraCertState era
  , EraGov era
  , EraEndorserBlock era
  , Eq (PredicateFailure (EraRule "EBLEDGERS" era))
  , Ord (PredicateFailure (EraRule "EBLEDGERS" era))
  , Show (PredicateFailure (EraRule "EBLEDGERS" era))
  , Embed (EraRule "EBLEDGERS" era) (EB era)
  , Environment (EraRule "EBLEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "EBLEDGERS" era) ~ LedgerState era
  , Signal (EraRule "EBLEDGERS" era) ~ EndorserBlock era
  , Monoid (BlockBodyLimits era)
  , DijkstraEraPParams era
  ) =>
  STS (EB era)
  where
  type State (EB era) = NewEpochState era
  type Signal (EB era) = EndorserBlock era
  type Environment (EB era) = ()
  type Event (EB era) = DijkstraLedgerEvent era

  type PredicateFailure (EB era) = EbPredFailure era
  type BaseM (EB era) = ShelleyBase

  initialRules = []
  transitionRules = [dijkstraEbTransition @era]

dijkstraEbTransition ::
  forall era.
  ( EraGov era
  , EraEndorserBlock era
  , Embed (EraRule "EBLEDGERS" era) (EB era)
  , Environment (EraRule "EBLEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "EBLEDGERS" era) ~ LedgerState era
  , Signal (EraRule "EBLEDGERS" era) ~ EndorserBlock era
  , DijkstraEraPParams era
  , STS (EB era)
  ) =>
  TransitionRule (EB era)
dijkstraEbTransition = do
  TRC ((), state, eb) <- judgmentContext

  let ebRefs = eb ^. referencesEndorserBlockL
      ledgerState = state ^. nesEsL . esLStateL
      pp = state ^. nesEsL . curPParamsEpochStateL
      lastTickSlotNo = state ^. nesLastTickSlotNoL

  case ledgerState ^. lsLastEbAnnouncementL of
    SNothing -> failBecause $ EbWasNotAnnouncedInEB $ EbWasNotAnnounced $ hashAnnotated ebRefs
    SJust ebA -> do
      validate $ validateEbReferences pp ebA ebRefs
      validateTrans EbAnnouncementSlotMismatchInEB $ validateEbAnnouncementSlot ebA lastTickSlotNo

  whenFailureFreeDefault state $ do
    epochNo <- liftSTS $ epochFromSlot lastTickSlotNo
    let env =
          LedgersEnv
            { ledgersSlotNo = lastTickSlotNo
            , ledgersEpochNo = epochNo
            , ledgersPp = pp
            , ledgersAccount = state ^. chainAccountStateG
            }
    newLedgerState <-
      trans @(EraRule "EBLEDGERS" era) $ TRC (env, ledgerState, eb)

    pure $
      state
        & nesEsL . esLStateL
          .~ (newLedgerState & lsLastEbAnnouncementL %~ fmap (ebIsAppliedL .~ True))

validateEbReferences ::
  ( DijkstraEraPParams era
  , EraEndorserBlock era
  ) =>
  PParams era -> EbAnnouncement -> EbReferences era -> Test (EbPredFailure era)
validateEbReferences pp ebA ebRefs =
  sequenceA_
    [ first (fmap EbReferencesSizeTooBigInEB) $ validateEbReferencesSize pp ebRefs
    , first (fmap EbAnnouncementHashMismatchInEB) $ validateEbAnnouncementHash announcement ebRefs
    , first (fmap EbAnnouncementSizeMismatchInEB) $ validateEbAnnouncementSize announcement ebRefs
    , first (fmap EbAlreadyAppliedInEB) $ validateEbNotApplied ebA
    ]
  where
    announcement = ebA ^. ebAnnouncementL

validateEbReferencesSize ::
  ( DijkstraEraPParams era
  , EraEndorserBlock era
  ) =>
  PParams era -> EbReferences era -> Test (EbReferencesSizeTooBig era)
validateEbReferencesSize pp ebRefs =
  let maxEbRefsSize = fromIntegral @Word32 @Int $ pp ^. ppMaxEndorserBlockReferencesSizeL
      ebRefsSize = originalBytesSize ebRefs
   in failureUnless (ebRefsSize <= maxEbRefsSize) $
        EbReferencesSizeTooBig
          Mismatch
            { mismatchSupplied = ebRefsSize
            , mismatchExpected = maxEbRefsSize
            }

validateEbAnnouncementSlot :: EbAnnouncement -> SlotNo -> Test (EbAnnouncementSlotMismatch era)
validateEbAnnouncementSlot ebA lastTickSlot =
  let announcementSlot = ebA ^. ebAnnouncementSlotNoL
   in failureUnless (lastTickSlot == announcementSlot) $
        EbAnnouncementSlotMismatch
          Mismatch
            { mismatchSupplied = lastTickSlot
            , mismatchExpected = announcementSlot
            }

validateEbAnnouncementHash ::
  EraEndorserBlock era =>
  EbReferencesAnnouncement -> EbReferences era -> Test (EbAnnouncementHashMismatch era)
validateEbAnnouncementHash announcement ebRefs =
  let ebRefsHash = hashAnnotated ebRefs
      announcedHash = announcement ^. ebReferencesAnnouncementHashL
   in failureUnless (ebRefsHash == announcedHash) $
        EbAnnouncementHashMismatch
          Mismatch
            { mismatchSupplied = ebRefsHash
            , mismatchExpected = announcedHash
            }

validateEbAnnouncementSize ::
  EraEndorserBlock era =>
  EbReferencesAnnouncement -> EbReferences era -> Test (EbAnnouncementSizeMismatch era)
validateEbAnnouncementSize announcement ebRefs =
  let ebRefsSize = originalBytesSize ebRefs
      announcedSize = fromIntegral @Word32 @Int $ announcement ^. ebReferencesAnnouncementSizeL
   in failureUnless (ebRefsSize == announcedSize) $
        EbAnnouncementSizeMismatch
          Mismatch
            { mismatchSupplied = ebRefsSize
            , mismatchExpected = announcedSize
            }

validateEbNotApplied :: EbAnnouncement -> Test (EbAlreadyApplied era)
validateEbNotApplied ebA =
  failureIf (ebA ^. ebIsAppliedL) $
    EbAlreadyApplied $
      ebA ^. ebAnnouncementL . ebReferencesAnnouncementHashL

type instance EraRuleFailure "EB" DijkstraEra = EbPredFailure DijkstraEra

instance InjectRuleFailure "EB" EbPredFailure DijkstraEra

instance InjectRuleFailure "EB" EbWasNotAnnounced DijkstraEra where
  injectFailure = EbWasNotAnnouncedInEB

instance InjectRuleFailure "EB" EbReferencesSizeTooBig DijkstraEra where
  injectFailure = EbReferencesSizeTooBigInEB

instance InjectRuleFailure "EB" EbAnnouncementSlotMismatch DijkstraEra where
  injectFailure = EbAnnouncementSlotMismatchInEB

instance InjectRuleFailure "EB" EbAnnouncementHashMismatch DijkstraEra where
  injectFailure = EbAnnouncementHashMismatchInEB

instance InjectRuleFailure "EB" EbAnnouncementSizeMismatch DijkstraEra where
  injectFailure = EbAnnouncementSizeMismatchInEB

instance InjectRuleFailure "EB" EbAlreadyApplied DijkstraEra where
  injectFailure = EbAlreadyAppliedInEB

instance
  ( Era era
  , STS (EBLEDGERS era)
  , PredicateFailure (EraRule "EBLEDGERS" era) ~ EbLedgersPredFailure era
  ) =>
  Embed (EBLEDGERS era) (EB era)
  where
  wrapFailed = EbLedgersFailure
