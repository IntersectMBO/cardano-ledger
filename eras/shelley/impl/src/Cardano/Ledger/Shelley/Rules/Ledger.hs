{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Cardano.Ledger.Shelley.Rules.Ledger (
  ShelleyLEDGER,
  LedgerEnv (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgerEvent (..),
  Event,
  PredicateFailure,
  epochFromSlot,
  renderDepositEqualsObligationViolation,
  shelleyLedgerAssertions,
)
where

import Cardano.Ledger.BaseTypes (Globals, ShelleyBase, TxIx, epochInfoPure, invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyLEDGER)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState (..),
  LedgerState (..),
  UTxOState (..),
  utxosDepositedL,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (allObligations, potEqualsObligation)
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Delegs (
  DelegsEnv (..),
  ShelleyDELEGS,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDelplPredFailure)
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Reports (showTxCerts)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoPredFailure (..), UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUTXOW, ShelleyUtxowPredFailure)
import Cardano.Ledger.Slot (EpochNo, SlotNo, epochInfoEpoch)
import Control.DeepSeq (NFData (..))
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Assertion (PostCondition),
  AssertionViolation (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- ========================================================

data LedgerEnv era = LedgerEnv
  { ledgerSlotNo :: !SlotNo
  , ledgerIx :: !TxIx
  , ledgerPp :: !(PParams era)
  , ledgerAccount :: !AccountState
  , ledgerMempool :: !Bool
  }
  deriving (Generic)

deriving instance Show (PParams era) => Show (LedgerEnv era)
deriving instance Eq (PParams era) => Eq (LedgerEnv era)

instance NFData (PParams era) => NFData (LedgerEnv era) where
  rnf (LedgerEnv _slotNo _ix pp _account _mempool) = rnf pp

data ShelleyLedgerPredFailure era
  = UtxowFailure (PredicateFailure (EraRule "UTXOW" era)) -- Subtransition Failures
  | DelegsFailure (PredicateFailure (EraRule "DELEGS" era)) -- Subtransition Failures
  deriving (Generic)

type instance EraRuleFailure "LEDGER" (ShelleyEra c) = ShelleyLedgerPredFailure (ShelleyEra c)

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure (ShelleyEra c)

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure (ShelleyEra c) where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure (ShelleyEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure (ShelleyEra c) where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure (ShelleyEra c) where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure (ShelleyEra c) where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure (ShelleyEra c) where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure (ShelleyEra c) where
  injectFailure = DelegsFailure . injectFailure

type instance EraRuleEvent "LEDGER" (ShelleyEra c) = ShelleyLedgerEvent (ShelleyEra c)

data ShelleyLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | DelegsEvent (Event (EraRule "DELEGS" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "DELEGS" era))
  ) =>
  Eq (ShelleyLedgerEvent era)

instance
  ( NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "DELEGS" era))
  ) =>
  NFData (ShelleyLedgerEvent era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEGS" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  Show (ShelleyLedgerPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEGS" era))
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  Eq (ShelleyLedgerPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEGS" era))
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  NoThunks (ShelleyLedgerPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEGS" era))
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  NFData (ShelleyLedgerPredFailure era)

instance
  ( EncCBOR (PredicateFailure (EraRule "DELEGS" era))
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  EncCBOR (ShelleyLedgerPredFailure era)
  where
  encCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> encCBOR (1 :: Word8) <> encCBOR a

instance
  ( DecCBOR (PredicateFailure (EraRule "DELEGS" era))
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , Era era
  ) =>
  DecCBOR (ShelleyLedgerPredFailure era)
  where
  decCBOR =
    decodeRecordSum "PredicateFailure (LEDGER era)" $
      \case
        0 -> do
          a <- decCBOR
          pure (2, UtxowFailure a)
        1 -> do
          a <- decCBOR
          pure (2, DelegsFailure a)
        k -> invalidKey k

epochFromSlot :: SlotNo -> Reader Globals EpochNo
epochFromSlot slot = do
  ei <- asks epochInfoPure
  epochInfoEpoch ei slot

shelleyLedgerAssertions ::
  ( EraGov era
  , State (rule era) ~ LedgerState era
  ) =>
  [Assertion (rule era)]
shelleyLedgerAssertions =
  [ PostCondition
      "Deposit pot must equal obligation (LEDGER)"
      ( \(TRC (_, _, _))
         (LedgerState utxoSt dpstate) -> potEqualsObligation dpstate utxoSt
      )
  ]

instance
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , EraTx era
  , EraGov era
  , Embed (EraRule "DELEGS" era) (ShelleyLEDGER era)
  , Embed (EraRule "UTXOW" era) (ShelleyLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , ProtVerAtMost era 8
  ) =>
  STS (ShelleyLEDGER era)
  where
  type State (ShelleyLEDGER era) = LedgerState era
  type Signal (ShelleyLEDGER era) = Tx era
  type Environment (ShelleyLEDGER era) = LedgerEnv era
  type BaseM (ShelleyLEDGER era) = ShelleyBase
  type PredicateFailure (ShelleyLEDGER era) = ShelleyLedgerPredFailure era
  type Event (ShelleyLEDGER era) = ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions

ledgerTransition ::
  forall era.
  ( EraTx era
  , Embed (EraRule "DELEGS" era) (ShelleyLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , Embed (EraRule "UTXOW" era) (ShelleyLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  ) =>
  TransitionRule (ShelleyLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account _, LedgerState utxoSt certState, tx) <- judgmentContext
  certState' <-
    trans @(EraRule "DELEGS" era) $
      TRC
        ( DelegsEnv slot txIx pp tx account
        , certState
        , StrictSeq.fromStrict $ tx ^. bodyTxL . certsTxBodyL
        )

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv slot pp certState
        , utxoSt
        , tx
        )
  pure (LedgerState utxoSt' certState')

instance
  ( Era era
  , STS (ShelleyDELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (ShelleyLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( STS (ShelleyUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ ShelleyUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ Event (ShelleyUTXOW era)
  ) =>
  Embed (ShelleyUTXOW era) (ShelleyLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

-- =============================================================

renderDepositEqualsObligationViolation ::
  ( EraTx era
  , EraGov era
  , Environment t ~ LedgerEnv era
  , Signal t ~ Tx era
  , State t ~ LedgerState era
  ) =>
  AssertionViolation t ->
  String
renderDepositEqualsObligationViolation
  AssertionViolation {avSTS, avMsg, avCtx = TRC (LedgerEnv slot _ pp _ _, _, tx), avState} =
    case avState of
      Nothing -> "\nAssertionViolation " ++ avSTS ++ " " ++ avMsg ++ " (avState is Nothing)."
      Just lstate ->
        let certstate = lsCertState lstate
            utxoSt = lsUTxOState lstate
            utxo = utxosUtxo utxoSt
            txb = tx ^. bodyTxL
            pot = utxoSt ^. utxosDepositedL
         in "\n\nAssertionViolation ("
              <> avSTS
              <> ")\n\n  "
              <> avMsg
              <> "\n\nCERTS\n"
              <> showTxCerts txb
              <> "\n(slot,keyDeposit,poolDeposit) "
              <> show (slot, pp ^. ppKeyDepositL, pp ^. ppPoolDepositL)
              <> "\nThe Pot (utxosDeposited) = "
              <> show pot
              <> "\n"
              <> show (allObligations certstate (utxosGovState utxoSt))
              <> "\nConsumed = "
              <> show (consumedTxBody txb pp certstate utxo)
              <> "\nProduced = "
              <> show (producedTxBody txb pp certstate)
