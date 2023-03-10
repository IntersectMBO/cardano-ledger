{-# LANGUAGE BangPatterns #-}
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
  depositEqualsObligation,
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
import Cardano.Ledger.Shelley.Era (ShelleyLEDGER)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState (..),
  DState (..),
  LedgerState (..),
  PState (..),
  UTxOState (..),
  obligationCertState,
 )
import Cardano.Ledger.Shelley.Rules.Delegs (
  DelegsEnv (..),
  ShelleyDELEGS,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Reports (showTxCerts, synopsisCoinMap)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUTXOW, ShelleyUtxowPredFailure)
import Cardano.Ledger.Slot (EpochNo, SlotNo, epochInfoEpoch)
import Cardano.Ledger.UMap (depositView)
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
  }

deriving instance Show (PParams era) => Show (LedgerEnv era)

instance NFData (PParams era) => NFData (LedgerEnv era) where
  rnf (LedgerEnv _slotNo _ix pp _account) = rnf pp

data ShelleyLedgerPredFailure era
  = UtxowFailure (PredicateFailure (EraRule "UTXOW" era)) -- Subtransition Failures
  | DelegsFailure (PredicateFailure (EraRule "DELEGS" era)) -- Subtransition Failures
  deriving (Generic)

data ShelleyLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | DelegsEvent (Event (EraRule "DELEGS" era))

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

instance
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , EraTx era
  , ShelleyEraTxBody era
  , Embed (EraRule "DELEGS" era) (ShelleyLEDGER era)
  , Embed (EraRule "UTXOW" era) (ShelleyLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert era)
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

  renderAssertionViolation = depositEqualsObligation

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (ShelleyLedger)"
        ( \(TRC (_, _, _))
           (LedgerState utxoSt dpstate) ->
              obligationCertState dpstate == utxosDeposited utxoSt
        )
    ]

ledgerTransition ::
  forall era.
  ( EraTx era
  , Embed (EraRule "DELEGS" era) (ShelleyLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert era)
  , Embed (EraRule "UTXOW" era) (ShelleyLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  ) =>
  TransitionRule (ShelleyLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt dpstate, tx) <- judgmentContext
  dpstate' <-
    trans @(EraRule "DELEGS" era) $
      TRC
        ( DelegsEnv slot txIx pp tx account
        , dpstate
        , StrictSeq.fromStrict $ tx ^. bodyTxL . certsTxBodyL
        )
  let genDelegs = dsGenDelegs (certDState dpstate)

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv slot pp dpstate genDelegs
        , utxoSt
        , tx
        )
  pure (LedgerState utxoSt' dpstate')

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

depositEqualsObligation ::
  ( EraTx era
  , ShelleyEraTxBody era
  , Environment t ~ LedgerEnv era
  , Signal t ~ Tx era
  , State t ~ LedgerState era
  ) =>
  AssertionViolation t ->
  String
depositEqualsObligation
  AssertionViolation {avSTS, avMsg, avCtx = (TRC (LedgerEnv slot _ pp _, _, tx)), avState} =
    let dpstate = lsCertState <$> avState
        utxo = (utxosUtxo . lsUTxOState) <$> avState
        txb = tx ^. bodyTxL
     in "\n\nAssertionViolation ("
          <> avSTS
          <> ")\n   "
          <> avMsg
          <> "\nCERTS\n"
          <> showTxCerts txb
          <> "\n(slot,keyDeposit,poolDeposit) "
          <> show (slot, pp ^. ppKeyDepositL, pp ^. ppPoolDepositL)
          <> "\nutxosDeposited = "
          <> show ((utxosDeposited . lsUTxOState) <$> avState)
          <> "\nKey Deposits summary = "
          <> synopsisCoinMap ((depositView . dsUnified . certDState . lsCertState) <$> avState)
          <> "\nPool Deposits summary = "
          <> synopsisCoinMap ((psDeposits . certPState . lsCertState) <$> avState)
          <> "\nConsumed = "
          <> show (consumedTxBody txb pp <$> dpstate <*> utxo)
          <> "\nProduced = "
          <> show (producedTxBody txb pp <$> dpstate)
