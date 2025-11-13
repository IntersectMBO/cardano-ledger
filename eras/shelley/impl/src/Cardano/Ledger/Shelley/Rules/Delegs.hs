{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Cardano.Ledger.Shelley.Rules.Delegs (
  ShelleyDELEGS,
  DelegsEnv (..),
  ShelleyDelegsPredFailure (..),
  ShelleyDelegsEvent (..),
  PredicateFailure,
  validateStakePoolDelegateeRegistered,
) where

import Cardano.Ledger.BaseTypes (
  CertIx (..),
  EpochNo,
  ShelleyBase,
  TxIx (..),
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr (..), SlotNo32 (..))
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyDELEGS, ShelleyEra)
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Delpl (
  DelplEnv (..),
  ShelleyDELPL,
  ShelleyDelplEvent,
  ShelleyDelplPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq
import Control.SetAlgebra (dom, eval, (∈))
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
  validateTrans,
 )
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data DelegsEnv era = DelegsEnv
  { delegsSlotNo :: SlotNo
  , delegsEpochNo :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , delegsIx :: TxIx
  , delegspp :: PParams era
  , delegsTx :: Tx TopTx era
  , delegsAccount :: ChainAccountState
  }

deriving stock instance
  ( Show (Tx TopTx era)
  , Show (PParams era)
  ) =>
  Show (DelegsEnv era)

data ShelleyDelegsPredFailure era
  = -- | Target pool which is not registered
    DelegateeNotRegisteredDELEG
      (KeyHash StakePool)
  | -- | Subtransition Failures
    DelplFailure (PredicateFailure (EraRule "DELPL" era))
  deriving (Generic)

type instance EraRuleFailure "DELEGS" ShelleyEra = ShelleyDelegsPredFailure ShelleyEra

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure ShelleyEra

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure ShelleyEra where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure ShelleyEra where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure ShelleyEra where
  injectFailure = DelplFailure . injectFailure

newtype ShelleyDelegsEvent era = DelplEvent (Event (EraRule "DELPL" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "DELPL" era)) => Eq (ShelleyDelegsEvent era)

instance NFData (Event (EraRule "DELPL" era)) => NFData (ShelleyDelegsEvent era)

deriving stock instance
  Show (PredicateFailure (EraRule "DELPL" era)) =>
  Show (ShelleyDelegsPredFailure era)

deriving stock instance
  Eq (PredicateFailure (EraRule "DELPL" era)) =>
  Eq (ShelleyDelegsPredFailure era)

instance
  NFData (PredicateFailure (EraRule "DELPL" era)) =>
  NFData (ShelleyDelegsPredFailure era)

instance
  ( EraTx era
  , EraCertState era
  , ShelleyEraTxBody era
  , Embed (EraRule "DELPL" era) (ShelleyDELEGS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , EraRule "DELEGS" era ~ ShelleyDELEGS era
  ) =>
  STS (ShelleyDELEGS era)
  where
  type State (ShelleyDELEGS era) = CertState era
  type Signal (ShelleyDELEGS era) = Seq (TxCert era)
  type Environment (ShelleyDELEGS era) = DelegsEnv era
  type BaseM (ShelleyDELEGS era) = ShelleyBase
  type
    PredicateFailure (ShelleyDELEGS era) =
      ShelleyDelegsPredFailure era
  type Event (ShelleyDELEGS era) = ShelleyDelegsEvent era

  transitionRules = [delegsTransition]

instance
  NoThunks (PredicateFailure (EraRule "DELPL" era)) =>
  NoThunks (ShelleyDelegsPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "DELPL" era))
  ) =>
  EncCBOR (ShelleyDelegsPredFailure era)
  where
  encCBOR = \case
    DelegateeNotRegisteredDELEG kh ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR kh
    (DelplFailure a) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR a

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "DELPL" era))
  , Typeable (Script era)
  ) =>
  DecCBOR (ShelleyDelegsPredFailure era)
  where
  decCBOR =
    decodeRecordSum "PredicateFailure" $
      \case
        0 -> do
          kh <- decCBOR
          pure (2, DelegateeNotRegisteredDELEG kh)
        1 -> do
          a <- decCBOR
          pure (2, DelplFailure a)
        k -> invalidKey k

delegsTransition ::
  forall era.
  ( EraTx era
  , EraCertState era
  , ShelleyEraTxBody era
  , Embed (EraRule "DELPL" era) (ShelleyDELEGS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , EraRule "DELEGS" era ~ ShelleyDELEGS era
  ) =>
  TransitionRule (ShelleyDELEGS era)
delegsTransition = do
  TRC
    ( env@(DelegsEnv slot@(SlotNo slot64) epochNo txIx pp _tx chainAccountState)
      , certState
      , certificates
      ) <-
    judgmentContext
  case certificates of
    Empty -> pure certState
    gamma :|> txCert -> do
      certState' <-
        trans @(ShelleyDELEGS era) $ TRC (env, certState, gamma)
      validateTrans DelegateeNotRegisteredDELEG $
        case txCert of
          DelegStakeTxCert _ targetPool ->
            validateStakePoolDelegateeRegistered (certState' ^. certPStateL) targetPool
          _ -> pure ()
      -- It is impossible to have 65535 number of certificates in a transaction.
      let certIx = CertIx (fromIntegral @Int @Word16 $ length gamma)
          ptr = Ptr (SlotNo32 (fromIntegral @Word64 @Word32 slot64)) txIx certIx
      trans @(EraRule "DELPL" era) $
        TRC (DelplEnv slot epochNo ptr pp chainAccountState, certState', txCert)

validateStakePoolDelegateeRegistered ::
  PState era ->
  KeyHash StakePool ->
  Test (KeyHash StakePool)
validateStakePoolDelegateeRegistered pState targetPool =
  let stPools = psStakePools pState
   in failureUnless (eval (targetPool ∈ dom stPools)) targetPool

instance
  ( Era era
  , STS (ShelleyDELPL era)
  , PredicateFailure (EraRule "DELPL" era) ~ ShelleyDelplPredFailure era
  , Event (EraRule "DELPL" era) ~ ShelleyDelplEvent era
  ) =>
  Embed (ShelleyDELPL era) (ShelleyDELEGS era)
  where
  wrapFailed = DelplFailure
  wrapEvent = DelplEvent
