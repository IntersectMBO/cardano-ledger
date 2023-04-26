{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Delegs (
  ConwayDELEGS,
  ConwayDelegsPredFailure (..),
  ConwayDelegsEvent (..),
) where

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, mkCertIxPartial)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (PState)
import Cardano.Ledger.Conway.Era (ConwayDELEGS)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (
  CertState (..),
  Coin,
  DCert,
  DState (..),
  DelegEnv,
  DelegsEnv (..),
  DelplEnv (..),
  KeyHash,
  KeyRole (..),
  PoolEnv,
  Ptr (..),
  RewardAcnt,
  ShelleyDELPL,
 )
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody)
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelplEvent,
  ShelleyDelplPredFailure,
  drainWithdrawals,
  validateDelegationRegistered,
  validateZeroRewards,
 )
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  validateTrans,
 )
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ConwayDelegsPredFailure era
  = -- | Target pool which is not registered
    DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era))
  | -- | Withdrawals that are missing or do not withdrawal the entire amount
    WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt (EraCrypto era)) Coin)
  | -- | CERT rule subtransition Failures
    CertFailure !(PredicateFailure (EraRule "CERT" era))
  deriving (Generic, Typeable)

instance NoThunks (PredicateFailure (EraRule "CERT" era)) => NoThunks (ConwayDelegsPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  EncCBOR (ConwayDelegsPredFailure era)
  where
  encCBOR = encode . encoder
    where
      encoder (DelegateeNotRegisteredDELEG kh) = Sum (DelegateeNotRegisteredDELEG @era) 0 !> To kh
      encoder (WithdrawalsNotInRewardsDELEGS rs) = Sum (WithdrawalsNotInRewardsDELEGS @era) 1 !> To rs
      encoder (CertFailure x) = Sum (CertFailure @era) 2 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  DecCBOR (ConwayDelegsPredFailure era)
  where
  decCBOR = decode $ Summands "ConwayTallyPredFailure" $ \case
    0 -> SumD DelegateeNotRegisteredDELEG <! From
    1 -> SumD WithdrawalsNotInRewardsDELEGS <! From
    2 -> SumD CertFailure <! From
    k -> Invalid k

deriving instance
  Eq (PredicateFailure (EraRule "CERT" era)) =>
  Eq (ConwayDelegsPredFailure era)
deriving instance
  Show (PredicateFailure (EraRule "CERT" era)) =>
  Show (ConwayDelegsPredFailure era)

newtype ConwayDelegsEvent era = CertEvent (Event (EraRule "CERT" era))

instance
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Signal (EraRule "CERT" era) ~ DCert (EraCrypto era)
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , EraRule "DELEGS" era ~ ConwayDELEGS era
  , Embed (EraRule "CERT" era) (ConwayDELEGS era)
  ) =>
  STS (ConwayDELEGS era)
  where
  type State (ConwayDELEGS era) = CertState era
  type Signal (ConwayDELEGS era) = Seq (DCert (EraCrypto era))
  type Environment (ConwayDELEGS era) = DelegsEnv era
  type BaseM (ConwayDELEGS era) = ShelleyBase
  type
    PredicateFailure (ConwayDELEGS era) =
      ConwayDelegsPredFailure era
  type Event (ConwayDELEGS era) = ConwayDelegsEvent era

  transitionRules = [conwayDelegsTransition @era]

conwayDelegsTransition ::
  forall era.
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Embed (EraRule "CERT" era) (ConwayDELEGS era)
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , Signal (EraRule "CERT" era) ~ DCert (EraCrypto era)
  , EraRule "DELEGS" era ~ ConwayDELEGS era
  ) =>
  TransitionRule (ConwayDELEGS era)
conwayDelegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), certState, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let dState = certDState certState
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      validateTrans WithdrawalsNotInRewardsDELEGS $
        validateZeroRewards dState withdrawals network
      pure $ certState {certDState = drainWithdrawals dState withdrawals}
    gamma :|> c -> do
      certState' <-
        trans @(ConwayDELEGS era) $ TRC (env, certState, gamma)
      validateTrans DelegateeNotRegisteredDELEG $
        validateDelegationRegistered certState' c
      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "CERT" era) $
        TRC (DelplEnv slot ptr pp acnt, certState', c)

instance
  ( Era era
  , Embed (EraRule "POOL" era) (ShelleyDELPL era)
  , Embed (EraRule "DELEG" era) (ShelleyDELPL era)
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "DELEG" era) ~ DState era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , Signal (EraRule "POOL" era) ~ DCert (EraCrypto era)
  , Signal (EraRule "DELEG" era) ~ DCert (EraCrypto era)
  , PredicateFailure (EraRule "CERT" era) ~ ShelleyDelplPredFailure era
  , Event (EraRule "CERT" era) ~ ShelleyDelplEvent era
  ) =>
  Embed (ShelleyDELPL era) (ConwayDELEGS era)
  where
  wrapFailed = CertFailure
  wrapEvent = CertEvent
