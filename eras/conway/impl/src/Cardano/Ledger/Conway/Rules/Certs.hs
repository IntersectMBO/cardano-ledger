{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Cardano.Ledger.Conway.Rules.Certs (
  ConwayCERTS,
  ConwayCertsPredFailure (..),
  ConwayCertsEvent (..),
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
import Cardano.Ledger.Conway.Era (ConwayCERT, ConwayCERTS)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertEvent, ConwayCertPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (
  CertState (..),
  Coin,
  DelegsEnv (DelegsEnv),
  DelplEnv (DelplEnv),
  KeyHash,
  KeyRole (..),
  Ptr (Ptr),
  RewardAcnt,
 )
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody)
import Cardano.Ledger.Shelley.Rules (
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
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ConwayCertsPredFailure era
  = -- | Target pool which is not registered
    DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era))
  | -- | Withdrawals that are missing or do not withdrawal the entire amount
    WithdrawalsNotInRewardsCERTS
      !(Map (RewardAcnt (EraCrypto era)) Coin)
  | -- | CERT rule subtransition Failures
    CertFailure !(PredicateFailure (EraRule "CERT" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "CERT" era)) =>
  Eq (ConwayCertsPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "CERT" era)) =>
  Show (ConwayCertsPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "CERT" era)) =>
  NoThunks (ConwayCertsPredFailure era)

newtype ConwayCertsEvent era = CertEvent (Event (EraRule "CERT" era))

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  EncCBOR (ConwayCertsPredFailure era)
  where
  encCBOR =
    encode . \case
      DelegateeNotRegisteredDELEG kh -> Sum (DelegateeNotRegisteredDELEG @era) 0 !> To kh
      WithdrawalsNotInRewardsCERTS rs -> Sum (WithdrawalsNotInRewardsCERTS @era) 1 !> To rs
      CertFailure x -> Sum (CertFailure @era) 2 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  DecCBOR (ConwayCertsPredFailure era)
  where
  decCBOR = decode $ Summands "ConwayTallyPredFailure" $ \case
    0 -> SumD DelegateeNotRegisteredDELEG <! From
    1 -> SumD WithdrawalsNotInRewardsCERTS <! From
    2 -> SumD CertFailure <! From
    k -> Invalid k

instance
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  ) =>
  STS (ConwayCERTS era)
  where
  type State (ConwayCERTS era) = CertState era
  type Signal (ConwayCERTS era) = Seq (TxCert era)
  type Environment (ConwayCERTS era) = DelegsEnv era
  type BaseM (ConwayCERTS era) = ShelleyBase
  type
    PredicateFailure (ConwayCERTS era) =
      ConwayCertsPredFailure era
  type Event (ConwayCERTS era) = ConwayCertsEvent era

  transitionRules = [conwayCertsTransition @era]

conwayCertsTransition ::
  forall era.
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  ) =>
  TransitionRule (ConwayCERTS era)
conwayCertsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), certState, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let dState = certDState certState
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      validateTrans WithdrawalsNotInRewardsCERTS $
        validateZeroRewards dState withdrawals network
      pure $ certState {certDState = drainWithdrawals dState withdrawals}
    gamma :|> c -> do
      certState' <-
        trans @(ConwayCERTS era) $ TRC (env, certState, gamma)
      validateTrans DelegateeNotRegisteredDELEG $
        validateDelegationRegistered certState' c
      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "CERT" era) $
        TRC (DelplEnv slot ptr pp acnt, certState', c)

instance
  ( Era era
  , STS (ConwayCERT era)
  , BaseM (EraRule "CERT" era) ~ ShelleyBase
  , Event (EraRule "CERT" era) ~ ConwayCertEvent era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  ) =>
  Embed (ConwayCERT era) (ConwayCERTS era)
  where
  wrapFailed = CertFailure
  wrapEvent = CertEvent
