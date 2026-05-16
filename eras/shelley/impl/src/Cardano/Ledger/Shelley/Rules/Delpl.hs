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

module Cardano.Ledger.Shelley.Rules.Delpl (
  DELPL,
  DelplEnv (..),
  ShelleyDelplPredFailure (..),
  ShelleyDelplEvent,
) where

import Cardano.Ledger.BaseTypes (EpochNo, ShelleyBase, invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Credential (Ptr)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (DELPL, ShelleyEra)
import Cardano.Ledger.Shelley.Rules.Deleg (
  DELEG,
  DelegEnv (..),
  ShelleyDelegEvent,
  ShelleyDelegPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Pool (POOL, PoolEnv (..), ShelleyPoolPredFailure)
import qualified Cardano.Ledger.Shelley.Rules.Pool as Pool
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.TxCert (GenesisDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Slot (SlotNo)
import Control.DeepSeq
import Control.State.Transition
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))

data DelplEnv era = DelplEnv
  { delplSlotNo :: SlotNo
  , delplEpochNo :: EpochNo
  , delPlPtr :: Ptr
  , delPlPp :: PParams era
  , delPlAccount :: ChainAccountState
  }

data ShelleyDelplPredFailure era
  = PoolFailure (PredicateFailure (EraRule "POOL" era)) -- Subtransition Failures
  | DelegFailure (PredicateFailure (EraRule "DELEG" era)) -- Subtransition Failures
  deriving (Generic)

type instance EraRuleFailure "DELPL" ShelleyEra = ShelleyDelplPredFailure ShelleyEra

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure ShelleyEra

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure ShelleyEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure ShelleyEra where
  injectFailure = DelegFailure

data ShelleyDelplEvent era
  = PoolEvent (Event (EraRule "POOL" era))
  | DelegEvent (Event (EraRule "DELEG" era))
  deriving (Generic)

instance
  ( NFData (Event (EraRule "DELEG" era))
  , NFData (Event (EraRule "POOL" era))
  ) =>
  NFData (ShelleyDelplEvent era)

deriving instance
  ( Eq (Event (EraRule "DELEG" era))
  , Eq (Event (EraRule "POOL" era))
  ) =>
  Eq (ShelleyDelplEvent era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  ) =>
  Eq (ShelleyDelplPredFailure era)

deriving stock instance
  ( Ord (PredicateFailure (EraRule "DELEG" era))
  , Ord (PredicateFailure (EraRule "POOL" era))
  ) =>
  Ord (ShelleyDelplPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  ) =>
  Show (ShelleyDelplPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "DELEG" era))
  , NFData (PredicateFailure (EraRule "POOL" era))
  ) =>
  NFData (ShelleyDelplPredFailure era)

instance
  ( Era era
  , EraCertState era
  , Embed (EraRule "DELEG" era) (DELPL era)
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , State (EraRule "DELEG" era) ~ CertState era
  , Embed (EraRule "POOL" era) (DELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "DELEG" era) ~ TxCert era
  , Embed (EraRule "POOL" era) (DELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Signal (EraRule "POOL" era) ~ PoolCert
  , TxCert era ~ ShelleyTxCert era
  ) =>
  STS (DELPL era)
  where
  type State (DELPL era) = CertState era
  type Signal (DELPL era) = TxCert era
  type Environment (DELPL era) = DelplEnv era
  type BaseM (DELPL era) = ShelleyBase
  type PredicateFailure (DELPL era) = ShelleyDelplPredFailure era
  type Event (DELPL era) = ShelleyDelplEvent era

  transitionRules = [delplTransition]

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "POOL" era))
  , EncCBOR (PredicateFailure (EraRule "DELEG" era))
  ) =>
  EncCBOR (ShelleyDelplPredFailure era)
  where
  encCBOR = \case
    (PoolFailure a) ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR a
    (DelegFailure a) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR a

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "POOL" era))
  , DecCBOR (PredicateFailure (EraRule "DELEG" era))
  , Typeable (Script era)
  ) =>
  DecCBOR (ShelleyDelplPredFailure era)
  where
  decCBOR =
    decodeRecordSum
      "PredicateFailure (DELPL era)"
      ( \case
          0 -> do
            a <- decCBOR
            pure (2, PoolFailure a)
          1 -> do
            a <- decCBOR
            pure (2, DelegFailure a)
          k -> invalidKey k
      )

delplTransition ::
  forall era.
  ( Embed (EraRule "DELEG" era) (DELPL era)
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "DELEG" era) ~ TxCert era
  , Embed (EraRule "POOL" era) (DELPL era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Signal (EraRule "POOL" era) ~ PoolCert
  , TxCert era ~ ShelleyTxCert era
  , EraCertState era
  ) =>
  TransitionRule (DELPL era)
delplTransition = do
  TRC (DelplEnv slot eNo ptr pp chainAccountState, d, c) <- judgmentContext
  case c of
    ShelleyTxCertPool poolCert -> do
      ps <-
        trans @(EraRule "POOL" era) $ TRC (PoolEnv eNo pp, d ^. certPStateL, poolCert)
      pure $ d & certPStateL .~ ps
    ShelleyTxCertGenesisDeleg GenesisDelegCert {} -> do
      trans @(EraRule "DELEG" era) $
        TRC (DelegEnv slot eNo ptr chainAccountState pp, d, c)
    ShelleyTxCertDelegCert _ -> do
      trans @(EraRule "DELEG" era) $
        TRC (DelegEnv slot eNo ptr chainAccountState pp, d, c)
    ShelleyTxCertMir _ -> do
      trans @(EraRule "DELEG" era) $
        TRC (DelegEnv slot eNo ptr chainAccountState pp, d, c)

instance
  ( Era era
  , STS (POOL era)
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  , Event (EraRule "POOL" era) ~ Pool.PoolEvent era
  ) =>
  Embed (POOL era) (DELPL era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( ShelleyEraAccounts era
  , ShelleyEraTxCert era
  , EraCertState era
  , EraPParams era
  , AtMostEra "Babbage" era
  , PredicateFailure (EraRule "DELEG" era) ~ ShelleyDelegPredFailure era
  , Event (EraRule "DELEG" era) ~ ShelleyDelegEvent era
  ) =>
  Embed (DELEG era) (DELPL era)
  where
  wrapFailed = DelegFailure
  wrapEvent = DelegEvent
