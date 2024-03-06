{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
  validateZeroRewards,
  validateStakePoolDelegateeRegistered,
  drainWithdrawals,
)
where

import Cardano.Ledger.BaseTypes (
  Network,
  ShelleyBase,
  TxIx,
  invalidKey,
  mkCertIxPartial,
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, Ptr (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.Era (ShelleyDELEGS, ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState (..),
  DState (..),
  PState (..),
  certDState,
  psStakePoolParams,
  rewards,
 )
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Delpl (
  DelplEnv (..),
  ShelleyDELPL,
  ShelleyDelplEvent,
  ShelleyDelplPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.TxBody (
  RewardAccount (..),
  ShelleyEraTxBody (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxCert (pattern DelegStakeTxCert)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.UMap (UMElem (..), UMap (..), UView (..), fromCompact)
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (∈))
import Control.State.Transition (
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
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data DelegsEnv era = DelegsEnv
  { delegsSlotNo :: !SlotNo
  , delegsIx :: !TxIx
  , delegspp :: !(PParams era)
  , delegsTx :: !(Tx era)
  , delegsAccount :: !AccountState
  }

deriving stock instance
  ( Show (Tx era)
  , Show (PParams era)
  ) =>
  Show (DelegsEnv era)

data ShelleyDelegsPredFailure era
  = -- | Target pool which is not registered
    DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era))
  | -- | Withdrawals that are missing or do not withdrawal the entire amount
    WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAccount (EraCrypto era)) Coin)
  | -- | Subtransition Failures
    DelplFailure !(PredicateFailure (EraRule "DELPL" era))
  deriving (Generic)

type instance EraRuleFailure "DELEGS" (ShelleyEra c) = ShelleyDelegsPredFailure (ShelleyEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure (ShelleyEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure (ShelleyEra c) where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure (ShelleyEra c) where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure (ShelleyEra c) where
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
  , Typeable (Script era)
  , EncCBOR (PredicateFailure (EraRule "DELPL" era))
  ) =>
  EncCBOR (ShelleyDelegsPredFailure era)
  where
  encCBOR = \case
    DelegateeNotRegisteredDELEG kh ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR kh
    WithdrawalsNotInRewardsDELEGS ws ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR ws
    (DelplFailure a) ->
      encodeListLen 2
        <> encCBOR (2 :: Word8)
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
          ws <- decCBOR
          pure (2, WithdrawalsNotInRewardsDELEGS ws)
        2 -> do
          a <- decCBOR
          pure (2, DelplFailure a)
        k -> invalidKey k

delegsTransition ::
  forall era.
  ( EraTx era
  , ShelleyEraTxBody era
  , Embed (EraRule "DELPL" era) (ShelleyDELEGS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , EraRule "DELEGS" era ~ ShelleyDELEGS era
  ) =>
  TransitionRule (ShelleyDELEGS era)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), certState, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let dState = certDState certState
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      validateTrans WithdrawalsNotInRewardsDELEGS $
        validateZeroRewards dState withdrawals network
      pure $ certState {certDState = drainWithdrawals dState withdrawals}
    gamma :|> txCert -> do
      certState' <-
        trans @(ShelleyDELEGS era) $ TRC (env, certState, gamma)
      validateTrans DelegateeNotRegisteredDELEG $
        case txCert of
          DelegStakeTxCert _ targetPool ->
            validateStakePoolDelegateeRegistered (certPState certState') targetPool
          _ -> pure ()
      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "DELPL" era) $
        TRC (DelplEnv slot ptr pp acnt, certState', txCert)

validateStakePoolDelegateeRegistered ::
  PState era ->
  KeyHash 'StakePool (EraCrypto era) ->
  Test (KeyHash 'StakePool (EraCrypto era))
validateStakePoolDelegateeRegistered pState targetPool =
  let stPools = psStakePoolParams pState
   in failureUnless (eval (targetPool ∈ dom stPools)) targetPool

-- @withdrawals_@ is small and @rewards@ big, better to transform the former
-- than the latter into the right shape so we can call 'Map.isSubmapOf'.
isSubmapOfUM ::
  forall era.
  Map (RewardAccount (EraCrypto era)) Coin ->
  UView (EraCrypto era) (Credential 'Staking (EraCrypto era)) UM.RDPair ->
  Bool
isSubmapOfUM ws (RewDepUView (UMap tripmap _)) = Map.isSubmapOfBy f withdrawalMap tripmap
  where
    withdrawalMap :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    withdrawalMap = Map.mapKeys (\(RewardAccount _ cred) -> cred) ws
    f :: Coin -> UMElem (EraCrypto era) -> Bool
    f coin1 (UMElem (SJust (UM.RDPair coin2 _)) _ _ _) = coin1 == fromCompact coin2
    f _ _ = False

drainWithdrawals :: DState era -> Withdrawals (EraCrypto era) -> DState era
drainWithdrawals dState (Withdrawals wdrls) =
  dState {dsUnified = rewards dState UM.⨃ drainedRewardAccounts}
  where
    drainedRewardAccounts =
      Map.foldrWithKey
        ( \(RewardAccount _ cred) _coin ->
            Map.insert cred (UM.RDPair (UM.CompactCoin 0) (UM.CompactCoin 0))
            -- Note that the deposit (CompactCoin 0) will be ignored.
        )
        Map.empty
        wdrls

validateZeroRewards ::
  forall era.
  DState era ->
  Withdrawals (EraCrypto era) ->
  Network ->
  Test (Map (RewardAccount (EraCrypto era)) Coin)
validateZeroRewards dState (Withdrawals wdrls) network = do
  failureUnless (isSubmapOfUM @era wdrls (rewards dState)) $ -- withdrawals_ ⊆ rewards
    Map.differenceWith
      (\x y -> if x /= y then Just x else Nothing)
      wdrls
      (Map.mapKeys (RewardAccount network) (UM.rewardMap (dsUnified dState)))

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
