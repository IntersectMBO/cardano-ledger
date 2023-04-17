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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Delegs (
  ShelleyDELEGS,
  DelegsEnv (..),
  ShelleyDelegsPredFailure (..),
  ShelleyDelegsEvent (..),
  PredicateFailure,
  zeroRewards,
  isDelegationRegistered,
  drainedRewardAccounts,
  isSubmapOfUM,
)
where

import Cardano.Ledger.Address (mkRwdAcnt)
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
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyDELEGS)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState (..),
  certDState,
  dsUnified,
  psStakePoolParams,
  rewards,
 )
import Cardano.Ledger.Shelley.Rules.Delpl (DelplEnv (..), ShelleyDELPL, ShelleyDelplEvent, ShelleyDelplPredFailure)
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  DelegCert (..),
  Delegation (..),
  Ptr (..),
  RewardAcnt (..),
  ShelleyEraTxBody (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.UMap (Trip (..), UMap (..), View (..), fromCompact)
import qualified Cardano.Ledger.UMap as UM
import Control.Arrow (left)
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
  (?!),
  (?!:),
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
  = DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era)) -- target pool which is not registered
  | WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt (EraCrypto era)) Coin) -- withdrawals that are missing or do not withdrawal the entire amount
  | DelplFailure (PredicateFailure (EraRule "DELPL" era)) -- Subtransition Failures
  deriving (Generic)

newtype ShelleyDelegsEvent era = DelplEvent (Event (EraRule "DELPL" era))

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
  , Signal (EraRule "DELPL" era) ~ DCert (EraCrypto era)
  , EraRule "DELEGS" era ~ ShelleyDELEGS era
  ) =>
  STS (ShelleyDELEGS era)
  where
  type State (ShelleyDELEGS era) = CertState era
  type Signal (ShelleyDELEGS era) = Seq (DCert (EraCrypto era))
  type Environment (ShelleyDELEGS era) = DelegsEnv era
  type BaseM (ShelleyDELEGS era) = ShelleyBase
  type
    PredicateFailure (ShelleyDELEGS era) =
      ShelleyDelegsPredFailure era
  type Event (ShelleyDELEGS era) = ShelleyDelegsEvent era

  transitionRules = [delegsTransition]

instance
  ( NoThunks (PredicateFailure (EraRule "DELPL" era))
  ) =>
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
  , Signal (EraRule "DELPL" era) ~ DCert (EraCrypto era)
  , EraRule "DELEGS" era ~ ShelleyDELEGS era
  ) =>
  TransitionRule (ShelleyDELEGS era)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> zeroRewards dpstate tx network
    gamma :|> c -> do
      dpstate' <-
        trans @(ShelleyDELEGS era) $ TRC (env, dpstate, gamma)
      left DelegateeNotRegisteredDELEG (isDelegationRegistered @era dpstate' c)
        ?!: id
      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "DELPL" era) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)

isDelegationRegistered ::
  CertState era ->
  DCert (EraCrypto era) ->
  Either (KeyHash 'StakePool (EraCrypto era)) ()
isDelegationRegistered dpstate' c = case c of
  DCertDeleg (Delegate deleg) ->
    let stPools_ = psStakePoolParams $ certPState dpstate'
        targetPool = dDelegatee deleg
     in if eval (targetPool ∈ dom stPools_)
          then Right ()
          else Left targetPool
  _ -> Right ()

-- @withdrawals_@ is small and @rewards@ big, better to transform the former
-- than the latter into the right shape so we can call 'Map.isSubmapOf'.
isSubmapOfUM ::
  forall era.
  Map (RewardAcnt (EraCrypto era)) Coin ->
  View (EraCrypto era) (Credential 'Staking (EraCrypto era)) UM.RDPair ->
  Bool
isSubmapOfUM ws (RewardDeposits (UMap tripmap _)) = Map.isSubmapOfBy f withdrawalMap tripmap
  where
    withdrawalMap :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    withdrawalMap =
      Map.fromList
        [ (cred, coin)
        | (RewardAcnt _ cred, coin) <- Map.toList ws
        ]
    f :: Coin -> Trip (EraCrypto era) -> Bool
    f coin1 (Triple (SJust (UM.RDPair coin2 _)) _ _) = coin1 == fromCompact coin2
    f _ _ = False

drainedRewardAccounts ::
  Map (RewardAcnt (EraCrypto era)) a ->
  Map.Map (Credential 'Staking (EraCrypto era)) UM.RDPair
drainedRewardAccounts =
  Map.foldrWithKey
    ( \(RewardAcnt _ cred) _coin ->
        Map.insert cred (UM.RDPair (UM.CompactCoin 0) (UM.CompactCoin 0))
        -- Note that the deposit (CompactCoin 0) will be ignored.
    )
    Map.empty

zeroRewards ::
  forall era.
  ( EraTx era
  , State (EraRule "DELEGS" era) ~ CertState era
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  ) =>
  CertState era ->
  Tx era ->
  Network ->
  TransitionRule (EraRule "DELEGS" era)
zeroRewards dpstate tx network = do
  let ds = certDState dpstate
      wdrls = unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
      rewards' = rewards ds
  isSubmapOfUM @era wdrls rewards' -- withdrawals_ ⊆ rewards
    ?! WithdrawalsNotInRewardsDELEGS @era
      ( Map.differenceWith
          (\x y -> if x /= y then Just x else Nothing)
          wdrls
          (Map.mapKeys (mkRwdAcnt network) (UM.rewView (dsUnified ds)))
      )
  let unified' = rewards' UM.⨃ drainedRewardAccounts @era wdrls
  pure $ dpstate {certDState = ds {dsUnified = unified'}}

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
