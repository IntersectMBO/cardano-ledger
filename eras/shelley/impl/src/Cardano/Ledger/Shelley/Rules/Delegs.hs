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

module Cardano.Ledger.Shelley.Rules.Delegs
  ( ShelleyDELEGS,
    DelegsEnv (..),
    ShelleyDelegsPredFailure (..),
    ShelleyDelegsEvent (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.Address (mkRwdAcnt)
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    TxIx,
    invalidKey,
    mkCertIxPartial,
    networkId,
  )
import Cardano.Ledger.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeRecordSum,
    encodeListLen,
  )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyDELEGS)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DPState (..),
    RewardAccounts,
    dpsDState,
    dsUnified,
    psStakePoolParams,
    rewards,
  )
import Cardano.Ledger.Shelley.Rules.Delpl (DelplEnv (..), ShelleyDELPL, ShelleyDelplEvent, ShelleyDelplPredFailure)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    Ptr (..),
    RewardAcnt (..),
    ShelleyEraTxBody (..),
    Wdrl (..),
  )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.UnifiedMap (Trip (..), UMap (..), View (..), ViewMap)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (∈))
import Control.State.Transition
  ( Embed (..),
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
import qualified Data.UMap as UM
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data DelegsEnv era = DelegsEnv
  { delegsSlotNo :: !SlotNo,
    delegsIx :: !TxIx,
    delegspp :: !(PParams era),
    delegsTx :: !(Tx era),
    delegsAccount :: !AccountState
  }

deriving stock instance
  ( Show (Tx era),
    Show (PParams era)
  ) =>
  Show (DelegsEnv era)

data ShelleyDelegsPredFailure era
  = DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era)) -- target pool which is not registered
  | WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt (EraCrypto era)) Coin) -- withdrawals that are missing or do not withdrawl the entire amount
  | DelplFailure (PredicateFailure (EraRule "DELPL" era)) -- Subtransition Failures
  deriving (Generic)

newtype ShelleyDelegsEvent era = DelplEvent (Event (EraRule "DELPL" era))

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELPL" era))
  ) =>
  Show (ShelleyDelegsPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELPL" era))
  ) =>
  Eq (ShelleyDelegsPredFailure era)

instance
  ( EraTx era,
    ShelleyEraTxBody era,
    Embed (EraRule "DELPL" era) (ShelleyDELEGS era),
    Environment (EraRule "DELPL" era) ~ DelplEnv era,
    State (EraRule "DELPL" era) ~ DPState (EraCrypto era),
    Signal (EraRule "DELPL" era) ~ DCert (EraCrypto era)
  ) =>
  STS (ShelleyDELEGS era)
  where
  type State (ShelleyDELEGS era) = DPState (EraCrypto era)
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
  ( Era era,
    Typeable (Script era),
    ToCBOR (PredicateFailure (EraRule "DELPL" era))
  ) =>
  ToCBOR (ShelleyDelegsPredFailure era)
  where
  toCBOR = \case
    DelegateeNotRegisteredDELEG kh ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR kh
    WithdrawalsNotInRewardsDELEGS ws ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR ws
    (DelplFailure a) ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR a

instance
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "DELPL" era)),
    Typeable (Script era)
  ) =>
  FromCBOR (ShelleyDelegsPredFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailure" $
      \case
        0 -> do
          kh <- fromCBOR
          pure (2, DelegateeNotRegisteredDELEG kh)
        1 -> do
          ws <- fromCBOR
          pure (2, WithdrawalsNotInRewardsDELEGS ws)
        2 -> do
          a <- fromCBOR
          pure (2, DelplFailure a)
        k -> invalidKey k

delegsTransition ::
  forall era.
  ( EraTx era,
    ShelleyEraTxBody era,
    Embed (EraRule "DELPL" era) (ShelleyDELEGS era),
    Environment (EraRule "DELPL" era) ~ DelplEnv era,
    State (EraRule "DELPL" era) ~ DPState (EraCrypto era),
    Signal (EraRule "DELPL" era) ~ DCert (EraCrypto era)
  ) =>
  TransitionRule (ShelleyDELEGS era)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let ds = dpsDState dpstate
          wdrls_ = unWdrl (tx ^. bodyTxL . wdrlsTxBodyL)
          rewards' = rewards ds
      isSubmapOf wdrls_ rewards' -- wdrls_ ⊆ rewards
        ?! WithdrawalsNotInRewardsDELEGS
          ( Map.differenceWith
              (\x y -> if x /= y then Just x else Nothing)
              wdrls_
              (Map.mapKeys (mkRwdAcnt network) (UM.unUnify rewards'))
          )

      let wdrls_' :: RewardAccounts (EraCrypto era)
          wdrls_' =
            Map.foldrWithKey
              ( \(RewardAcnt _ cred) _coin ->
                  Map.insert cred mempty
              )
              Map.empty
              wdrls_
          unified' = rewards' UM.⨃ wdrls_'
      pure $ dpstate {dpsDState = ds {dsUnified = unified'}}
    gamma :|> c -> do
      dpstate' <-
        trans @(ShelleyDELEGS era) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let stPools_ = psStakePoolParams $ dpsPState dpstate'
                  targetPool = dDelegatee deleg
               in if eval (targetPool ∈ dom stPools_)
                    then Right ()
                    else Left $ DelegateeNotRegisteredDELEG targetPool
            _ -> Right ()
      isDelegationRegistered ?!: id

      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "DELPL" era) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)
  where
    -- @wdrls_@ is small and @rewards@ big, better to transform the former
    -- than the latter into the right shape so we can call 'Map.isSubmapOf'.
    isSubmapOf ::
      Map (RewardAcnt (EraCrypto era)) Coin ->
      ViewMap (EraCrypto era) (Credential 'Staking c) Coin ->
      Bool
    isSubmapOf wdrls_ (Rewards (UnifiedMap tripmap _)) = Map.isSubmapOfBy f withdrawalMap tripmap
      where
        withdrawalMap =
          Map.fromList
            [ (cred, coin)
              | (RewardAcnt _ cred, coin) <- Map.toList wdrls_
            ]
        f coin1 (Triple (SJust coin2) _ _) = coin1 == coin2
        f _ _ = False

instance
  ( Era era,
    STS (ShelleyDELPL era),
    PredicateFailure (EraRule "DELPL" era) ~ ShelleyDelplPredFailure era,
    Event (EraRule "DELPL" era) ~ ShelleyDelplEvent era
  ) =>
  Embed (ShelleyDELPL era) (ShelleyDELEGS era)
  where
  wrapFailed = DelplFailure
  wrapEvent = DelplEvent
