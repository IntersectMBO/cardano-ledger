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

module Cardano.Ledger.Shelley.Rules.Delegs
  ( DELEGS,
    DelegsEnv (..),
    DelegsPredicateFailure (..),
    DelegsEvent (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( Annotator,
    Decoder,
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Address (mkRwdAcnt)
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    invalidKey,
    networkId,
  )
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Serialization
  ( decodeRecordSum,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DPState (..),
    RewardAccounts,
    _dstate,
    _pParams,
    _rewards,
  )
import Cardano.Ledger.Shelley.Rules.Delpl (DELPL, DelplEnv (..), DelplEvent, DelplPredicateFailure)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    Ix,
    Ptr (..),
    RewardAcnt (..),
    Wdrl (..),
  )
import Cardano.Ledger.Slot (SlotNo)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (∈), (⨃))
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
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data DELEGS era

data DelegsEnv era = DelegsEnv
  { delegsSlotNo :: SlotNo,
    delegsIx :: Ix,
    delegspp :: Core.PParams era,
    delegsTx :: Core.Tx era,
    delegsAccount :: AccountState
  }

deriving stock instance
  ( Show (Core.Tx era),
    Show (Core.PParams era)
  ) =>
  Show (DelegsEnv era)

data DelegsPredicateFailure era
  = DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (Crypto era)) -- target pool which is not registered
  | WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt (Crypto era)) Coin) -- withdrawals that are missing or do not withdrawl the entire amount
  | DelplFailure (PredicateFailure (Core.EraRule "DELPL" era)) -- Subtransition Failures
  deriving (Generic)

newtype DelegsEvent era = DelplEvent (Event (Core.EraRule "DELPL" era))

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  Show (DelegsPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  Eq (DelegsPredicateFailure era)

instance
  ( Era era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Embed (Core.EraRule "DELPL" era) (DELEGS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  STS (DELEGS era)
  where
  type State (DELEGS era) = DPState (Crypto era)
  type Signal (DELEGS era) = Seq (DCert (Crypto era))
  type Environment (DELEGS era) = DelegsEnv era
  type BaseM (DELEGS era) = ShelleyBase
  type
    PredicateFailure (DELEGS era) =
      DelegsPredicateFailure era
  type Event _ = DelegsEvent era

  transitionRules = [delegsTransition]

instance
  ( NoThunks (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  NoThunks (DelegsPredicateFailure era)

instance
  ( Era era,
    Typeable (Core.Script era),
    ToCBOR (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  ToCBOR (DelegsPredicateFailure era)
  where
  toCBOR = \case
    DelegateeNotRegisteredDELEG kh ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR kh
    WithdrawalsNotInRewardsDELEGS ws ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> mapToCBOR ws
    (DelplFailure a) ->
      encodeListLen 2 <> toCBOR (2 :: Word8)
        <> toCBOR a

instance
  ( Era era,
    FromCBOR (Annotator (PredicateFailure (Core.EraRule "DELPL" era))),
    Typeable (Core.Script era)
  ) =>
  FromCBOR (Annotator (DelegsPredicateFailure era))
  where
  fromCBOR = decodePredFail

decodePredFail ::
  ( Era era,
    Applicative f,
    FromCBOR (f (PredicateFailure (Core.EraRule "DELPL" era)))
  ) =>
  (Decoder s (f (DelegsPredicateFailure era)))
decodePredFail =
  decodeRecordSum "PredicateFailure" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, pure $ DelegateeNotRegisteredDELEG kh)
      1 -> do
        ws <- mapFromCBOR
        pure (2, pure $ WithdrawalsNotInRewardsDELEGS ws)
      2 -> do
        a <- fromCBOR
        pure (2, fmap DelplFailure a)
      k -> invalidKey k

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "DELPL" era)),
    Typeable (Core.Script era)
  ) =>
  FromCBOR (DelegsPredicateFailure era)
  where
  fromCBOR = runIdentity <$> decodePredFail

delegsTransition ::
  forall era.
  ( Era era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Embed (Core.EraRule "DELPL" era) (DELEGS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  TransitionRule (DELEGS era)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let ds = _dstate dpstate
          wdrls_ = unWdrl . getField @"wdrls" $ getField @"body" tx
          rewards = _rewards ds

      isSubmapOf wdrls_ rewards -- wdrls_ ⊆ rewards
        ?! WithdrawalsNotInRewardsDELEGS
          ( Map.differenceWith
              (\x y -> if x /= y then Just x else Nothing)
              wdrls_
              (Map.mapKeys (mkRwdAcnt network) rewards)
          )

      let wdrls_' :: RewardAccounts (Crypto era)
          wdrls_' =
            Map.foldrWithKey
              ( \(RewardAcnt _ cred) _coin ->
                  Map.insert cred mempty
              )
              Map.empty
              wdrls_
          rewards' = eval (rewards ⨃ wdrls_')
      pure $ dpstate {_dstate = ds {_rewards = rewards'}}
    gamma :|> c -> do
      dpstate' <-
        trans @(DELEGS era) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let stPools_ = _pParams $ _pstate dpstate'
                  targetPool = _delegatee deleg
               in case eval (targetPool ∈ dom stPools_) of
                    True -> Right ()
                    False -> Left $ DelegateeNotRegisteredDELEG targetPool
            _ -> Right ()
      isDelegationRegistered ?!: id

      let ptr = Ptr slot txIx (fromIntegral $ length gamma)
      trans @(Core.EraRule "DELPL" era) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)
  where
    -- @wdrls_@ is small and @rewards@ big, better to transform the former
    -- than the latter into the right shape so we can call 'Map.isSubmapOf'.
    isSubmapOf :: Map (RewardAcnt (Crypto era)) Coin -> RewardAccounts (Crypto era) -> Bool
    isSubmapOf wdrls_ rewards = wdrls_' `Map.isSubmapOf` rewards
      where
        wdrls_' =
          Map.fromList
            [ (cred, coin)
              | (RewardAcnt _ cred, coin) <- Map.toList wdrls_
            ]

instance
  ( Era era,
    STS (DELPL era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ DelplPredicateFailure era,
    Event (Core.EraRule "DELPL" era) ~ DelplEvent era
  ) =>
  Embed (DELPL era) (DELEGS era)
  where
  wrapFailed = DelplFailure
  wrapEvent = DelplEvent
