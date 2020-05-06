{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Pool
  ( POOL,
    PoolEnv (..),
    PredicateFailure (..),
  )
where

import Byron.Spec.Ledger.Core (dom, (∈), (∉), (⋪))
import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeWord)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.TxData

data POOL crypto

data PoolEnv
  = PoolEnv SlotNo PParams
  deriving (Show, Eq)

instance STS (POOL crypto) where
  type State (POOL crypto) = PState crypto

  type Signal (POOL crypto) = DCert crypto

  type Environment (POOL crypto) = PoolEnv

  type BaseM (POOL crypto) = ShelleyBase

  data PredicateFailure (POOL crypto)
    = StakePoolNotRegisteredOnKeyPOOL
    | StakePoolRetirementWrongEpochPOOL
    | WrongCertificateTypePOOL
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyPState]

  transitionRules = [poolDelegationTransition]

instance NoUnexpectedThunks (PredicateFailure (POOL crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (POOL crypto))
  where
  toCBOR = \case
    StakePoolNotRegisteredOnKeyPOOL -> toCBOR (0 :: Word8)
    StakePoolRetirementWrongEpochPOOL -> toCBOR (1 :: Word8)
    WrongCertificateTypePOOL -> toCBOR (2 :: Word8)

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (POOL crypto))
  where
  fromCBOR = do
    decodeWord >>= \case
      0 -> pure StakePoolNotRegisteredOnKeyPOOL
      1 -> pure StakePoolRetirementWrongEpochPOOL
      2 -> pure WrongCertificateTypePOOL
      k -> invalidKey k

poolDelegationTransition :: TransitionRule (POOL crypto)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let StakePools stpools = _stPools ps
  case c of
    DCertPool (RegPool poolParam) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      let hk = _poolPubKey poolParam
      if hk ∉ dom stpools
        then-- register new, Pool-Reg

          pure $
            ps
              { _stPools = StakePools $ stpools ∪ (hk, slot),
                _pParams = _pParams ps ∪ (hk, poolParam)
              }
        else-- re-register, Pool-reReg

          pure $
            ps
              { _pParams = _pParams ps ⨃ (hk, poolParam),
                _retiring = Set.singleton hk ⋪ _retiring ps
              }
    DCertPool (RetirePool hk (EpochNo e)) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      hk ∈ dom stpools ?! StakePoolNotRegisteredOnKeyPOOL
      EpochNo cepoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      let EpochNo maxEpoch = _eMax pp
      cepoch < e && e < cepoch + maxEpoch ?! StakePoolRetirementWrongEpochPOOL
      pure $ ps {_retiring = _retiring ps ⨃ (hk, EpochNo e)}
    _ -> do
      failBecause WrongCertificateTypePOOL
      pure ps

-- Note: we avoid using the Relation operators (⨃) and (∪) here because that
-- would require an Ord instance for PParams, which we don't need otherwise.
-- Instead, we just define these operators here.

(⨃) ::
  Map (KeyHash kr crypto) a ->
  (KeyHash kr crypto, a) ->
  Map (KeyHash kr crypto) a
m ⨃ (k, v) = Map.union (Map.singleton k v) m

(∪) ::
  Map (KeyHash kr crypto) a ->
  (KeyHash kr crypto, a) ->
  Map (KeyHash kr crypto) a
m ∪ (k, v) = Map.union m (Map.singleton k v)
