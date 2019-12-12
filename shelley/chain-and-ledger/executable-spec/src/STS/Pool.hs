{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Pool
  ( POOL,
    PoolEnv (..),
  )
where

import           BaseTypes
import           Cardano.Ledger.Shelley.Crypto
import           Control.Monad.Trans.Reader (asks, runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Functor.Identity (runIdentity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Delegation.Certificates
import           Hedgehog (Gen)
import           Keys
import           Ledger.Core (dom, (∈), (∉), (⋪))
import           LedgerState
import           Lens.Micro ((^.))
import           PParams
import           Slot
import           TxData

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
    deriving (Show, Eq)

  initialRules = [pure emptyPState]

  transitionRules = [poolDelegationTransition]

poolDelegationTransition :: TransitionRule (POOL crypto)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let StakePools stPools_ = _stPools ps
  case c of
    RegPool poolParam -> do
      let hk = poolParam ^. poolPubKey
      if hk ∉ dom stPools_
        then-- register new

          pure $
            ps
              { _stPools = StakePools $ stPools_ ∪ (hk, slot),
                _pParams = _pParams ps ∪ (hk, poolParam)
              }
        else-- re-register

          pure $
            ps
              { _pParams = _pParams ps ⨃ (hk, poolParam),
                _retiring = Set.singleton hk ⋪ _retiring ps
              }
    RetirePool hk (EpochNo e) -> do
      EpochNo cepoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      let EpochNo maxEpoch = pp ^. eMax
      hk ∈ dom stPools_ ?! StakePoolNotRegisteredOnKeyPOOL
      cepoch < e && e < cepoch + maxEpoch ?! StakePoolRetirementWrongEpochPOOL
      pure $ ps {_retiring = _retiring ps ⨃ (hk, EpochNo e)}
    _ -> do
      failBecause WrongCertificateTypePOOL
      pure ps

-- Note: we avoid using the Relation operators (⨃) and (∪) here because that
-- would require an Ord instance for PParams, which we don't need otherwise.
-- Instead, we just define these operators here.

(⨃) ::
  Map (KeyHash crypto) a ->
  (KeyHash crypto, a) ->
  Map (KeyHash crypto) a
m ⨃ (k, v) = Map.union (Map.singleton k v) m

(∪) ::
  Map (KeyHash crypto) a ->
  (KeyHash crypto, a) ->
  Map (KeyHash crypto) a
m ∪ (k, v) = Map.union m (Map.singleton k v)

instance
  Crypto crypto =>
  HasTrace (POOL crypto)
  where

  envGen _ = undefined :: Gen PoolEnv

  sigGen _ _ = undefined :: Gen (DCert crypto)

  type BaseEnv (POOL crypto) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals
