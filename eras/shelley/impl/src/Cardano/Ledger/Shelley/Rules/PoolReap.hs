{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
  ShelleyPoolreapEvent (..),
  ShelleyPoolreapState (..),
  prCertStateL,
  prChainAccountStateL,
  prUTxOStateL,
  PredicateFailure,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  allObligations,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (potEqualsObligation)
import Cardano.Ledger.State
import Cardano.Ledger.Val ((<+>), (<->))
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Assertion (..),
  AssertionViolation (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Foldable as F (foldl')
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro

data ShelleyPoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era
  , prChainAccountState :: ChainAccountState
  , prCertState :: CertState era
  }

deriving stock instance
  (Show (UTxOState era), Show (CertState era)) => Show (ShelleyPoolreapState era)

prUTxOStateL :: Lens' (ShelleyPoolreapState era) (UTxOState era)
prUTxOStateL = lens prUTxOSt $ \sprs x -> sprs {prUTxOSt = x}

prChainAccountStateL :: Lens' (ShelleyPoolreapState era) ChainAccountState
prChainAccountStateL = lens prChainAccountState $ \sprs x -> sprs {prChainAccountState = x}

prCertStateL :: Lens' (ShelleyPoolreapState era) (CertState era)
prCertStateL = lens prCertState $ \sprs x -> sprs {prCertState = x}

data ShelleyPoolreapEvent era = RetiredPools
  { refundPools ::
      Map.Map (Credential Staking) (Map.Map (KeyHash StakePool) (CompactForm Coin))
  , unclaimedPools ::
      Map.Map (Credential Staking) (Map.Map (KeyHash StakePool) (CompactForm Coin))
  , epochNo :: EpochNo
  }
  deriving (Generic)

deriving instance Eq (ShelleyPoolreapEvent era)

instance NFData (ShelleyPoolreapEvent era)

instance (Default (UTxOState era), Default (CertState era)) => Default (ShelleyPoolreapState era) where
  def = PoolreapState def def def

type instance EraRuleEvent "POOLREAP" ShelleyEra = ShelleyPoolreapEvent ShelleyEra

instance
  ( Default (ShelleyPoolreapState era)
  , EraPParams era
  , EraGov era
  , EraCertState era
  ) =>
  STS (ShelleyPOOLREAP era)
  where
  type State (ShelleyPOOLREAP era) = ShelleyPoolreapState era
  type Signal (ShelleyPOOLREAP era) = EpochNo
  type Environment (ShelleyPOOLREAP era) = ()
  type BaseM (ShelleyPOOLREAP era) = ShelleyBase
  type PredicateFailure (ShelleyPOOLREAP era) = Void
  type Event (ShelleyPOOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]

  renderAssertionViolation = renderPoolReapViolation
  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (PoolReap)"
        ( \_trc st ->
            potEqualsObligation
              (prCertState st)
              (prUTxOSt st)
        )
    , PostCondition
        "PoolReap may not create or remove account addresses"
        ( \(TRC (_, st, _)) st' ->
            let accountsCount prState =
                  Map.size (prCertState prState ^. certDStateL . accountsL . accountsMapL)
             in accountsCount st == accountsCount st'
        )
    ]

poolReapTransition :: forall era. EraCertState era => TransitionRule (ShelleyPOOLREAP era)
poolReapTransition = do
  TRC (_, PoolreapState us a cs0, e) <- judgmentContext
  let
    ps0 = cs0 ^. certPStateL
    -- find the set of VRF key hashes that are no longer relevant, since they have been overwritten
    -- via pool re-registration
    danglingVRFKeyHashes =
      Set.fromList $
        Map.elems $
          Map.merge
            Map.dropMissing
            Map.dropMissing
            ( Map.zipWithMaybeMatched $ \_ sps sppF ->
                if sps ^. spsVrfL /= sppF ^. sppVrfL then Just (sps ^. spsVrfL) else Nothing
            )
            (ps0 ^. psStakePoolsL)
            (ps0 ^. psFutureStakePoolParamsL)

    -- activate future stakePools
    ps =
      ps0
        { psStakePools =
            Map.merge
              Map.dropMissing
              Map.preserveMissing
              ( Map.zipWithMatched $ \_ futureParams currentState ->
                  mkStakePoolState
                    (currentState ^. spsDepositL)
                    (currentState ^. spsDelegatorsL)
                    futureParams
              )
              (ps0 ^. psFutureStakePoolParamsL)
              (ps0 ^. psStakePoolsL)
        , psFutureStakePoolParams = Map.empty
        }
    cs = cs0 & certPStateL .~ ps

    ds = cs ^. certDStateL
    -- The set of pools retiring this epoch
    retired :: Set (KeyHash StakePool)
    retired = Set.fromDistinctAscList [k | (k, v) <- Map.toAscList (psRetiring ps), v == e]
    -- The Map of pools retiring this epoch
    retiringPools :: Map.Map (KeyHash StakePool) StakePoolState
    retiringPools = Map.restrictKeys (psStakePools ps) retired
    -- collect all accounts for stake pools that will retire
    retiredVRFKeyHashes = spsVrf <$> Map.elems retiringPools

    -- collect all of the potential refunds
    accountRefunds :: Map.Map (Credential Staking) (CompactForm Coin)
    accountRefunds =
      Map.fromListWith
        (<>)
        [(spsAccountAddress sps, spsDeposit sps) | sps <- Map.elems retiringPools]
    accounts = ds ^. accountsL
    -- Deposits that can be refunded and those that are unclaimed (to be deposited into the treasury).
    refunds, unclaimedDeposits :: Map.Map (Credential Staking) (CompactForm Coin)
    (refunds, unclaimedDeposits) =
      Map.partitionWithKey
        (\stakeCred _ -> isAccountRegistered stakeCred accounts) -- (k ∈ dom (rewards ds))
        accountRefunds

    refunded = fold refunds
    unclaimed = fold unclaimedDeposits

  tellEvent $
    let rewardAccountsWithPool =
          Map.foldrWithKey'
            ( \poolId sps ->
                let cred = spsAccountAddress sps
                 in Map.insertWith (Map.unionWith (<>)) cred (Map.singleton poolId (spsDeposit sps))
            )
            Map.empty
            retiringPools
        (refundPools', unclaimedPools') =
          Map.partitionWithKey
            (\cred _ -> isAccountRegistered cred accounts)
            rewardAccountsWithPool
     in RetiredPools
          { refundPools = refundPools'
          , unclaimedPools = unclaimedPools'
          , epochNo = e
          }
  pure $
    PoolreapState
      us {utxosDeposited = utxosDeposited us <-> fromCompact (unclaimed <> refunded)}
      a {casTreasury = casTreasury a <+> fromCompact unclaimed}
      ( cs
          & certDStateL . accountsL
            %~ removeStakePoolDelegations (delegsToClear cs retired)
              . addToBalanceAccounts refunds
          & certPStateL . psStakePoolsL %~ (`Map.withoutKeys` retired)
          & certPStateL . psRetiringL %~ (`Map.withoutKeys` retired)
          & certPStateL . psVRFKeyHashesL
            %~ ( removeVRFKeyHashOccurrences retiredVRFKeyHashes
                   . (`Map.withoutKeys` danglingVRFKeyHashes)
               )
      )
  where
    removeVRFKeyHashOccurrences ::
      [VRFVerKeyHash StakePoolVRF] ->
      Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64) ->
      Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
    removeVRFKeyHashOccurrences vrfs vrfsMap = F.foldl' (flip removeVRFKeyHashOccurrence) vrfsMap vrfs
    removeVRFKeyHashOccurrence =
      -- Removes the key from the map if the value drops to 0
      Map.update (mapNonZero (\n -> n - 1))
    delegsToClear cState pools =
      foldMap spsDelegators $
        Map.restrictKeys (cState ^. certPStateL . psStakePoolsL) pools

renderPoolReapViolation ::
  ( EraGov era
  , State t ~ ShelleyPoolreapState era
  , EraCertState era
  ) =>
  AssertionViolation t ->
  String
renderPoolReapViolation
  AssertionViolation {avSTS, avMsg, avCtx = TRC (_, poolreapst, _)} =
    let obligations =
          allObligations (prCertState poolreapst) (prUTxOSt poolreapst ^. utxosGovStateL)
     in "\n\nAssertionViolation ("
          <> avSTS
          <> ")\n   "
          <> avMsg
          <> "\npot (utxosDeposited) = "
          <> show (utxosDeposited (prUTxOSt poolreapst))
          <> show obligations
