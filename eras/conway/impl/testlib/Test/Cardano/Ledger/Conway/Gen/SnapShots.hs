{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Ledger.Conway.Gen.SnapShots (
  genSnapShots,
) where

import Cardano.Ledger.BaseTypes (NonZero, unsafeNonZero)
import Cardano.Ledger.Coin (Coin, CompactForm (CompactCoin))
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (lsCertState, lsUTxOState),
  UTxOState (utxosFees),
 )
import Cardano.Ledger.State (
  ActiveStake (..),
  CanSetAccounts (..),
  DState,
  EraAccounts (..),
  EraCertState (..),
  EraStake,
  PState (..),
  SnapShot (..),
  SnapShots (SnapShots),
  StakeWithDelegation (..),
  calculatePoolDistr,
  instantStakeG,
  mkSnapShot,
  resetStakePoolsSnapShot,
  snapShotFromInstantStake,
 )
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Data.Word (Word64)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common (Gen, arbitrary, choose, elements, listOf)

genSnapShots :: (EraStake era, EraCertState era) => LedgerState era -> Gen SnapShots
genSnapShots ledgerState = do
  let certState = lsCertState ledgerState
      dState = certState ^. certDStateL
      pState = certState ^. certPStateL
      instStake = ledgerState ^. instantStakeG
      base = snapShotFromInstantStake instStake dState pState
  -- Derive mark by applying epoch-eligible operations (no pool deregistration):
  -- pool retirement takes effect only after the mark snapshot is taken.
  ssStakeMark <- alterSnapshot False dState pState base
  let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
  -- Derive set and go, which may additionally include pool deregistration.
  ssStakeSet <- alterSnapshot True dState pState ssStakeMark
  ssStakeGo <- alterSnapshot True dState pState ssStakeSet
  let ssFee = utxosFees (lsUTxOState ledgerState)
  pure $
    SnapShots
      ssStakeMark
      ssStakeMarkPoolDistr
      ssStakeSet
      ssStakeGo
      ssFee

-- | Alter a snapshot to simulate the evolution of stake distribution across epochs.
--
-- Randomly samples zero or more operations from the following set and applies them
-- in sequence to the initial snapshot:
--
-- * New stake registration: introduce a fresh credential delegated to an existing pool
-- * Stake address deregistration: remove a credential from active stake
-- * Stake address redelegation: redirect an already-delegated credential to a different pool
-- * First delegation: delegate a registered-but-undelegated credential to a pool
-- * Stake pool registration: add a new pool
-- * Stake amount change: adjust the stake of an existing credential (simulates UTxO movement)
--
-- When @isMarkSnapShot@ is 'False' (for set\/go snapshots), pool deregistration is also
-- included as a candidate operation. It is excluded from the mark snapshot because the
-- SNAP rule runs before POOLREAP in the epoch transition, so a retiring pool is still
-- visible in mark.
alterSnapshot ::
  EraAccounts era =>
  Bool ->
  DState era ->
  PState era ->
  SnapShot ->
  Gen SnapShot
alterSnapshot isMarkSnapShot dstate pstate initialSnapshot = do
  let snapShotOps = markSnapShotOps ++ [genPoolDereg | not isMarkSnapShot]
  selectedSnapShotOps <- listOf (elements snapShotOps)
  foldM (\s f -> f s) initialSnapshot selectedSnapShotOps
  where
    markSnapShotOps =
      [ genStakeCredRegDeleg pstate
      , genStakeDereg
      , genDelegChange pstate
      , genFirstDeleg dstate pstate
      , genPoolReg
      , genStakeAmountChange
      ]

    -- Register a new stake credential delegated to a random existing pool.
    genStakeCredRegDeleg PState {psStakePools} snapshot =
      case Map.keys psStakePools of
        [] -> pure snapshot
        pools -> do
          cred <- arbitrary
          stake <- genNonZeroCompactCoin
          pool <- elements pools
          let activeStakeMap = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
              newActiveStake =
                ActiveStake $
                  VMap.fromDistinctAscList $
                    Map.toAscList $
                      Map.insertWith
                        (\_ old -> old)
                        cred
                        (StakeWithDelegation stake pool)
                        activeStakeMap
          pure $
            resetStakePoolsSnapShot (VMap.fromMap psStakePools) $
              mkSnapShot newActiveStake VMap.empty

    -- Deregister a random stake credential.
    genStakeDereg snapshot =
      let activeStakeMap = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
       in case Map.keys activeStakeMap of
            [] -> pure snapshot
            creds -> do
              cred <- elements creds
              pure $ rebuildActiveStake snapshot (Map.delete cred activeStakeMap)

    -- Change the delegation target of a random stake credential.
    genDelegChange PState {psStakePools} snapshot =
      let activeStakeMap = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
       in case (Map.keys activeStakeMap, Map.keys psStakePools) of
            ([], _) -> pure snapshot
            (_, []) -> pure snapshot
            (creds, pools) -> do
              cred <- elements creds
              pool <- elements pools
              pure $
                rebuildActiveStake
                  snapshot
                  (Map.adjust (\swd -> swd {swdDelegation = pool}) cred activeStakeMap)

    -- Delegate a registered-but-undelegated credential to a random existing pool.
    genFirstDeleg dState PState {psStakePools} snapshot =
      let activeStakeMap = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
          undelegatedCreds =
            filter (`Map.notMember` activeStakeMap) $
              Map.keys (dState ^. accountsL . accountsMapL)
       in case (undelegatedCreds, Map.keys psStakePools) of
            ([], _) -> pure snapshot
            (_, []) -> pure snapshot
            (creds, pools) -> do
              cred <- elements creds
              stake <- genNonZeroCompactCoin
              pool <- elements pools
              pure $
                rebuildActiveStake
                  snapshot
                  (Map.insert cred (StakeWithDelegation stake pool) activeStakeMap)

    -- Register a new stake pool with an arbitrary snapshot.
    genPoolReg snapshot = do
      poolId <- arbitrary
      poolSnapShot <- arbitrary
      let newPools =
            VMap.fromMap $
              Map.insert poolId poolSnapShot $
                Map.fromList $
                  VMap.toAscList $
                    ssStakePoolsSnapShot snapshot
      pure $ mkSnapShot (ssActiveStake snapshot) newPools

    -- Change the stake amount of a random credential.
    genStakeAmountChange snapshot =
      let am = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
       in case Map.keys am of
            [] -> pure snapshot
            creds -> do
              cred <- elements creds
              stake <- genNonZeroCompactCoin
              pure $
                rebuildActiveStake
                  snapshot
                  (Map.adjust (\swd -> swd {swdStake = stake}) cred am)

    -- Remove a random pool and all credentials delegating to it.
    genPoolDereg snapshot =
      case VMap.toAscList (ssStakePoolsSnapShot snapshot) of
        [] -> pure snapshot
        poolEntries -> do
          (poolId, _) <- elements poolEntries
          let am = Map.fromList $ VMap.toAscList $ unActiveStake $ ssActiveStake snapshot
              newAm = Map.filter (\swd -> swdDelegation swd /= poolId) am
              newPools =
                VMap.fromMap $
                  Map.delete poolId $
                    Map.fromList $
                      VMap.toAscList $
                        ssStakePoolsSnapShot snapshot
          pure $
            mkSnapShot
              (ActiveStake $ VMap.fromDistinctAscList $ Map.toAscList newAm)
              newPools

    -- Rebuild a snapshot with updated active stake, preserving existing pool snapshots.
    rebuildActiveStake snapshot am =
      mkSnapShot
        (ActiveStake $ VMap.fromDistinctAscList $ Map.toAscList am)
        (ssStakePoolsSnapShot snapshot)

-- | Generate a positive compact coin for use as stake.
genNonZeroCompactCoin :: Gen (NonZero (CompactForm Coin))
genNonZeroCompactCoin = unsafeNonZero . CompactCoin <$> choose (1 :: Word64, 1_000_000_000_000)
