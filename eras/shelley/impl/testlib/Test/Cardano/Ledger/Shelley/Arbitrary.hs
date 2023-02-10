{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Arbitrary where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Crypto
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.RewardUpdate
import Cardano.Ledger.Shelley.Rewards (
  LeaderOnlyReward (..),
  PoolRewardInfo (..),
  StakeShare (..),
 )
import Cardano.Ledger.Shelley.TxOut
import Control.Monad.Identity (Identity)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.PParams --------------------------------------------------------
------------------------------------------------------------------------------------------

instance Era era => Arbitrary (ShelleyPParams Identity era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (ShelleyPParams StrictMaybe era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

deriving instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ProposedPPUpdates era)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.TxOut ----------------------------------------------------------
------------------------------------------------------------------------------------------

instance (EraTxOut era, Arbitrary (Value era)) => Arbitrary (ShelleyTxOut era) where
  arbitrary = ShelleyTxOut <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.LedgerState ----------------------------------------------------
------------------------------------------------------------------------------------------

instance
  ( EraTxOut era
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (GovernanceState era)
  ) =>
  Arbitrary (NewEpochState era)
  where
  arbitrary =
    NewEpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( EraTxOut era
  , Arbitrary (TxOut era)
  , Arbitrary (PParams era)
  , Arbitrary (GovernanceState era)
  ) =>
  Arbitrary (EpochState era)
  where
  arbitrary =
    EpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (SnapShot c) where
  arbitrary =
    SnapShot
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (SnapShots c) where
  arbitrary = do
    ssStakeMark <- arbitrary
    ssStakeSet <- arbitrary
    ssStakeGo <- arbitrary
    ssFee <- arbitrary
    let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
    pure $ SnapShots {..}
  shrink = genericShrink

instance
  ( EraTxOut era
  , Arbitrary (TxOut era)
  , Arbitrary (GovernanceState era)
  ) =>
  Arbitrary (LedgerState era)
  where
  arbitrary =
    LedgerState
      <$> arbitrary
      <*> arbitrary
  shrink LedgerState {..} =
    -- We drop the first element in the list so the list does not contain the
    -- original LedgerState which would cause `shrink` to loop indefinitely.
    -- This call of `tail` is safe since the list guaranteed to have at least
    -- one element.
    tail $
      LedgerState
        <$> (lsUTxOState : shrink lsUTxOState)
        <*> (lsDPState : shrink lsDPState)

instance
  ( EraTxOut era
  , Arbitrary (TxOut era)
  , Arbitrary (GovernanceState era)
  ) =>
  Arbitrary (UTxOState era)
  where
  arbitrary =
    UTxOState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

  -- The 'genericShrink' function returns first the immediate subterms of a
  -- value (in case it is a recursive data-type), and then shrinks the value
  -- itself. Since 'UTxOState' is not a recursive data-type, there are no
  -- subterms, and we can use `recursivelyShrink` directly. This is particularly
  -- important when abstracting away the different fields of the ledger state,
  -- since the generic subterms instances will overlap due to GHC not having
  -- enough context to infer if 'a' and 'b' are the same types (since in this
  -- case this will depend on the definition of 'era').
  --
  -- > instance OVERLAPPING_ GSubtermsIncl (K1 i a) a where
  -- > instance OVERLAPPING_ GSubtermsIncl (K1 i a) b where
  shrink = recursivelyShrink

instance Arbitrary AccountState where
  arbitrary = AccountState <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (IncrementalStake c) where
  arbitrary = IStake <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.PoolRank -------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary Likelihood where
  arbitrary = Likelihood <$> arbitrary

instance Arbitrary LogWeight where
  arbitrary = LogWeight <$> arbitrary

instance Arbitrary PerformanceEstimate where
  arbitrary = PerformanceEstimate <$> arbitrary

instance Crypto c => Arbitrary (NonMyopic c) where
  arbitrary = NonMyopic <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.Rewards --------------------------------------------------------
------------------------------------------------------------------------------------------
deriving instance Arbitrary StakeShare

instance Crypto c => Arbitrary (LeaderOnlyReward c) where
  arbitrary = LeaderOnlyReward <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (PoolRewardInfo c) where
  arbitrary =
    PoolRewardInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.RewardUpdate ---------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (RewardUpdate c) where
  arbitrary =
    RewardUpdate
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (RewardAns c) where
  arbitrary = RewardAns <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Crypto c, a ~ RewardAns c) => Arbitrary (RewardPulser c ShelleyBase a) where
  arbitrary = RSLP <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (PulsingRewUpdate c) where
  arbitrary =
    oneof
      [ Pulsing <$> arbitrary <*> arbitrary
      , Complete <$> arbitrary
      ]
  shrink = genericShrink

instance Crypto c => Arbitrary (RewardSnapShot c) where
  arbitrary =
    RewardSnapShot
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (FreeVars c) where
  arbitrary =
    FreeVars
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Shelley.Rules ----------------------------------------------------------
------------------------------------------------------------------------------------------

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ShelleyPPUPState era) where
  arbitrary = ShelleyPPUPState <$> arbitrary <*> arbitrary
  shrink = genericShrink
