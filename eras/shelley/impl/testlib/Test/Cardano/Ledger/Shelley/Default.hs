module Test.Cardano.Ledger.Shelley.Default () where

instance Default (ShelleyInstantStake era) where
  def = mempty

instance Default (ShelleyCertState era) where
  def = emptyShelleyCertState

instance Default NonMyopic where
  def = NonMyopic Map.empty (Coin 0)

instance EraPParams era => Default (ShelleyGovState era) where
  def = emptyShelleyGovState

instance Default RewardProvenancePool where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

instance Default RewardProvenance where
  def = emptyRewardProvenance

instance Default InstantaneousRewards where
  def = emptyInstantaneousRewards

instance Default (DState era) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState era) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

instance (EraGov era, EraStake era) => Default (UTxOState era) where
  def = UTxOState mempty mempty mempty def mempty mempty

instance
  Default (LedgerState era) =>
  Default (EpochState era)
  where
  def = EpochState def def def def

instance (Default (UTxOState era), Default (CertState era)) => Default (LedgerState era) where
  def = LedgerState def def
