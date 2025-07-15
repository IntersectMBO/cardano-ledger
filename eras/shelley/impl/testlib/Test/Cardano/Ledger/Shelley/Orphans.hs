module Test.Cardano.Ledger.Shelley.Orphans () where

instance EraPParams era => Default (ShelleyGovState era) where
  def = emptyShelleyGovState

instance Default (ShelleyInstantStake era) where
  def = mempty

instance Default (ShelleyCertState era) where
  def = ShelleyCertState def def

instance Default NonMyopic where
  def = NonMyopic Map.empty (Coin 0)

instance Default RewardProvenancePool where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

instance Default RewardProvenance where
  def =
    RewardProvenance
      0
      (BlocksMade def)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      0
      0
      0
      0
      (Coin 0)
      (Coin 0)
      (Coin 0)
      def
      def

instance (EraGov era, EraStake era) => Default (UTxOState era) where
  def = UTxOState mempty mempty mempty def mempty mempty

instance
  Default (LedgerState era) =>
  Default (EpochState era)
  where
  def = EpochState def def def def

instance (Default (UTxOState era), Default (CertState era)) => Default (LedgerState era) where
  def = LedgerState def def
