{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Conway.Gen.DRepPulsingState (
  genPulsingSnapshot,
) where

import Cardano.Ledger.Conway.Governance (PulsingSnapshot (..))
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Common (perturbList, perturbMap)
import Test.ImpSpec (HasStatefulGen)

-- | Generate a perturbation of the given 'PulsingSnapshot'. Currently
-- drop-only: each of the four fields has a random subset of its entries
-- removed (preserving order in 'psProposals').
genPulsingSnapshot ::
  HasStatefulGen g m =>
  PulsingSnapshot era ->
  m (PulsingSnapshot era)
genPulsingSnapshot PulsingSnapshot {..} =
  (PulsingSnapshot . StrictSeq.fromList <$> perturbList Nothing psProposals)
    <*> perturbMap Nothing psDRepDistr
    <*> perturbMap Nothing psDRepState
    <*> perturbMap Nothing psPoolDistr
