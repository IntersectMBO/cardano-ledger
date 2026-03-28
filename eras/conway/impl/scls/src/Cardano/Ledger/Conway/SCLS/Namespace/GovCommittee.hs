{-# LANGUAGE RecordWildCards #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovCommittee (
  module Cardano.Ledger.SCLS.Namespace.GovCommittee.V0,
  mkCanonicalCommitteeState,
  fromCanonicalCommitteeState,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.SCLS.Common (
  fromCanonicalCredential,
  mkCanonicalCredential,
 )
import Cardano.Ledger.SCLS.Namespace.GovCommittee.V0
import Cardano.Ledger.State (CommitteeState (..))
import qualified Data.Map.Strict as Map

mkCanonicalCommitteeState :: CommitteeState ConwayEra -> CanonicalCommitteeState
mkCanonicalCommitteeState CommitteeState {..} =
  CanonicalCommitteeState
    ( Map.fromList
        [ (mkCanonicalCredential k, mkCanonicalCommitteeAuthorization v)
        | (k, v) <- Map.toList csCommitteeCreds
        ]
    )

fromCanonicalCommitteeState :: CanonicalCommitteeState -> CommitteeState ConwayEra
fromCanonicalCommitteeState CanonicalCommitteeState {..} =
  CommitteeState
    ( Map.fromList
        [ (fromCanonicalCredential k, fromCanonicalCommitteeAuthorization v)
        | (k, v) <- Map.toList csCommitteeCreds
        ]
    )
