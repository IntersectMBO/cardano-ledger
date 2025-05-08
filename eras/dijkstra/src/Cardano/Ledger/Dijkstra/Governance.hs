{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Governance () where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  EraGov (..),
  cgsCommitteeL,
  cgsConstitutionL,
  cgsCurPParamsL,
  cgsDRepPulsingStateL,
  cgsFuturePParamsL,
  cgsPrevPParamsL,
  cgsProposalsL,
  gasDeposit,
  proposalsActions,
 )
import Cardano.Ledger.Conway.State (Obligations (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.State.Stake ()
import Data.Foldable (Foldable (..))
import Lens.Micro ((^.))

instance EraGov DijkstraEra where
  type GovState DijkstraEra = ConwayGovState DijkstraEra

  curPParamsGovStateL = cgsCurPParamsL

  prevPParamsGovStateL = cgsPrevPParamsL

  futurePParamsGovStateL = cgsFuturePParamsL

  obligationGovState st =
    Obligations
      { oblProposal = foldMap' gasDeposit $ proposalsActions (st ^. cgsProposalsL)
      , oblDRep = Coin 0
      , oblStake = Coin 0
      , oblPool = Coin 0
      }

instance ConwayEraGov DijkstraEra where
  constitutionGovStateL = cgsConstitutionL
  proposalsGovStateL = cgsProposalsL
  drepPulsingStateGovStateL = cgsDRepPulsingStateL
  committeeGovStateL = cgsCommitteeL
