{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Api.State.Query.PParams (
  queryCurrentPParams,
  queryFuturePParams,
) where

import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, epochStateGovStateL, nesEpochStateL)
import Cardano.Ledger.State (
  EraGov (curPParamsGovStateL, futurePParamsGovStateL),
  FuturePParams (..),
 )
import Lens.Micro ((^.))

-- | This is a simple lookup into the state for the values of current protocol
-- parameters. These values can change on the epoch boundary. Use 'queryFuturePParams' to
-- see if we are aware of any upcoming changes.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:418
--   answerPureBlockQuery case for GetCurrentPParams
queryCurrentPParams :: EraGov era => NewEpochState era -> PParams era
queryCurrentPParams nes = nes ^. nesEpochStateL . epochStateGovStateL . curPParamsGovStateL

-- | This query will return values for protocol parameters that are likely to be adopted
-- at the next epoch boundary. It is only when we passed 2 stability windows before the
-- end of the epoch that users can rely on this query to produce stable results.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:483
--   answerPureBlockQuery case for GetFuturePParams
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:395
--   queryFuturePParams cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1673
--   CLI invocation
queryFuturePParams :: EraGov era => NewEpochState era -> Maybe (PParams era)
queryFuturePParams nes =
  case nes ^. nesEpochStateL . epochStateGovStateL . futurePParamsGovStateL of
    NoPParamsUpdate -> Nothing
    PotentialPParamsUpdate mpp -> mpp
    DefinitePParamsUpdate pp -> Just pp
