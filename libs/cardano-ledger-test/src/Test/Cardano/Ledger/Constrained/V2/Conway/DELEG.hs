{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.V2.Conway.DELEG where

import Cardano.Ledger.Api
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UMap (RDPair (..), fromCompact, unUnify)
import qualified Data.Map as Map
import Lens.Micro

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway

dStateSpec ::
  IsConwayUniv fn =>
  Spec fn (DState (ConwayEra StandardCrypto))
dStateSpec = constrained $ \ds ->
  match ds $ \rewardMap _futureGenDelegs _genDelegs _rewards ->
    match rewardMap $ \rdMap ptrMap sPoolMap _dRepMap ->
      [ assertExplain ["dom sPoolMap is a subset of dom rdMap"] $ dom_ sPoolMap `subset_` dom_ rdMap
      , assertExplain ["dom ptrMap is empty"] $ dom_ ptrMap ==. mempty
      ]

delegCertSpec ::
  IsConwayUniv fn =>
  PParams (ConwayEra StandardCrypto) ->
  DState (ConwayEra StandardCrypto) ->
  Spec fn (ConwayDelegCert StandardCrypto)
delegCertSpec pp ds =
  let rewardMap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardMap of
          Just d | d > 0 -> SJust d
          _              -> SNothing
   in constrained $ \dc ->
        (caseOn dc)
          -- ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
          (branch $ \_ mc -> mc ==. lit (SJust (pp ^. ppKeyDepositL)))
          -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branch $ \sc mc ->
              [ assert $ elem_ sc $ lit (Map.keys $ Map.filter ((== 0) . fromCompact . rdReward) rewardMap)
              , assert $ elem_ sc $ lit (Map.keys delegMap)
              , reify sc depositOf (==. mc)
              ]
          )
          -- ConwayDelegCert !(StakeCredential c) !(Delegatee c)
          (branch $ \sc _ -> member_ sc $ lit (Map.keysSet delegMap))
          -- ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
          ( branch $ \_ _ c ->
              c ==. lit (pp ^. ppKeyDepositL)
          )
