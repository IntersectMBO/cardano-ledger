{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.V2.Conway.DelegCert where

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
   in constrained $ \dc ->
        (caseOn dc)
          -- ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
          (branch $ \_ mc -> mc ==. lit (SJust (pp ^. ppKeyDepositL)))
          -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branch $ \sc mc ->
              onJust' mc $ \c ->
                elem_ (pair_ sc c) $ lit [(k, r) | (k, RDPair (fromCompact -> r) _) <- Map.toList rewardMap]
          )
          -- ConwayDelegCert !(StakeCredential c) !(Delegatee c)
          (branch $ \sc _ -> member_ sc $ lit (Map.keysSet delegMap))
          -- ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
          ( branch $ \_ _ c ->
              c ==. lit (pp ^. ppKeyDepositL)
          )
