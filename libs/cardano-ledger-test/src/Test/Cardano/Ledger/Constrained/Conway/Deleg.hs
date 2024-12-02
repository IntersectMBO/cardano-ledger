{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.Conway.Deleg where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (ConwayDelegEnv (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core (Era (..), EraPParams (..), ppKeyDepositL)
import Cardano.Ledger.Credential (credKeyHash, credScriptHash)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UMap (RDPair (..), fromCompact, unUnify)
import Constrained
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

-- | Specify that some of the rewards in the RDPair's are zero.
--   without this in the DState, it is hard to generate the ConwayUnRegCert
--   certificate, since it requires a rewards balance of 0.
--   We also specify that both reward and deposit are greater thn (Coin 0)
someZeros :: forall fn. IsConwayUniv fn => Specification fn RDPair
someZeros = constrained $ \ [var| someRdpair |] ->
  match someRdpair $ \ [var| reward |] [var|deposit|] ->
    [ satisfies reward (chooseSpec (1, constrained $ \ [var| x |] -> assert $ x ==. lit 0) (3, gtSpec 0))
    , satisfies deposit (geqSpec 0)
    ]

dStateSpec ::
  forall fn era. (IsConwayUniv fn, EraSpecDeleg era) => Specification fn (DState era)
dStateSpec = constrained $ \ [var| dstate |] ->
  match dstate $ \ [var| rewardMap |] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    match rewardMap $ \ [var| rdMap |] [var| ptrMap |] [var| sPoolMap |] _dRepMap ->
      [ assert $ sizeOf_ futureGenDelegs ==. (if hasGenDelegs @era [] then 3 else 0)
      , match genDelegs $ \gd -> assert $ sizeOf_ gd ==. (if hasGenDelegs @era [] then 3 else 0)
      , match irewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
      , assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
      , assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
      , assertExplain (pure "some rewards are zero") $
          forAll rdMap $
            \p -> match p $ \_cred rdpair -> satisfies rdpair someZeros
      ]

conwayDelegCertSpec ::
  forall fn era.
  (EraPParams era, IsConwayUniv fn) =>
  ConwayDelegEnv era ->
  CertState era ->
  Specification fn (ConwayDelegCert)
conwayDelegCertSpec (ConwayDelegEnv pp pools) (CertState vs _ps ds) =
  let rewardMap = unUnify $ rewards ds
      dReps = vsDReps vs
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardMap of
          Just d | d > 0 -> SJust d
          _ -> SNothing
      delegateeInPools :: Term fn (Delegatee) -> Pred fn
      delegateeInPools delegatee =
        (caseOn delegatee)
          (branch $ \kh -> isInPools kh)
          (branch $ \drep -> isInDReps drep)
          (branch $ \kh drep -> [assert $ isInPools kh, assert $ isInDReps drep])
        where
          isInPools = (`member_` lit (Map.keysSet pools))
          drepsSet f drepsMap = Set.fromList [k' | k <- Map.keys drepsMap, Just k' <- [f k]]
          isInDReps :: Term fn (DRep) -> Pred fn
          isInDReps drep =
            (caseOn drep)
              ( branch $ \drepKeyHash ->
                  drepKeyHash `member_` lit (drepsSet credKeyHash dReps)
              )
              ( branch $ \drepScriptHash ->
                  drepScriptHash `member_` lit (drepsSet credScriptHash dReps)
              )
              (branch $ const True)
              (branch $ const True)
   in constrained $ \dc ->
        (caseOn dc)
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency

          -- ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branchW 2 $ \sc mc ->
              [ assert $ not_ (member_ sc (lit (Map.keysSet rewardMap)))
              , assert $ mc ==. lit (SJust (pp ^. ppKeyDepositL))
              ]
          )
          -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branchW 2 $ \sc mc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter zeroReward rewardMap)
              , assert $ elem_ sc $ lit (Map.keys delegMap)
              , -- The `StrictMaybe` needs to be precisely what is in the delegation map
                reify sc depositOf (==. mc)
              ]
          )
          -- ConwayDelegCert !(StakeCredential c) !(Delegatee c)
          ( branchW 1 $ \sc delegatee ->
              [ assert . member_ sc $ lit (Map.keysSet delegMap)
              , delegateeInPools delegatee
              ]
          )
          -- ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
          ( branchW 1 $ \sc delegatee c ->
              [ assert $ c ==. lit (pp ^. ppKeyDepositL)
              , assert $ not_ (member_ sc (lit (Map.keysSet rewardMap)))
              , delegateeInPools delegatee
              ]
          )

delegEnvSpec ::
  (EraSpecPParams era, IsConwayUniv fn) =>
  Specification fn (ConwayDelegEnv era)
delegEnvSpec = constrained $ \env ->
  match env $ \pp _ ->
    pp `satisfies` pparamsSpec

-- ====================================
-- Pre-Conway Deleg Certs

shelleyDelegCertSpec ::
  forall fn era.
  (EraPParams era, IsConwayUniv fn) =>
  ConwayDelegEnv era ->
  DState era ->
  Specification fn (ShelleyDelegCert)
shelleyDelegCertSpec (ConwayDelegEnv _pp pools) ds =
  let rewardMap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
   in constrained $ \dc ->
        (caseOn dc)
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency

          -- ShelleyRegCert !(StakeCredential c)
          ( branchW 2 $ \sc ->
              [ assert $ not_ (member_ sc (lit (Map.keysSet rewardMap)))
              ]
          )
          -- ShelleyUnRegCert !(StakeCredential c)
          ( branchW 3 $ \sc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter zeroReward rewardMap)
              , assert $ elem_ sc $ lit (Map.keys delegMap)
              ]
          )
          -- ShelleyDelegCert !(StakeCredential c) (KeyHash StakePool c)
          ( branchW 2 $ \sc kh ->
              [ dependsOn sc dc
              , dependsOn kh dc
              , assert . elem_ sc $ lit (Map.keys delegMap)
              , assert $ elem_ kh (lit (Map.keys pools))
              ]
          )

-- =============================================

class (Era era, EraPParams era) => EraSpecDeleg era where
  hasGenDelegs :: proxy era -> Bool

instance EraSpecDeleg ShelleyEra where
  hasGenDelegs _proxy = True

instance EraSpecDeleg AllegraEra where
  hasGenDelegs _proxy = True

instance EraSpecDeleg MaryEra where
  hasGenDelegs _proxy = True

instance EraSpecDeleg AlonzoEra where
  hasGenDelegs _proxy = True

instance EraSpecDeleg BabbageEra where
  hasGenDelegs _proxy = True

instance EraSpecDeleg ConwayEra where
  hasGenDelegs _proxy = False
