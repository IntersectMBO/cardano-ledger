{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.Conway.Deleg where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (ConwayDelegEnv (..))
import Cardano.Ledger.Conway.State hiding (balance)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.PoolParams (StakePoolParams (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.TxCert
import Constrained.API
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

coinToWord64 :: Coin -> Word64
coinToWord64 (Coin n) = fromIntegral n

wdrlCredentials :: Map RewardAccount Coin -> Set (Credential 'Staking)
wdrlCredentials m = Set.map raCredential (Map.keysSet m)

keyHashWdrl :: Map RewardAccount Coin -> Set (Credential 'Staking)
keyHashWdrl m = Set.filter isKeyHash (wdrlCredentials m)

isKeyHash :: Credential 'Staking -> Bool
isKeyHash (KeyHashObj _) = True
isKeyHash (ScriptHashObj _) = False

accountBalanceSpec :: (HasSpec a, Monoid a) => Term a -> Pred
accountBalanceSpec balance =
  satisfies
    balance
    ( chooseSpec
        (1, constrained $ \ [var| x |] -> assert $ x ==. lit mempty)
        (3, constrained (const True))
    )

dRepMembershipPred :: Map (Credential 'DRepRole) a -> Term DRep -> Pred
dRepMembershipPred dRepsMap dRep =
  assert $
    (caseOn dRep)
      (branchW 5 (`member_` lit (dRepsSet credKeyHash)))
      (branchW 5 (`member_` lit (dRepsSet credScriptHash)))
      (branchW 1 $ const True)
      (branchW 1 $ const True)
  where
    dRepsSet :: Ord a => (Credential 'DRepRole -> Maybe a) -> Set a
    dRepsSet f = Set.fromList [k' | k <- Map.keys dRepsMap, Just k' <- [f k]]

-- | The DState needs a witnessed set of delegations to be usefull. Use this Spec to obtain a random one
witnessedKeyHashPoolParamMapSpec ::
  Era era => WitUniv era -> Specification (Map (KeyHash StakePool) StakePoolParams)
witnessedKeyHashPoolParamMapSpec univ =
  constrained $ \keyPoolParamMap ->
    [witness univ (dom_ keyPoolParamMap), witness univ (rng_ keyPoolParamMap)]

conwayAccountsSpec ::
  Era era =>
  WitUniv era ->
  Term (Map (KeyHash 'StakePool) StakePoolParams) ->
  Specification (ConwayAccounts era)
conwayAccountsSpec univ poolreg = constrained $ \ [var|conwayAccounts|] ->
  match conwayAccounts $ \ [var|accountmap|] ->
    [ witness univ (dom_ accountmap)
    , forAll accountmap $ \ [var|pair|] ->
        match pair $ \ [var|stakecred|] [var|accountstate|] ->
          [ witness univ stakecred
          , witness univ accountstate
          , match accountstate $ \ [var|_rewardbal|] [var|_depositbal|] [var|mStakeDelegKeyhash|] [var|mDRep|] ->
              [ ( caseOn
                    (mStakeDelegKeyhash :: Term (StrictMaybe (KeyHash 'StakePool)))
                    (branchW 1 $ \_ -> True)
                    (branchW 3 $ \ [var|stakekeyhash|] -> mapMember_ stakekeyhash poolreg)
                )
              , (caseOn mDRep) -- This case only adds frequency info, the (witness univ accountstate) ensures witnessing
                  (branchW 1 $ \_ -> True)
                  ( branchW 3 $ \(drep :: Term DRep) ->
                      (caseOn drep)
                        (branchW 3 $ \_keyhash -> True)
                        (branchW 3 $ \_scripthash -> True)
                        (branchW 1 $ \_abstain -> True)
                        (branchW 1 $ \_noconfidence -> True)
                  )
              ]
          ]
    ]

stakePoolDelegationsSpec ::
  Era era =>
  WitUniv era ->
  Specification (Map (Credential 'Staking) (KeyHash 'StakePool))
stakePoolDelegationsSpec univ =
  constrained $ \ [var|stakePoolDelegations|] -> witness univ stakePoolDelegations

dRepDelegationsSpec ::
  Era era =>
  WitUniv era ->
  Specification (Map (Credential 'Staking) DRep)
dRepDelegationsSpec univ =
  constrained $ \ [var|dRepDelegations|] ->
    [ witness univ (dom_ dRepDelegations)
    , forAll (rng_ dRepDelegations) $ \ [var|dRepDelegation|] ->
        caseOn
          dRepDelegation
          (branchW 5 (witness univ))
          (branchW 3 (witness univ))
          (branchW 1 $ const True)
          (branchW 1 $ const True)
    ]

dStateSpec ::
  (Era era, HasSpec (Accounts era)) =>
  WitUniv era ->
  Map RewardAccount Coin ->
  Specification (DState era)
dStateSpec _univ _wdrls = constrained $ \ [var| dstate |] ->
  match dstate $ \_ [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    [ -- futureGenDelegs
      assert $ sizeOf_ futureGenDelegs ==. 0
    , -- genDelegs
      match genDelegs $ \gd ->
        [ assert $ sizeOf_ gd ==. 0
        ]
    , -- irewards
      match irewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
    ]

conwayDelegCertSpec ::
  forall era.
  (EraPParams era, ConwayEraCertState era) =>
  ConwayDelegEnv era ->
  CertState era ->
  Specification ConwayDelegCert
conwayDelegCertSpec (ConwayDelegEnv pp pools) certState =
  let ds = certState ^. certDStateL
      vs = certState ^. certVStateL
      accountsMap = ds ^. accountsL . accountsMapL
      dReps = vsDReps vs
      depositOf cred =
        case Map.lookup cred accountsMap of
          Just accountState -> SJust $ fromCompact (accountState ^. depositAccountStateL)
          Nothing -> SNothing
      delegateeInPools :: Term Delegatee -> Pred
      delegateeInPools delegatee =
        (caseOn delegatee)
          (branch $ \kh -> isInPools kh)
          (branch $ \dRep -> dRepMembershipPred dReps dRep)
          (branch $ \kh dRep -> [assert $ isInPools kh, dRepMembershipPred dReps dRep])
        where
          isInPools = (`member_` lit (Map.keysSet pools))
   in constrained $ \dc ->
        (caseOn dc)
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency

          -- ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branchW 2 $ \sc mc ->
              [ assert $ not_ (member_ sc (lit (Map.keysSet accountsMap)))
              , assert $ mc ==. lit (SJust (pp ^. ppKeyDepositL))
              ]
          )
          -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branchW 2 $ \sc mc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter isZeroAccountBalance accountsMap)
              , -- The `StrictMaybe` needs to be precisely what is in the delegation map
                reify sc depositOf (==. mc)
              ]
          )
          -- ConwayDelegCert !(StakeCredential c) !(Delegatee c)
          ( branchW 1 $ \sc delegatee ->
              [ assert . member_ sc $ lit (Map.keysSet accountsMap)
              , delegateeInPools delegatee
              ]
          )
          -- ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
          ( branchW 1 $ \sc delegatee c ->
              [ assert $ c ==. lit (pp ^. ppKeyDepositL)
              , assert $ not_ (member_ sc (lit (Map.keysSet accountsMap)))
              , delegateeInPools delegatee
              ]
          )

delegEnvSpec ::
  EraSpecPParams era =>
  Specification (ConwayDelegEnv era)
delegEnvSpec = constrained $ \env ->
  match env $ \pp _ ->
    pp `satisfies` pparamsSpec

-- ====================================
-- Pre-Conway Deleg Certs

shelleyDelegCertSpec ::
  forall era.
  (EraPParams era, EraAccounts era) =>
  WitUniv era ->
  ConwayDelegEnv era ->
  DState era ->
  Specification ShelleyDelegCert
shelleyDelegCertSpec univ (ConwayDelegEnv _pp pools) ds =
  let accountsMap = ds ^. accountsL . accountsMapL
   in constrained $ \dc ->
        (caseOn dc)
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency

          -- ShelleyRegCert !(StakeCredential c)
          ( branchW 2 $ \sc ->
              [witness univ sc, assert $ not_ (member_ sc (lit (Map.keysSet accountsMap)))]
          )
          -- ShelleyUnRegCert !(StakeCredential c)
          ( branchW 3 $ \sc ->
              [ -- You can only unregister credentials with 0 balance
                assert $ member_ sc $ lit (Map.keysSet $ Map.filter isZeroAccountBalance accountsMap)
              , witness univ sc
              ]
          )
          -- ShelleyDelegCert !(StakeCredential c) (KeyHash StakePool c)
          ( branchW 2 $ \sc kh ->
              [ dependsOn sc dc
              , dependsOn kh dc
              , assert $ member_ sc (lit (Map.keysSet accountsMap))
              , assert $ member_ kh (lit (Map.keysSet pools))
              , witness univ sc
              , witness univ kh
              ]
          )

isZeroAccountBalance :: EraAccounts era => AccountState era -> Bool
isZeroAccountBalance accountState = accountState ^. balanceAccountStateL == mempty

-- =============================================

class (Era era, EraPParams era, HasSpec (Accounts era), IsNormalType (Accounts era)) => EraSpecDeleg era where
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
