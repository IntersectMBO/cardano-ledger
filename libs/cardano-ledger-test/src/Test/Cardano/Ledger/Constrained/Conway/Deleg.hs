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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

-- | Specify that some of the rewards in the RDPair's are zero.
--   without this in the DState, it is hard to generate the ConwayUnRegCert
--   certificate, since it requires a rewards balance of 0.
--   We also specify that both reward and deposit are greater than (Coin 0)
someZeros :: forall fn. IsConwayUniv fn => Specification fn RDPair
someZeros = constrained $ \ [var| someRdpair |] ->
  match someRdpair $ \ [var| rewardx |] [var|deposit|] ->
    [ satisfies rewardx (chooseSpec (1, constrained $ \ [var| x |] -> assert $ x ==. lit 0) (3, gtSpec 0))
    , satisfies deposit (geqSpec 0)
    ]

-- | Specification for the RewardDepositMap of the Umap in the DState
--   It must be witnessed, and conform to some properties relating to the
--   withdrawals map, which is part of the context, so passed as an arg.
rewDepMapSpec ::
  (Era era, IsConwayUniv fn) =>
  WitUniv era ->
  Map (RewardAccount (EraCrypto era)) Coin ->
  Specification fn (Map (Credential 'Staking (EraCrypto era)) RDPair)
rewDepMapSpec univ wdrl =
  let n = wvSize univ
      m = Map.size wdrl
      maxRewDepSize = fromIntegral (2 * n - (m + 2))
   in constrained $ \ [var|rdmap|] ->
        [ -- can't be bigger than the witness set (n keys + n scripts)
          -- must also have enough slack to accomodate the credentials in wdrl (m)
          assert $ sizeOf_ (dom_ rdmap) <=. lit maxRewDepSize -- If this is too large
        , assert $ subset_ (lit (wdrlCredentials wdrl)) (dom_ rdmap) -- it is hard to satisfy this
        , forAll' rdmap $ \ [var|cred|] [var| rdpair|] ->
            [ witness univ cred
            , satisfies rdpair someZeros
            ]
        ]

rewDepMapSpec2 ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era ->
  Map (RewardAccount (EraCrypto era)) Coin ->
  Specification fn (Map (Credential 'Staking (EraCrypto era)) RDPair)
rewDepMapSpec2 univ wdrl =
  let n = wvSize univ
      m = Map.size wdrl
      maxRewDepSize = fromIntegral (n - (m + 2)) -- (2 * n - (m + 2))
      withdrawalPairs :: [(Credential 'Staking (EraCrypto era), Word64)]
      withdrawalPairs = Map.toList (Map.mapKeys raCredential (Map.map coinToWord64 wdrl))
      withdrawalKeys :: Set (Credential 'Staking (EraCrypto era))
      withdrawalKeys = Map.keysSet (Map.mapKeys raCredential wdrl)
   in constrained $ \ [var|rdmap|] ->
        [ -- size of rdmap, can't be bigger than the witness set (n keys + n scripts)
          -- must also have enough slack to accomodate the credentials in wdrl (m)
          assert $ sizeOf_ (dom_ rdmap) <=. lit maxRewDepSize
        , assertExplain (pure "some rewards (not in withdrawals) are zero") $
            forAll rdmap $
              \ [var| keycoinpair |] -> match keycoinpair $ \cred [var| rdpair |] ->
                -- Apply this only to entries NOT IN the withdrawal set, since entries in the withdrawal set
                -- already force the reward in the RDPair to the withdrawal amount.
                [ witness univ cred
                , whenTrue (not_ (member_ cred (lit withdrawalKeys))) (satisfies rdpair someZeros)
                ]
        , forAll (lit withdrawalPairs) $ \ [var| pair |] ->
            match pair $ \ [var| wcred |] [var| coin |] ->
              [ assertExplain (pure "withdrawalKeys are a subset of the rdMap") $ member_ wcred (dom_ rdmap)
              , -- Force the reward in the RDPair to the withdrawal amount.
                onJust (lookup_ wcred rdmap) $ \ [var|rdpair|] ->
                  match rdpair $ \rew _deposit -> assert $ rew ==. coin
              ]
        ]

coinToWord64 :: Coin -> Word64
coinToWord64 (Coin n) = fromIntegral n

wdrlCredentials :: Map (RewardAccount c) Coin -> Set (Credential 'Staking c)
wdrlCredentials m = Set.map raCredential (Map.keysSet m)

keyHashWdrl :: Map (RewardAccount c) Coin -> Set (Credential 'Staking c)
keyHashWdrl m = Set.filter isKeyHash (wdrlCredentials m)
  where
    isKeyHash (KeyHashObj _) = True
    isKeyHash (ScriptHashObj _) = False

dStateSpec ::
  forall fn era.
  (IsConwayUniv fn, EraSpecDeleg era) =>
  WitUniv era ->
  Map (RewardAccount (EraCrypto era)) Coin ->
  Specification fn (DState era)
dStateSpec univ wdrls = constrained $ \ [var| dstate |] ->
  match dstate $ \ [var| uMap |] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    [ -- futureGenDelegs
      assert $ sizeOf_ futureGenDelegs ==. (if hasGenDelegs @era [] then 3 else 0)
    , -- genDelegs
      match genDelegs $ \gd ->
        [ witness univ (dom_ gd)
        , witness univ (rng_ gd)
        , assert $ sizeOf_ gd ==. (if hasGenDelegs @era [] then 3 else 0)
        ]
    , -- irewards
      match irewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
    , match uMap $ \ [var| rdMap |] [var| ptrMap |] [var| sPoolMap |] [var|dRepMap|] ->
        [ -- rdMap
          satisfies rdMap (rewDepMapSpec2 univ wdrls)
        , -- dRepMap
          dependsOn dRepMap rdMap
        , reify rdMap id $ \ [var|rdm|] ->
            [ witness univ (dom_ dRepMap)
            , assert $ subset_ (lit (keyHashWdrl wdrls)) (dom_ dRepMap)
            , witness univ (rng_ dRepMap)
            , assert $ subset_ (dom_ dRepMap) (dom_ rdm)
            ]
        , -- sPoolMap
          reify rdMap id $ \ [var|rdmp|] ->
            assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdmp
        , -- ptrMapo
          assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
        ]
    ]

conwayDelegCertSpec ::
  forall fn era.
  (EraPParams era, IsConwayUniv fn) =>
  ConwayDelegEnv era ->
  CertState era ->
  Specification fn ConwayDelegCert
conwayDelegCertSpec (ConwayDelegEnv pp pools) (CertState vs _ps ds) =
  let rewardMap = unUnify $ rewards ds
      dReps = vsDReps vs
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardMap of
          Just d | d > 0 -> SJust d
          _ -> SNothing
      delegateeInPools :: Term fn Delegatee -> Pred fn
      delegateeInPools delegatee =
        (caseOn delegatee)
          (branch $ \kh -> isInPools kh)
          (branch $ \drep -> isInDReps drep)
          (branch $ \kh drep -> [assert $ isInPools kh, assert $ isInDReps drep])
        where
          isInPools = (`member_` lit (Map.keysSet pools))
          drepsSet f drepsMap = Set.fromList [k' | k <- Map.keys drepsMap, Just k' <- [f k]]
          isInDReps :: Term fn DRep -> Pred fn
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
  WitUniv era ->
  ConwayDelegEnv era ->
  DState era ->
  Specification fn (ShelleyDelegCert (EraCrypto era))
shelleyDelegCertSpec univ (ConwayDelegEnv _pp pools) ds =
  let rewardMap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
   in constrained $ \dc ->
        (caseOn dc)
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency

          -- ShelleyRegCert !(StakeCredential c)
          ( branchW 2 $ \sc ->
              [witness univ sc, assert $ not_ (member_ sc (lit (Map.keysSet rewardMap)))]
          )
          -- ShelleyUnRegCert !(StakeCredential c)
          ( branchW 3 $ \sc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter zeroReward rewardMap)
              , assert $ elem_ sc $ lit (Map.keys delegMap)
              , witness univ sc
              ]
          )
          -- ShelleyDelegCert !(StakeCredential c) (KeyHash StakePool c)
          ( branchW 2 $ \sc kh ->
              [ dependsOn sc dc
              , dependsOn kh dc
              , assert . elem_ sc $ lit (Map.keys delegMap)
              , assert $ elem_ kh (lit (Map.keys pools))
              , witness univ sc
              , witness univ kh
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
