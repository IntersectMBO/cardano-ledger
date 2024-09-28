{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.Conway.Deleg where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UMap (RDPair (..), fromCompact, unUnify)
import qualified Data.Map as Map
import Lens.Micro

import Constrained

import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Rules (ConwayDelegEnv (..))
import Cardano.Ledger.Core (ppKeyDepositL)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

-- | Specify that some of the rewards in the RDPair's are zero.
--   without this in the DState, it is hard to generate the ConwayUnRegCert
--   certificate, since it requires a rewards balance of 0.
someZeros :: forall fn. IsConwayUniv fn => Specification fn RDPair
someZeros = constrained $ \ [var| someRdpair |] ->
  match someRdpair $ \ [var| reward |] _deposit ->
    satisfies reward (chooseSpec (1, constrained $ \ [var| x |] -> assert $ x ==. lit 0) (3, TrueSpec))

dStateSpec ::
  forall fn.
  IsConwayUniv fn =>
  Specification fn (DState (ConwayEra StandardCrypto))
dStateSpec = constrained $ \ [var| dstate |] ->
  match dstate $ \ [var| rewardMap |] _futureGenDelegs _genDelegs _rewards ->
    match rewardMap $ \ [var| rdMap |] [var| ptrMap |] [var| sPoolMap |] _dRepMap ->
      [ assert $ sizeOf_ _futureGenDelegs ==. 0
      , match _genDelegs $ \gd -> assert $ sizeOf_ gd ==. 0
      , match _rewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
      , assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
      , assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
      , assertExplain (pure "some rewards are zero") $
          forAll rdMap $
            \p -> match p $ \_cred rdpair -> satisfies rdpair someZeros
      ]

delegCertSpec ::
  forall fn.
  IsConwayUniv fn =>
  ConwayDelegEnv (ConwayEra StandardCrypto) ->
  CertState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayDelegCert StandardCrypto)
delegCertSpec (ConwayDelegEnv pp pools) certState =
  let ds = certDState certState
      rewardMap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardMap of
          Just d | d > 0 -> SJust d
          _ -> SNothing
      delegateeInPools :: Term fn (Delegatee StandardCrypto) -> Pred fn
      delegateeInPools delegatee =
        (caseOn delegatee)
          (branch $ \kh -> isInPools kh)
          (branch $ \_ -> True)
          (branch $ \kh _ -> isInPools kh)
        where
          isInPools = (`member_` lit (Map.keysSet pools))
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
  IsConwayUniv fn =>
  Specification fn (ConwayDelegEnv Conway)
delegEnvSpec = constrained $ \env ->
  match env $ \pp _ ->
    pp `satisfies` pparamsSpec
