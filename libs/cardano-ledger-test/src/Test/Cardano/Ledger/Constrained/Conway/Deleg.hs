{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.Conway.Deleg where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Rules (ConwayDelegEnv (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UMap (RDPair (..), fromCompact, rewardMap, unUnify)
import Constrained
import qualified Data.Map as Map
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (
  DeltaExecEnv (..),
  agdaDepositFromDstate,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (prettyA))
import Test.QuickCheck (Gen, generate)

-- | Specify that some of the rewards in the RDPair's are zero.
--   without this in the DState, it is hard to generate the ConwayUnRegCert
--   certificate, since it requires a rewards balance of 0.
someZeros :: forall fn. IsConwayUniv fn => Specification fn RDPair
someZeros = constrained $ \rdpair ->
  match rdpair $ \reward _deposit ->
    satisfies reward (chooseSpec (1, constrained $ \x -> assert $ x ==. lit 0) (3, TrueSpec))

dStateSpec ::
  forall fn.
  IsConwayUniv fn =>
  Specification fn (DState (ConwayEra StandardCrypto))
dStateSpec = constrained $ \ds ->
  match ds $ \rewardmap _futureGenDelegs _genDelegs _rewards ->
    match rewardmap $ \rdMap ptrMap sPoolMap _dRepMap ->
      [ assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
      , assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
      , assertExplain (pure "some rewards are zero") $
          forAll rdMap $
            \p -> match p $ \_cred rdpair -> satisfies rdpair someZeros
      ]

conwayDelegCertSpec ::
  forall fn.
  IsConwayUniv fn =>
  ConwayDelegEnv (ConwayEra StandardCrypto) ->
  DState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayDelegCert StandardCrypto)
conwayDelegCertSpec (ConwayDelegEnv pp pools) ds =
  let rewardmap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardmap of
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
          (branchW 2 $ \_ mc -> mc ==. lit (SJust (pp ^. ppKeyDepositL)))
          -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branchW 2 $ \sc mc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter zeroReward rewardmap)
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
              , assert $ not_ (member_ sc (lit (Map.keysSet rewardmap)))
              , delegateeInPools delegatee
              ]
          )

conwayDelegEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayDelegEnv Conway)
conwayDelegEnvSpec = constrained $ \env ->
  match env $ \pp _ ->
    pp `satisfies` pparamsSpec

-- ===============================================================

delegExecEnvSpec ::
  forall fn.
  IsConwayUniv fn =>
  (ConwayDelegEnv Conway, DState Conway) ->
  Specification fn (DeltaExecEnv (ConwayDelegEnv Conway) Conway)
delegExecEnvSpec (denv, dstate) = constrained $ \ [var| deEnv |] ->
  match deEnv $ \ [var|env|] [var|deposits|] [var|withdrawal|] [var|votes|] ->
    [ assert $ env ==. lit denv
    , match votes $ \m -> sizeOf_ m ==. 0
    , assert $ sizeOf_ withdrawal ==. 0 -- DELEG rule ignores withdrawals
    , assert $ deposits ==. lit (agdaDepositFromDstate dstate)
    -- , assert $ forAll withdrawal (\p -> elem_ p (lit (possibleWithdrawal dstate)))
    ]

genDELEGDeltaEnv :: Gen (ConwayDelegEnv Conway, DState Conway)
genDELEGDeltaEnv = do
  denv <- genFromSpec @ConwayFn conwayDelegEnvSpec
  dstate <- genFromSpec @ConwayFn dStateSpec
  pure (denv, dstate)

main :: IO ()
main = do
  (a, b) <- generate $ genDELEGDeltaEnv
  putStrLn ("\nENV\n" ++ show (prettyA a))
  putStrLn ("\nSTATE\n" ++ show (prettyA b))

possibleWithdrawal :: DState era -> [((Network, Credential 'Staking (EraCrypto era)), Coin)]
possibleWithdrawal dstate =
  filter (\(_, Coin n) -> n /= 0) $
    Map.toList $
      Map.mapKeys (\x -> (Testnet, x)) (rewardMap (dsUnified dstate))
