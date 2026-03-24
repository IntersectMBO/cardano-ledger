{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.Imp.PoolSpec (babbageEraSpecificSpec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Rewards
import Cardano.Ledger.Shelley.Rules
import Data.Coerce
import Data.Map ((!))
import qualified Data.Set as Set
import Data.Typeable (cast)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Imp.Common

babbageEraSpecificSpec ::
  forall era.
  ( BabbageEraImp era
  , ShelleyEraAccounts era
  , Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  it "Pool to pool member rewards" $ do
    -- This test attempts to reproduce the issue that appeared with the release of
    -- `cardano-db-sync-10.6.1` (using `cardano-ledger-shelley-1.17.0.0`),
    -- where all of a sudden some rewards gone missing.
    -- Pools didn't receive member rewards when their reward accounts were delegated
    -- to other pools. This was only observed pre-Conway, see the `simpleRewards` test here:
    -- https://github.com/IntersectMBO/cardano-db-sync/blob/b8748fbbcb8c2d7e7a69e771914cc077bcdb3fa6/cardano-chain-gen/test/Test/Cardano/Db/Mock/Unit/Babbage/Reward.hs
    -- Also consult the genesis file which we try to immitate with the setup below:
    -- https://github.com/IntersectMBO/cardano-db-sync/blob/b8748fbbcb8c2d7e7a69e771914cc077bcdb3fa6/cardano-chain-gen/test/testfiles/config/genesis.json

    pools@[p1, p2, p3] <- replicateM 3 freshKeyHash
    poolSCreds@[s3, s7, _s8] <- replicateM 3 (KeyHashObj <$> freshKeyHash)
    screds@[s2, s4, s5] <- replicateM 3 (KeyHashObj <$> freshKeyHash)
    pcreds <- replicateM 6 (freshKeyHash @Payment)
    let addrs = zipWith mkAddr pcreds $ poolSCreds <> screds
        registerPoolWithPledge pk rewAcc = do
          pps <- freshPoolParams pk rewAcc
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps {sppPledge = Coin 0}]

    rewAccs <- mapM registerStakeCredential poolSCreds >>= \ras -> ras <$ withTxsInBlock_ (pure ras)
    withTxsInBlock_ $ mapM_ registerStakeCredential screds
    withTxsInBlock_ $
      mapM_ (uncurry registerPoolWithPledge) $
        zip pools rewAccs
    withTxsInBlock_ $
      delegateStake s4 (coerce p3)
        >> delegateStake s5 (coerce p1)
        >> delegateStake s7 (coerce p3)
        >> delegateStake s3 (coerce p1)
        >> delegateStake s2 (coerce p2)

    -- Some transactions in order to fill up the fees pot and to make sure
    -- that our pools produce blocks so they can hand out rewards.
    mapM_
      (\p -> withIssuerAndTxsInBlock_ (coerce p) $ mapM_ (`sendCoinTo_` Coin 300_000_000) addrs)
      pools

    passNEpochs 3

    -- Some more transactions in order to fill up the fees pot and to make sure
    -- that our pools produce blocks so they can hand out rewards.
    mapM_
      (\p -> withIssuerAndTxsInBlock_ (coerce p) $ mapM_ (`sendCoinTo_` Coin 30_000_000) addrs)
      pools

    -- We want to make sure that `s7`, the reward account associated with `p2`, receives
    -- member rewards (because they delegated to `p3`, which is producing blocks).
    let
      isMemberRewardEvent (SomeSTSEvent ev)
        | Just (TickNewEpochEvent (TotalRewardEvent _ m) :: ShelleyTickEvent era) <- cast ev =
            Set.size (Set.filter ((== MemberReward) . rewardType) (m ! s7)) > 0
      isMemberRewardEvent _ = False

    passEpoch
    finalEvs <- impEventsFrom passEpoch
    let res = filter isMemberRewardEvent finalEvs

    -- This assertion should fail if pool-to-pool delegation
    -- fails to yield member rewards. However, that is not the
    -- case here, so ultimately I could not reproduce the behaviour
    -- that was observed in `cardano-db-syn-10.6.1`.
    length res `shouldBe` 1
