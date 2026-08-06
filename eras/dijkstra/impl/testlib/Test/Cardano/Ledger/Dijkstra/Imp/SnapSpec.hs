{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SnapSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "SNAP" $ do
  let getSpoVotingStake :: KeyHash StakePool -> ImpTestM era Coin
      getSpoVotingStake pool = do
        poolDistr <- psPoolDistr . fst . finishDRepPulser <$> getsNES (nesEsL . epochStateDRepPulsingStateL)
        pure $ fromCompact $ poolDistr Map.! pool
      getDRepVotingStake :: Credential DRepRole -> ImpTestM era Coin
      getDRepVotingStake drep = do
        drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
        pure $ fromCompact $ drepDistr Map.! DRepCredential drep
  it "SPO voting stake equals DRep voting stake after a refunded deposit" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 1
        & ppGovActionDepositL .~ Coin 1_000_000

    (drep, cred, _) <- setupSingleDRep 500_000_000
    pool <- freshKeyHash
    registerPool pool
    delegateStake cred pool
    returnAddr <- getAccountAddressFor cred

    govActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
    expectPresentGovActionId govActionId
    passNEpochs 3
    expectMissingGovActionId govActionId

    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake pool
    -- Conway's reproduction (Conway.Imp.SnapSpec.conwayOnlySpec) asserts the SPO trails the DRep by
    -- the refunded deposit: (drepVotingStake <-> spoVotingStake) `shouldBe` govActionDeposit.
    -- The Dijkstra SNAP fix removes that one-epoch lag, so here the two stakes are equal instead.
    impAnn "SPO voting stake reflects the refund immediately, like the DRep stake" $
      spoVotingStake `shouldBe` drepVotingStake
