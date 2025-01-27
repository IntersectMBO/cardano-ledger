{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Experiment.Certs where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHash, credScriptHash)
import Constrained.Experiment.API
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Sequence (Seq, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Cert
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Deleg (
  hasGenDelegs,
  keyHashWdrl,
  rewDepMapSpec2,
 )
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Instances
import Test.Cardano.Ledger.Constrained.Conway.Experiment.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Experiment.ParametricSpec (EraSpecTxOut (..))
import Test.Cardano.Ledger.Constrained.Conway.Experiment.WitnessUniverse

-- =======================================================

setMapMaybe :: Ord a => (t -> Maybe a) -> Set t -> Set a
setMapMaybe f set = Set.foldr' (\x s -> maybe s (`Set.insert` s) $ f x) mempty set

-- The current spec is written to specify the phase when Voting goes into effect (the Post BootStrap phase)
-- The implementation is written to implement the phase before Voting goes into effect (the BootStrap phase)
-- This affects the Certs rule because in the Post Bootstrap Phase, the spec tests that the (Credential 'Staking c)
-- of every withdrawal, is delegated to some DRep, and that every Withdrawal is consistent with some Rewards entry.
-- This is tested in the Spec, but it is not tested in implementation.
-- A hardfork, sometime in the future will turn this test on.
-- So to satisfy both we add this more refined DState spec, that make sure these post bootstrap tests are always True.
-- The implementation does not test these, so the extra refinement has no effect here, the Spec will test them so refinement does matter there.
-- Note that only the keyhash credentials need be delegated to a DRep.
bootstrapDStateSpec ::
  forall era.
  EraSpecTxOut era =>
  WitUniv era ->
  -- Set of credentials, each uniquely identifying a DRep,
  -- Every delegation of a stake credential to a DRep should be in this set.
  Set (Credential 'DRepRole) ->
  Map RewardAccount Coin ->
  Specification (DState era)
bootstrapDStateSpec univ delegatees withdrawals =
  constrained $ \ [var| dstate |] ->
    match dstate $ \ [var| uMap |] futureGenDelegs genDelegs [var|irewards|] ->
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
      , -- uMap
        match [var| uMap |] $ \ [var| rdMap |] [var| ptrMap |] [var| sPoolMap |] [var|dRepMap|] ->
          [ -- rdMap
            satisfies rdMap (rewDepMapSpec2 univ withdrawals)
          , -- dRepMap
            dependsOn dRepMap rdMap
          , reify rdMap id $ \ [var|rdm|] ->
              [ witness univ (dom_ dRepMap)
              , assert $ subset_ (lit (keyHashWdrl withdrawals)) (dom_ dRepMap)
              , witness univ (rng_ dRepMap)
              , assert $ subset_ (dom_ dRepMap) (dom_ rdm)
              , -- All DReps delegated to are known delegatees
                forAll' dRepMap $ \_ dRep ->
                  [ (caseOn dRep)
                      (branch $ \kh -> assert (kh `member_` lit (setMapMaybe credKeyHash delegatees)))
                      (branch $ \sh -> assert (sh `member_` lit (setMapMaybe credScriptHash delegatees)))
                      (branch $ \_ -> assert True)
                      (branch $ \_ -> assert True)
                  ]
              ]
          , -- sPoolMap
            dependsOn sPoolMap rdMap
          , reify rdMap id $ \ [var|rdmp|] ->
              assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdmp
          , -- ptrMap
            assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
          ]
      ]

txZero :: EraTx era => Tx era
txZero = mkBasicTx mkBasicTxBody

certsEnvSpec ::
  (EraSpecPParams era, HasSpec (Tx era)) =>
  Specification (CertsEnv era)
certsEnvSpec = constrained $ \ce ->
  match ce $ \tx pp _currepoch _currcommittee commproposals ->
    [ satisfies pp pparamsSpec
    , assert $ tx ==. lit txZero
    , genHint 3 commproposals
    ]

-- | Project a CertEnv out of a CertsEnv (i.e drop the Tx)
projectEnv :: CertsEnv era -> CertEnv era
projectEnv x =
  CertEnv
    { cePParams = certsPParams x
    , ceCurrentEpoch = certsCurrentEpoch x
    , ceCurrentCommittee = certsCurrentCommittee x
    , ceCommitteeProposals = certsCommitteeProposals x
    }

txCertsSpec ::
  forall era.
  EraSpecCert era =>
  WitUniv era ->
  CertsEnv era ->
  CertState era ->
  Specification (Seq (TxCert era))
txCertsSpec univ env state =
  constrained $ \seqs ->
    exists
      (\eval -> pure $ toList (eval seqs))
      (\list -> satisfies (pair_ list seqs) (listSeqCertPairSpec @era univ (projectEnv @era env) state))

noSameKeys :: forall era. EraSpecCert era => [TxCert era] -> [TxCert era]
noSameKeys [] = []
noSameKeys (x : xs) = x : noSameKeys @era (filter (\y -> txCertKey @era x /= txCertKey @era y) xs)

-- | Specify a pair of List and Seq, where they have essentially the same elements
--   EXCEPT, the Seq has duplicate keys filtered out.
listSeqCertPairSpec ::
  forall era.
  EraSpecCert era =>
  WitUniv era ->
  CertEnv era ->
  CertState era ->
  Specification ([TxCert era], Seq (TxCert era))
listSeqCertPairSpec univ env state =
  constrained' $ \list seqs ->
    [ assert $ sizeOf_ list <=. 5
    , forAll list $ \x -> satisfies x (txCertSpec @era univ env state)
    , reify list (fromList . noSameKeys @era) (\x -> seqs ==. x)
    ]
