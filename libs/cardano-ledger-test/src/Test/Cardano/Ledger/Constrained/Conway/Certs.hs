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
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHash, credScriptHash)
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained
import Constrained.Base (Pred (..))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Conway.Cert
import Test.Cardano.Ledger.Constrained.Conway.Deleg (someZeros)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

-- =======================================================

-- The current spec is written to specify the phase when Voting goes into effect (the Post BootStrap phase)
-- The implementation is written to implement the phase before Voting goes into effect (the BootStrap phase)
-- This affects the Certs rule beacuse in the Post Bootstrap Phase, the spec tests that the (Credential 'Staking c)
-- of every withdrawal, is delegated to some DRep, and that every Withdrawal is consistent with some Rewards entry.
-- This is tested in the Spec, but it is not tested in implementation.
-- A hardfork, sometime in the future will turn this test on.
-- So to satisfy both we add this more refined DState spec, that make sure these post bootstrap tests are always True.
-- The implementation does not test these, so the extra refinement has no effect here, the Spec will test them so refinement does matter there.
-- Note that only the keyhash credentials need be delegated to a DRep.
bootstrapDStateSpec ::
  forall fn era.
  EraSpecTxOut era fn =>
  Set (Credential 'DRepRole) ->
  CertsContext era ->
  Specification fn (DState era)
bootstrapDStateSpec delegatees withdrawals =
  let isKey (ScriptHashObj _) = False
      isKey (KeyHashObj _) = True
      withdrawalPairs = Map.toList (Map.mapKeys raCredential (Map.map coinToWord64 withdrawals))
      withdrawalKeys = Map.keysSet (Map.mapKeys raCredential withdrawals)
      setMapMaybe f = Set.foldr' (\x s -> maybe s (`Set.insert` s) $ f x) mempty
   in constrained $ \ [var| dstate |] ->
        match dstate $ \ [var| rewardMap |] futureGenDelegs genDelegs _rewards ->
          [ assert $ sizeOf_ futureGenDelegs ==. (if hasGenDelegs @era [] then 3 else 0)
          , match genDelegs $ \gd -> assert $ sizeOf_ gd ==. (if hasGenDelegs @era [] then 3 else 0)
          , match _rewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
          , match [var| rewardMap |] $ \ [var| rdMap |] [var| ptrMap |] [var| sPoolMap |] dRepDelegs ->
              [ assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
              , assertExplain (pure "dom ptrMap is empty") $ dom_ ptrMap ==. mempty
              , assertExplain (pure "some rewards (not in withdrawals) are zero") $
                  forAll rdMap $
                    \ [var| keycoinpair |] -> match keycoinpair $ \cred [var| rdpair |] ->
                      -- Apply this only to entries NOT IN the withdrawal set, since withdrawals already set the reward in the RDPair.
                      whenTrue (not_ (member_ cred (lit withdrawalKeys))) (satisfies rdpair someZeros)
              , forAll (lit (Set.filter isKey withdrawalKeys)) $ \cred -> assert $ member_ cred (dom_ dRepDelegs)
              , forAll' dRepDelegs $ \_ dRep ->
                  [ (caseOn dRep)
                      (branch $ \kh -> assert (kh `member_` lit (setMapMaybe credKeyHash delegatees)))
                      (branch $ \sh -> assert (sh `member_` lit (setMapMaybe credScriptHash delegatees)))
                      (branch $ \_ -> assert True)
                      (branch $ \_ -> assert True)
                  ]
              , forAll (lit withdrawalPairs) $ \ [var| pair |] ->
                  match pair $ \ [var| cred |] [var| coin |] ->
                    [ assert $ member_ cred (dom_ rdMap)
                    , (caseOn (lookup_ cred rdMap))
                        -- Nothing
                        ( branch $ \_ -> FalsePred (pure ("credential " ++ show cred ++ " not in rdMap, bootstrapCertStateSpec"))
                        )
                        -- Just
                        ( branch $ \ [var| rdpair |] ->
                            match rdpair $ \rew _deposit -> assert $ rew ==. coin
                        )
                    ]
              ]
          ]

coinToWord64 :: Coin -> Word64
coinToWord64 (Coin n) = fromIntegral n

type CertsContext era = Map RewardAccount Coin

txZero :: EraTx era => Tx era
txZero = mkBasicTx mkBasicTxBody

certsEnvSpec ::
  (EraSpecPParams era, HasSpec fn (Tx era), IsConwayUniv fn) =>
  Specification fn (CertsEnv era)
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
  EraSpecCert era fn =>
  CertsEnv era ->
  CertState era ->
  Specification fn (Seq (TxCert era))
txCertsSpec env state =
  constrained $ \seqs ->
    exists
      (\eval -> pure $ toList (eval seqs))
      (\list -> satisfies (pair_ list seqs) (listSeqCertPairSpec (projectEnv env) state))

noSameKeys :: forall era fn. EraSpecCert era fn => [TxCert era] -> [TxCert era]
noSameKeys [] = []
noSameKeys (x : xs) = x : noSameKeys @era @fn (filter (\y -> txCertKey @era @fn x /= txCertKey @era @fn y) xs)

-- | Specify a pair of List and Seq, where they have essentially the same elements
--   EXCEPT, the Seq has duplicate keys filtered out.
listSeqCertPairSpec ::
  forall era fn.
  EraSpecCert era fn =>
  CertEnv era ->
  CertState era ->
  Specification fn ([TxCert era], Seq (TxCert era))
listSeqCertPairSpec env state =
  constrained' $ \list seqs ->
    [ assert $ sizeOf_ list <=. 5
    , forAll list $ \x -> satisfies x (txCertSpec @era @fn env state)
    , reify list (fromList . noSameKeys @era @fn) (\x -> seqs ==. x)
    ]
