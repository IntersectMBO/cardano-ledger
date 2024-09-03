{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.UMap (dRepMap)
import Constrained
import Constrained.Base (Pred (..))
import Data.Default.Class
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, fromList)
import qualified Data.Set as Set
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Conway.Cert (txCertSpec)
import Test.Cardano.Ledger.Constrained.Conway.Deleg (someZeros)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.QuickCheck hiding (forAll)

main :: IO ()
main = do
  context <- generate $ genFromSpec @ConwayFn (constrained $ \x -> sizeOf_ x ==. 3)
  state <- generate $ genFromSpec @ConwayFn (bootstrapDStateSpec context)
  putStrLn ("\n\nDRepDelegs\n" ++ show (prettyA (dRepMap (dsUnified state))))
  putStrLn ("\n\nContext\n" ++ show (prettyA context))

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
  IsConwayUniv fn =>
  CertsContext Conway ->
  Specification fn (DState Conway)
bootstrapDStateSpec withdrawals =
  let isKey (ScriptHashObj _) = False
      isKey (KeyHashObj _) = True
      withdrawalPairs = Map.toList (Map.mapKeys snd (Map.map coinToWord64 withdrawals))
      withdrawalKeys = Map.keysSet (Map.mapKeys snd withdrawals)
   in constrained $ \ [var| dstate |] ->
        match dstate $ \ [var| rewardMap |] _futureGenDelegs _genDelegs _rewards ->
          [ assert $ sizeOf_ _futureGenDelegs ==. 0
          , match _genDelegs $ \gd -> assert $ sizeOf_ gd ==. 0
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

type CertsContext era = (Map (Network, Credential 'Staking (EraCrypto era)) Coin)

txZero :: AlonzoTx Conway
txZero = AlonzoTx mkBasicTxBody mempty (IsValid True) def

certsEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (CertsEnv Conway)
certsEnvSpec = constrained $ \ce ->
  match ce $ \tx pp _ _ a b ->
    [ satisfies pp pparamsSpec
    , assert $ tx ==. lit txZero
    , assert $ a ==. lit SNothing
    , assert $ sizeOf_ b ==. 0
    ]

-- | Project a CertEnv out of a CertsEnv (i.e drop the Tx)
projectEnv :: CertsEnv Conway -> CertEnv Conway
projectEnv x =
  CertEnv
    { cePParams = certsPParams x
    , ceSlotNo = certsSlotNo x
    , ceCurrentEpoch = certsCurrentEpoch x
    , ceCurrentCommittee = certsCurrentCommittee x
    , ceCommitteeProposals = certsCommitteeProposals x
    }

-- | Specify a pair of List and Seq, where they have essentially the same elements
--   EXCEPT, the Seq has duplicate keys filtered out.
listSeqPairSpec ::
  IsConwayUniv fn =>
  CertsEnv Conway ->
  CertState Conway ->
  Specification fn ([ConwayTxCert Conway], Seq (ConwayTxCert Conway))
listSeqPairSpec env state =
  constrained' $ \list seqs ->
    [ assert $ sizeOf_ list <=. 5
    , forAll list $ \x -> satisfies x (txCertSpec (projectEnv env) state)
    , reify list (fromList . noSameKeys) (\x -> seqs ==. x)
    ]

txCertsSpec ::
  IsConwayUniv fn =>
  CertsEnv Conway ->
  CertState Conway ->
  Specification fn (Seq (ConwayTxCert Conway))
txCertsSpec env state =
  constrained $ \seqs ->
    exists
      (\eval -> pure $ toList (eval seqs))
      (\list -> satisfies (pair_ list seqs) (listSeqPairSpec env state))

-- | Used to aggregate the key used in registering a Certificate. Different
--   certificates use different kinds of Keys, that allows us to use one
--   type to represent all kinds of keys (Similar to DepositPurpose)
data CertKey c
  = StakeKey !(Credential 'Staking c)
  | PoolKey !(KeyHash 'StakePool c)
  | DRepKey !(Credential 'DRepRole c)
  | ColdKey !(Credential 'ColdCommitteeRole c)
  deriving (Eq, Show, Ord)

-- | Compute the aggregate key type of a Certificater
txCertKey :: ConwayTxCert era -> CertKey (EraCrypto era)
txCertKey (ConwayTxCertDeleg (ConwayRegCert x _)) = StakeKey x
txCertKey (ConwayTxCertDeleg (ConwayUnRegCert x _)) = StakeKey x
txCertKey (ConwayTxCertDeleg (ConwayDelegCert x _)) = StakeKey x
txCertKey (ConwayTxCertDeleg (ConwayRegDelegCert x _ _)) = StakeKey x
txCertKey (ConwayTxCertPool (RegPool x)) = PoolKey (ppId x)
txCertKey (ConwayTxCertPool (RetirePool x _)) = PoolKey x
txCertKey (ConwayTxCertGov (ConwayRegDRep x _ _)) = DRepKey x
txCertKey (ConwayTxCertGov (ConwayUnRegDRep x _)) = DRepKey x
txCertKey (ConwayTxCertGov (ConwayUpdateDRep x _)) = DRepKey x
txCertKey (ConwayTxCertGov (ConwayAuthCommitteeHotKey x _)) = ColdKey x
txCertKey (ConwayTxCertGov (ConwayResignCommitteeColdKey x _)) = ColdKey x

noSameKeys :: [ConwayTxCert era] -> [ConwayTxCert era]
noSameKeys [] = []
noSameKeys (x : xs) = x : noSameKeys (filter (\y -> txCertKey x /= txCertKey y) xs)
