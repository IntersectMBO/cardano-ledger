{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.Conway.NecessaryAndSufficient where

-- hiding (Pred,match,reify)
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (Crypto, HASH, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.UMap
import Constrained hiding (Value)
import Constrained.Base
import Control.State.Transition.Extended (BaseM, STS (Environment, Signal))
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH ()
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Generic.Proof (Proof (..), WitRule (DELEG, GOVCERT, POOL), goSTS)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Args, Fun, forAll)

-- =====================================================

initPParams :: PParams Conway
initPParams =
  def
    & ppEMaxL .~ EpochInterval 20
    & ppMinPoolCostL .~ Coin 10

umapSpec :: (Crypto c, IsConwayUniv fn) => Specification fn (UMap c)
umapSpec =
  constrained $ \um ->
    match um $ \regpairs ptrs staking voting ->
      [ assert $ ptrs ==. lit (Map.empty)
      , assert $ dom_ staking `subset_` dom_ regpairs
      , assert $ dom_ voting `subset_` dom_ regpairs
      ]

pcUMap :: UMap c -> PDoc
pcUMap um =
  ppRecord
    "Umap"
    [
      ( "regKeys"
      , ppMap pcCredential (\x -> ppString $ show (rdRewardCoin x, rdDepositCoin x)) (rdPairMap um)
      )
    , ("stakeDelegated", ppMap pcCredential pcKeyHash (sPoolMap um))
    -- , ("voteDelegated", ppMap pcCredential pcDRep (dRepMap um))
    -- , ("ptrs", ppMap ppPtr pcCredential (ptrMap um))
    ]

-- =====================================================================================
-- EraRule DELEG
-- ======================================================================================
statusTag :: Status -> Int
statusTag (Invert n) = n
statusTag _ = 0

delegCertSpec ::
  forall fn.
  IsConwayUniv fn =>
  Status ->
  PParams Conway ->
  Specification fn (ConwayDelegEnv Conway, DState Conway, ConwayDelegCert StandardCrypto)
delegCertSpec status pp =
  constrained $ \ [var|triple|] ->
    match triple $ \ [var|env|] [var|dstate|] [var|dcert|] ->
      match env $ \ [var|pparams|] [var|poolregs|] ->
        match dstate $ \ [var|umap|] _futgenD _genD _iRewards ->
          match umap $ \ [var|regpairs|] _ptrs [var|delegstake|] _ ->
            [ forAll' regpairs $ \_ rd -> match rd $ \ [var|rew|] _ -> satisfies rew (chooseSpec (1, equalSpec 0) (3, TrueSpec))
            , assert $ pparams ==. (lit pp)
            , (caseOn dcert)
                -- ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
                ( branchW 1 $ \ [var|cred|] [var|mcoin|] ->
                    [ assert $ mcoin ==. lit (SJust (pp ^. ppKeyDepositL))
                    , assertWith 1 status $ not_ (member_ cred (dom_ regpairs))
                    ]
                )
                -- ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
                ( branchW 1 $ \ [var|cred|] [var|mcoin|] ->
                    [ -- You can only unregister things with 0 reward, so the pair (cred, RDPair 0 _) must be in the map
                      assertWith 2 status $ member_ cred $ dom_ regpairs
                    , dependsOn mcoin cred
                    , forAll' regpairs $ \key rdpair ->
                        [ whenTrue
                            (key ==. cred)
                            (satisfies (pair_ mcoin rdpair) pairSpec)
                        ]
                    ]
                )
                -- ConwayDelegCert !(StakeCredential c) !(Delegatee c)
                ( branchW 1 $ \cred delegatee ->
                    [ assert $ not_ (member_ cred (dom_ delegstake))
                    , assertWith 3 status $ member_ cred (dom_ regpairs)
                    , delegateeInPools poolregs delegatee
                    ]
                )
                -- ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
                ( branchW 1 $ \cred delegatee c ->
                    [ assert $ c ==. lit (pp ^. ppKeyDepositL)
                    , assertWith 4 status $ not_ (member_ cred (dom_ regpairs))
                    , delegateeInPools poolregs delegatee
                    ]
                )
            ]

delegName :: ConwayDelegCert c -> String
delegName (ConwayRegCert {}) = "RegCert"
delegName (ConwayUnRegCert {}) = "UnRegCert"
delegName (ConwayDelegCert {}) = "DelegCert"
delegName (ConwayRegDelegCert {}) = "RegDelegCert"

delegateeInPools ::
  (Crypto c, IsConwayUniv fn) =>
  Term fn (Map (KeyHash 'StakePool c) (PoolParams c)) ->
  Term fn (Delegatee c) ->
  Pred fn
delegateeInPools regpools delegatee =
  (caseOn delegatee)
    -- DelegStake !(KeyHash 'StakePool c)
    (branch $ \kh -> member_ kh (dom_ regpools))
    -- DelegVote !(DRep c)
    (branch $ \_ -> True)
    --  DelegStakeVote !(KeyHash 'StakePool c) !(DRep c)
    (branch $ \kh _ -> member_ kh (dom_ regpools))

pairSpec :: IsConwayUniv fn => Specification fn (StrictMaybe Coin, RDPair)
pairSpec = constrained $ \pair ->
  match pair $ \mcoin rdpair ->
    match rdpair $ \reward deposit ->
      [ assert $ reward ==. lit (Coin 0)
      , (caseOn mcoin)
          -- SNothing
          (branch $ \_ -> True)
          -- SJust _
          (branch $ \x -> x ==. deposit)
      ]

-- ==============================================
-- EraRule "POOL"
-- ==============================================

poolRuleSpec ::
  IsConwayUniv fn =>
  Status ->
  PParams Conway ->
  Specification fn (PoolEnv Conway, PState Conway, PoolCert StandardCrypto)
poolRuleSpec status pp = constrained $ \triple ->
  match triple $ \poolenv pstate poolcert ->
    match poolenv $ \slot pparams ->
      [ assert $ pparams ==. lit (pp)
      , (caseOn poolcert)
          -- RegPool :: PoolParams c -> PoolCert c
          ( branchW 1 $ \poolparam ->
              [ match poolparam $ \_ident _vrf _pledge cost _margin rewAccnt _owners _relays mMetadata ->
                  [ assertWith 1 status $ cost >=. lit (pp ^. ppMinPoolCostL)
                  , match rewAccnt $ \net' _ -> [assertWith 2 status $ net' ==. lit Testnet]
                  , onJust' mMetadata $ \metadata ->
                      match metadata $ \_ hash -> [assertWith 3 status $ strLen_ hash <=. lit (maxMetaLen - 1)]
                      -- Aside form these constraints
                      -- registering a poolparam is very flexible. The pool can already be registered.
                      -- It can be schedled for retirement. It can aleady be in the future pool params.
                      -- The only thing that can cause a failure (in Conway Era) is malformed poolparams
                      -- properties. There are no properties relating the Poolparams to the Env or State.
                  ]
              ]
          )
          -- RetirePool :: (KeyHash 'StakePool c) -> EpochNo -> PoolCert c
          ( branchW 1 $ \keyhash epochNo ->
              match pstate $ \curPoolparams _futpoolparams _retiring _pooldeposits ->
                [ curPoolparams `dependsOn` keyhash
                , assertWith 4 status $ member_ keyhash (dom_ curPoolparams)
                , reify
                    slot
                    (epochFromSlotNo)
                    ( \curEpoch ->
                        [ epochNo <=. lit (maxEpochFromPParams pp - 1)
                        , curEpoch <. epochNo
                        ]
                    )
                ]
          )
      ]

maxEpochFromPParams :: EraPParams era => PParams era -> EpochNo
maxEpochFromPParams pp = maxEpochNo
  where
    EpochInterval maxEp = pp ^. ppEMaxL
    maxEpochNo = EpochNo (fromIntegral maxEp)

maxMetaLen :: Int
maxMetaLen = fromIntegral (Hash.sizeHash ([] @(HASH StandardCrypto)))

poolName :: PoolCert c -> String
poolName (RegPool {}) = "RegPool"
poolName (RetirePool {}) = "RetirePool"

-- =====================================================
-- EraRule GOVCERT
-- ==============================================

govRuleSpec ::
  IsConwayUniv fn =>
  Status ->
  Specification
    fn
    ( ConwayGovCertEnv (ConwayEra StandardCrypto)
    , VState (ConwayEra StandardCrypto)
    , ConwayGovCert StandardCrypto
    )
govRuleSpec status =
  let getDeposits ::
        Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto) ->
        [(Credential 'DRepRole StandardCrypto, Coin)]
      getDeposits vs = [(k, drepDeposit dep) | (k, dep) <- Map.toList vs]
      getDRepDeposit :: ConwayEraPParams era => PParams era -> Coin
      getDRepDeposit pp = pp ^. ppDRepDepositL
   in constrained $ \triple ->
        match triple $ \env vstate govcert ->
          match env $ \pp _ ->
            match vstate $ \voteDelegation _comState _dormantEpochs ->
              [ satisfies pp pparamsSpec
              , caseOn
                  govcert
                  -- ConwayRegDRep:: Credential 'DRepRole c -> Coin -> StrictMaybe (Anchor c) -> ConwayGovCert c
                  ( branchW 3 $ \key coin _ ->
                      [ assertWith 1 status $ not_ $ member_ key (dom_ voteDelegation)
                      , reify pp getDRepDeposit (\deposit -> assertWith 2 status $ coin ==. deposit)
                      ]
                  )
                  --  ConwayUnRegDRep:: Credential 'DRepRole c -> Coin -> ConayGovCert
                  ( branchW 6 $ \cred coin ->
                      [ dependsOn voteDelegation (pair_ cred coin)
                      , reify voteDelegation getDeposits $
                          \deposits -> assertWith 3 status $ elem_ (pair_ cred coin) deposits
                      ]
                  )
                  --  ConwayUpdateDRep :: Credential 'DRepRole c -> StrictMaybe (Anchor c) -> ConwayGovCert
                  ( branchW 2 $ \key _ ->
                      assertWith 4 status $ member_ key (dom_ voteDelegation)
                  )
                  -- ConwayAuthCommitteeHotKey:: Credential 'ColdCommitteeRole c -> Credential 'HotCommitteeRole c -> ConwayGovCert
                  (branchW 1 $ \c _ -> assert $ (c ==. c))
                  -- ConwayResignCommitteeColdKey :: Credential 'ColdCommitteeRole c -> StrictMaybe (Anchor c) -> ConwayGovCert
                  (branchW 1 $ \c _ -> assert $ (c ==. c))
              ]

govName :: ConwayGovCert c -> String
govName x = case x of
  ConwayRegDRep {} -> "ConwayRegDRep"
  ConwayUnRegDRep {} -> "ConwayUnRegDRep"
  ConwayUpdateDRep {} -> "ConwayUpdateDRep"
  ConwayAuthCommitteeHotKey {} -> "ConwayAuthCommitteeHotKey"
  ConwayResignCommitteeColdKey {} -> "ConwayResignCommitteeColdKey"

-- =================================================================
-- A test for every Rule

data Status = Apply | Invert Int | Remove deriving (Show)

assertWith :: IsConwayUniv fn => Int -> Status -> Term fn Bool -> Pred fn
assertWith n Apply x = assertExplain ["Status Apply " ++ show n ++ " (" ++ show x ++ ")"] x
assertWith n (Invert m) x =
  if n == m
    then
      assertExplain
        ["Status Invert " ++ show n ++ " (" ++ show x ++ ")"]
        [assert $ not_ x, monitor (\_eval -> expectFailure)]
    else
      assertExplain
        ["Status Nonmatching Invert (" ++ show n ++ " /= " ++ show m ++ " (" ++ show x ++ ")"]
        x
assertWith n Remove x = assertExplain ["Status Remove " ++ show n ++ " (" ++ show x ++ ")"] TruePred

testRule ::
  ( STS (EraRule s a)
  , BaseM (EraRule s a) ~ ShelleyBase
  , HasSpec ConwayFn (Environment (EraRule s a))
  , HasSpec ConwayFn (State (EraRule s a))
  , HasSpec ConwayFn (Signal (EraRule s a))
  ) =>
  WitRule s a ->
  Status ->
  Specification ConwayFn (Environment (EraRule s a), State (EraRule s a), Signal (EraRule s a)) ->
  (Signal (EraRule s a) -> String) ->
  Gen Property
testRule witrule status thespec nameF = do
  triple@(env, state, signal) <- genFromSpec @ConwayFn thespec
  pure $
    monitorSpec
      thespec
      triple
      ( goSTS
          witrule
          env
          state
          signal
          ( \x ->
              case (x, status) of
                (Left _fails, Remove) -> classify True (nameF signal ++ " Fails") $ property True
                (Left fails, Apply) -> counterexample ("Apply " ++ show signal ++ "\n" ++ show fails) (property False)
                (Left _fails, Invert _) -> property False -- The monitor(\ _eval -> expectFailure) in assertWith, should catch this.
                (Right _newdstate, Remove) -> classify True (nameF signal ++ " We got lucky, it randomly succeeds.") $ property True
                (Right _newdstate, _) -> classify True (nameF signal) $ property True
          )
      )

go1, go2, go3 :: Status -> IO ()
go1 status = quickCheck (testRule (DELEG Conway) status (delegCertSpec status initPParams) delegName)
go2 status = quickCheck (testRule (POOL Conway) status (poolRuleSpec status initPParams) poolName)
go3 status = quickCheck (testRule (GOVCERT Conway) status (govRuleSpec @ConwayFn status) govName)

delegProp, poolProp, govProp :: Status -> Gen Property
delegProp status = testRule (DELEG Conway) status (delegCertSpec status initPParams) delegName
poolProp status = testRule (POOL Conway) status (poolRuleSpec status initPParams) poolName
govProp status = testRule (GOVCERT Conway) status (govRuleSpec @ConwayFn status) govName

neccessaryAndSufficientTests :: Spec
neccessaryAndSufficientTests =
  describe "NeccessaryAndSufficient" $ do
    prop "DELEG apply" $ delegProp Apply
    prop "DELEG remove" $ delegProp Remove
    prop "DELEG Invert 1" $ delegProp (Invert 1)
    prop "DELEG Invert 2" $ delegProp (Invert 2)
    prop "DELEG Invert 3" $ delegProp (Invert 3)
    prop "DELEG Invert 4" $ delegProp (Invert 4)

    prop "POOL apply" $ poolProp Apply
    prop "POOL remove" $ poolProp Remove
    prop "POOL Invert 1" $ poolProp (Invert 1)
    prop "POOL Invert 2" $ poolProp (Invert 2)
    prop "POOL Invert 3" $ poolProp (Invert 3)
    prop "POOL Invert 4" $ poolProp (Invert 4)

    prop "GOVCERT apply" $ govProp Apply
    prop "GOVCERT remove" $ govProp Remove
    prop "GOVCERT Invert 1" $ govProp (Invert 1)
    prop "GOVCERT Invert 2" $ govProp (Invert 2)
    prop "GOVCERT Invert 3" $ govProp (Invert 3)

main :: IO ()
main = hspec neccessaryAndSufficientTests
